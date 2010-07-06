-- | Changes to alex's grammar:
--
-- * All sets include their unicode equivalents by default
--
-- * @%language MultiParamTypeClasses@ to allow language extensions in code fragments
--
-- * The @%wrapper@ directive doesn\'t make sense any more and so is removed
--
-- * Since it is parsed by TH, some fixities in code fragments may not work
--
-- * TODO: %infixl 4 -, etc. to introduce fixities
--
-- * TODO: allow the end user to supply an %encoding directive to get
--       ascii, latin1, utf-8, ucs-2, or ucs-4 lexers
--
-- * TODO: add a directive to allow "." to match "\n"
--
-- * @\u{...}@ as an alias for @\x{...}@ (char escape)
--
-- * optional braces on @\x{...}@, @\o{...}@ and @\u{....}@
--
-- * Haskell escape codes be used @\STX@ (these do not conflict with @\X@, or @\P@)
--
-- * @\p{....}@ for unicode block, category and posix classes (set escape)
--
-- * @\P{....}@ for the negation of the same (set escape)
--
-- * @\s@, posix space shorthand (set escape)
--
-- * @\w@, posix word shorthand (set escape)
--
-- * @\d@, posix digit shorthand (set escape)
--
-- * @\X@, grapheme recognition to recognize (an entire character with its modifiers)
--       works like a unicode . except that it matches "\n" (regexp escape)
-- 
-- * @\&@ is added as a Haskell-like synonym for () (regexp escape)

module Text.Luthor.Parser
    ( P
    , MacroState
    , initialMacroState
    , scanner 
    ) where

import Data.Char
import Data.CharSet (CharSet)
import qualified Data.CharSet.Posix.Unicode as Posix
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Applicative
import Language.Haskell.Exts.Extension as Haskell
import Language.Haskell.Exts.Parser as Haskell
import Language.Haskell.Meta.Syntax.Translate as Meta

data Code = Code String TH.Exp

data MacroState = MacroState 
    { setMacroDefs :: Map String CharSet
    , regExpMacroDefs :: Map String RegExp 
    , haskellExtensions :: [Extension]
    }

defaultMacroState = MacroState
    { setMacroDefs = 
    , regExpMacroDefs = Map/empty
    , haskellExtensions = [ QuasiQuotes, TemplateHaskell ]
    }

type P = CharParser MacroState

-- | lexical scanner
scanner :: P (Scanner Code String)
scanner = do 
    whitespace
    many directive
    many macroDef
    name <- optional identifier
    operator ":-"
    rules <- many rule
    return $ Scanner name rules



-- directives

directive :: P ()
directive = do
    operator "%" 
    directiveBody
  <?> "directive"

directiveBody :: P ()
directiveBody = languageDirectiveBody
  <?> "directive body"

languageDirectiveBody :: P ()
languageDirectiveBody = do
    try $ identifier "language"
    ext <- languageExtension
    modify $ \s -> s { haskellExtensions = ext : haskellExtensions s }
    
languageExtension :: P Haskell.Extension
languageExtension = do
    st <- getParserState
    ident <- identifier 
    case [ x | (x,"") <- reads ident ] of
        (x:_) -> return x
        [] -> do
            setParserState st
            fail $ "Unknown language extension: " ++ ident
  <?> "language extension"

-- macro definitions
    
macroDef :: P ()
macroDef = setMacroDef <|> regExpMacroDef
  <?> "macro definition"

setMacroDef :: P ()
setMacroDef = do
    m <- setMacroId
    operator '='
    s <- set 
    modify (\st-> st { setMacroDefs = insert m s (setMacroDefs st) } 
    
regExpMacroDef :: P ()
regExpMacroDef = do
    m <- regExpMacro
    operator '='
    r <- regExp
    modify (\st-> st { regExpMacroDefs = insert m r (regExpMacroDefs st) } 

-- set parsing

set :: P CharSet
set = set0 `sepBy` lexeme (char "#")
    <?> "set"

set0 :: P CharSet
set0  = complementedSet0
    <|> bracketedSet -- '[' 
    <|> setMacro     -- $...
    <|> dot          -- .
    <|> (char '\\' >> setEscapeCode) -- \p{...} \P{...}
    <|> rangeSet     -- a \u{1234}

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

setEscape = do
    char '\\'
    setEscapeCode

-- uses look-ahead on '\\'
setEscapeCode :: P CharSet
setEscapeCode = do 
        code <- oneOf "pP"
        st <- getParserState 
        clazz <- ident <|> braces $ noneOf "}" 
            <?> "character class"
        let (careted, cs) = case clazz of
            '^' : rest -> (True, rest)
            rest -> (False, rest)
        case lookupCategoryCharSet cs `mplus` 
             lookupBlockCharSet cs `mplus`
             lookupPosixUnicodeCharSet cs of
            Just c -> return $ if careted `xor` code == 'P' 
                               then complement c
                               else c
            Nothing -> do
                setParserState st
                fail $ "Unknown character class: " ++ clazz
    <|> do char "d"; return Posix.digit
    <|> do char "s"; return Posix.space
    <|> do char "w"; return Posix.word
    <|> do
        ch <- charEscape
        rangeSetK ch
  <?> "set escape code"

dot :: P CharSet
dot = do
    char '.'
    return $ complement (CharSet.singleton '\n')

rangeSet :: P CharSet
rangeSet = do 
    lower <- character
    rangeSet0 lower

rangeSetK :: Char -> P CharSet
rangeSetK lower = do
    r <- optional $ do
             lexeme (char '-')
             character
    case r of 
        Just upper -> return $ range lower upper
        _ -> return $ singleton lower

setMacroId :: P String
setMacroId = do
      char '$' 
      bracedIdent
  <?> "character set macro"

setMacro :: P CharSet
setMacro = do
    st <- getParserState
    macro <- setMacroId
    smacs <- gets setMacroDefs
    case lookup macro smacs of
        Just s -> return s
        _ -> do
            setParserState st
            fail $ "Unknown set macro: " ++ macro

bracketedSet :: P CharSet
bracketedSet = do
    brackets $ do
        caret <- optional (lexeme (char '^'))
        ss <- many set
        let s = mconcat ss
        case caret of
            Just _ -> complement s
            _      -> s

complementedSet0 :: P CharSet
complementedSet0 = do
    lexeme (char '~') -- allow this to be part of a larger operator
    complement <$> set0
    
nonspecialCharSet :: CharSet
nonspecialCharSet = CharSet.delete '%' (graphicCharSet \\ specialCharSet)

-- Rule parsing

startCodes = P [String]
startCodes = angles (startCode `sepBy` comma)
    <?> "start codes"

startCode :: P String
startCode = identifier 
        <|> lexeme (char '0') >> return "0"

tokenDef :: P [Rule Code String]
tokenDef = do cs <- startCodes 
          rule cs
   <|> do t <- token
          rule []

tokens :: P [Token]
tokens = braces (many token) 
     <|> fmap return token

data LeftContext 
    = LeftNone
    | LeftSet CharSet

data Token = Token LeftContext RegExp RightContext Action

token :: P Token
token = do
    l <- optional pre
    b <- regExp
    r <- optional post
    rh <- rhs
    return (Token l b r rh)

alt :: P RegExp
alt = foldr1 (:%%) <$> many1 term

term :: P RegExp
term = do
    re <- regExp0
    reps <- optional rep
    case reps of
        Just f -> f re
        Nothing -> re

regExp :: P RegExp
regExp = alt `sepBy1` lexeme (char '|')

rep :: P (RegExp -> RegExp)
rep = lexeme (char '*') >> return Star
  <|> lexeme (char '+') >> return Plus
  <|> lexeme (char '?') >> return Ques
  <|> braces $ do
        d <- decimal
        opt <- optional (lexeme (char ',') >> optional decimal)
        return $ repeat_rng d opt
    where 
        repeat_rng :: Int -> Maybe (Maybe Int) -> RegExp -> RegExp
        repeat_rng n (Nothing) re = foldr (:%%) Eps (replicate n re)
        repeat_rng n (Just Nothing) re = foldr (:%%) (Star re) (replicate n re)
        repeat_rng n (Just (Just m)) re = intl :%% rst
                where
                    intl = repeat_rng n Nothing re
                    rst = foldr (\re re' -> Ques (re :%% re')) Eps (replicate (m-n) re)

regExp0 :: P RegExp
regExp0 = fromMaybe Eps <$> parens (optional regExp)
    <|> foldr (\x y -> x :%% Ch (singleton y)) Eps <$> stringLiteral 
    <|> regExpMacro
    <|> regExpEscape -- have to try this before set below consumes '\\'
    <|> Ch <$> set

-- thought: this could be expressed as a built-in regexp @X instead
regExpEscape :: P RegExp
regExpEscape = do
    char '\\'
    regExpEscapeCode 

regExpEscapeCode = regExpGraphemeEscapeCode 
             <|> regExpEmptyEscapeCode
             <|> Ch <$> setEscapeCode
    <?> "regexp escape code"
    
regExpGraphemeEscapeCode :: RegExp
regExpGraphemeEscapeCode = do
    lexeme (char 'X')
    return $ Ch (complement mark) :%% Star (Ch mark)

regExpEmptyEscapeCode = do
    lexeme (char '&')
    return Eps

customParseMode :: [Haskell.Extension] -> Haskell.ParseMode
customParseMode exts = Haskell.defaultParseMode
    { parseFileNae = ""
    , extensions = exts
    , ignoreLanguagePragmas = False
    , fixities = baseFixities
    }

data Code = Code String TH.Exp

-- TODO: more gracefully handle nested '{'s
-- TODO: fix haskell-src-exts to give tighter location reporting
code :: P Code 
code = do
    st <- getParserState
    fragment <- braces $ many $ noneOf "}"
    exts <- get haskellExtensions
    result <- Haskell.parseExpWithMode (customParseMode exts) fragment
    case result of
        Haskell.ParseOk a -> return $ Code fragment (Meta.toExp a)
        Haskell.ParseFailed _loc reason -> do
            setParserState st
            fail reason

rule :: [startCodes] -> P (Rule Code startCodes)
rule startCodes = 
    Rule `fmap` optional pre 
           `ap` regExp
           `ap` (fromMaybe NoPost <$> optional post)
           `ap` ruleMaybeCode
 <?> "rule"

pre :: P CharSet
pre = try $ do
    p <- optional set
    lexeme (char '^')
    case p of
        Just x  -> return x
        Nothing -> return newLineSet
    
post :: P (Post Code RegExp)
post 
    = do lexeme (char '$')
         return $ PostRegExp (Ch newLineSet)
  <|> do lexeme (char '/')
         postRegExp <|> postCode

ruleMaybeCode :: P (Maybe Code)
ruleMaybeCode = Just <$> code
  <|> do lexeme (char ';')
         return Nothing

-- | character lexeme

-- Unfortunately, we have to replicate much of this from Text.ParserCombinators.Parsec.Token because the parsers there do not export enough of the interim combinators used

character :: P Char
character = charEscape <|> charLetter 
    <?> "character"

charLetter :: P Char
charLetter = satisfy (\x -> x `member` (graphicCharSet \\ specialCharSet))

charEscape :: P Char
charEscape = char '\\' >> charEscapeCode

charEscapeCode :: P Char
charEscapeCode 
      = charEsc 
    <|> charNum 
    <|> charAscii 
    <|> charControl 
    <|> charQuote
    <?> "character escape code"


charQuote :: P Char
charQuote = satisfy (`CharSet.member` (specialCharSet `union` Category.separator))

charControl :: P Char
charControl = do
    char '^'
    code <- upper
    return $! toEnum (fromEnum code - fromEnum 'A')

charNum :: P Char
charNum = do
    code <- decimal  -- cannot be braced, or it will conflict with \{
        <|> do char 'o';   maybeBraces $ number 8 octDigit 
        <|> do oneOf "xu"; maybeBraces $ number 16 hexDigit 
    return $! toEnum code

foldDigits :: Int -> String -> Int
foldDigits base digits = 
    foldl (\x d -> base * x + toInteger (digitToInt d)) 0

number :: Int -> P Char -> P Int
number base baseDigit = do
    digits <- many1 baseDigit
    return $! foldDigits base digits

charEsc :: P Char
charEsc = choice $ map parseEsc escMap
  where
    parseEsc (c,code) = do 
        char c
        return code
                      
charAscii :: CharParser st Char
charAscii = choice (map parseAscii asciiMap)
    where
        parseAscii (asc,code) = try $ do string asc; return code

escMap :: [(Char,Char)]
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap :: [(String,Char)]
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2) 

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']
    
-- * Utility 'Charset's

newlineCharSet :: CharSet
newlineCharSet = CharSet.singleton '\n'

specialCharSet :: CharSet
specialCharSet = CharSet.fromList ".;,$|*+?#~-{}()[]^/"

spaceCharSet :: CharSet
spaceCharSet = Posix.space

printableCharSet :: CharSet
printableCharSet = Posix.print

graphicCharSet :: CharSet
graphicCharSet = printableCharSet \\ spaceCharSet

-- * "Free" TokenParser parsers

language      = makeTokenParser haskellStyle 
identifier    = T.identifier language
reserved      = T.reserved language
stringLiteral = T.stringLiteral language
natural       = T.natural language
integer       = T.integer language
lexeme        = T.lexeme language
whiteSpace    = T.whiteSpace language
parens        = T.parens language
braces        = T.braces language
angles        = T.angles language

-- * Useful combinators

bracedIdentifier :: P String
bracedIdentifier
      = identifier
    <|> braces identifier
    <?> "braced identifier"


luthor
------

fast unicode-enabled lexers for Haskell, based on Alex, and written in the form of 
quasiquoters to avoid a separate build step.

Example:

    import Text.Luthor

    data Token 
        = Let
        | In
        | Sym Char
        | Var String
        | Int Int
        deriving (Eq,Show)

    tokenLexer :: Lexer Token
    tokenLexer = [$lexer|
        $identStart = [A-Za-z0-9]         -- set macros
        $identBody  = [${identStart} _ ']
        @var = $identStart ${identBody}*  -- regex macros

        tokens :-

        \s+                 ;              -- perl shortcuts
        "--".*              ;
        let                 { \s -> Let }
        in                  { \s -> In }
        \p{Digit}+          { Int . read } -- posix classes
        \p{Symbol}          { Sym . head } -- unicode classes
        @var                { Var } |]

    scanTokens :: String -> [Token]
    scanTokens = scan tokenLexer


-- -----------------------------------------------------------------------------
-- Syntax.hs, part of Luthor
--
-- (c) Edward Kmett 2010
-- Based on AbsSyn.hs from 'alex' (c) Chris Dornan 1995-2000, Simon Marlow 2003
-- ----------------------------------------------------------------------------}

module Text.Luthor.Syntax 
    ( Scanner(..)
    , Rule(..)
    , Code(..)
    , RegExp(..)
    , Post(..)
    , mapPostCode
    , assignStartCodes
    ) where

import Data.CharSet (CharSet)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Luthor.Bifunctor
import Data.Maybe (fromJust)

infixl 4 :|
infixl 5 :%%

data Scanner sc -- ^ start code format
             c  -- ^ code type
    = Scanner
    { scannerName   :: String
    , scannerRules :: [Rule sc c] 
    } deriving (Show)

instance Functor Scanner where
    fmap f (Scanner n rules) = Scanner n (map (fmap f) rules)

instance Bifunctor Scanner where
    bimap f g (Scanner n rules) = Scanner n (map (bimap f g) rules)

type StartCode = Int

data Rule sc -- ^ start code format
          c -- ^ code type
    = Rule 
    { ruleStartCodes :: [sc]
    , rulePre        :: Maybe CharSet
    , ruleRegExp     :: RegExp
    , rulePost       :: Post RegExp c
    , ruleCode       :: Maybe c
    } deriving (Show)

instance Functor Rule where
    fmap f (Rule as pre r post code) = Rule (fmap f as) pre r post (fmap f code)

instance Bifunctor Rule where
    bimap f g (Rule as pre r post code) = 
        Rule (fmap g as) pre r (first f post) (fmap f code)

data Post re -- ^ regexp format
          c -- ^ code type
    = NoPost 
    | Post re
    | PostCode c
    deriving (Show)

instance Functor (Post re) where
    fmap _ NoPost = NoPost
    fmap _ (Post re) = Post re
    fmap f (PostCode c) = PostCode (f c)

instance Bifunctor Post where
    bimap _ _ NoPost = NoPost
    bimap f _ (Post re) = Post (f re)
    bimap _ g (PostCode c) = PostCode (g c)

data RegExp 
    = Eps
    | Ch CharSet
    | RegExp :%% RegExp
    | RegExp :| RegExp
    | Star RegExp
    | Plus RegExp
    | Ques RegExp        
   deriving (Show)

-- | Takes a 'Scanner' with startCodes identified by 'String'
-- and recodes it to one with startCodes @[0 .. n - 1]@,
-- returning the new 'Scanner', the mapping, and @n@.
assignStartCodes :: Scanner String c -> (Scanner Int c, Map String Int, Int)
assignStartCodes scanner = (first startCode scanner, codeMap, numStartCodes)
  where
    startCode name = fromJust $ Map.lookup name codeMap
    next a _ = let a' = a + 1 in a' `seq` (a', a')
    (numStartCodes, codeMap) = 
        second (insert "0" 0) $ 
        Traversable.mapAccumL next 1 $ 
        Map.fromList 
            [ (name, ()) 
            | Rule { ruleStartCodes = names } <- scannerRules scanner
            , name <- names
            , name /= "0" 
            ]

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Luthor.Handler
-- Copyright   :  (c) Edward Kmett 2010,
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An 'Handler' describe actions that must be taken in order to accept the 
-- current state and produce output.
-----------------------------------------------------------------------------

module Text.Luthor.Handler
    ( Handler(..)
    , StateNum
    ) where

import Data.CharSet (CharSet)
import Text.Luthor.Bifunctor

type StateNum = Int

data Handler s -- ^ state label
             c -- ^ code type
    = Handler
    { handlerPriority :: Int             -- ^ lower priorities resolve first
    , handlerPre      :: Maybe CharSet   -- ^ left-hand context
    , handlerPost     :: Post s c        -- ^ right-hand context
    , handlerAction   :: Maybe c         -- ^ what to do 
    } deriving (Show)

instance Functor Handler where
    fmap f (Handler pri pre post mc) = 
         Handler pri pre (first f post) (fmap f mc)

instance Bifunctor Handler where
    bimap f g (Handler pri pre post mc) =
         Handler pri pre (bimap f g post) (fmap g mc)

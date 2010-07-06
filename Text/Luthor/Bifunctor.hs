-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Luthor.Bifunctor
-- Copyright   :  (c) Edward Kmett 2010,
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic definition for bifunctors over the category of Haskell types.
-- Useful for manipulating types with multiple parameters.
--
-----------------------------------------------------------------------------

module Text.Luthor.Bifunctor 
    ( Bifunctor(..) 
    ) where

-- | Minimum definition: either 'bimap' or both 'first' and 'second'
class Bifunctor f where
    bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
    first :: (a -> b) -> f a c -> f b c
    second :: (a -> b) -> f c a -> f c b
    first f = bimap f id
    second = bimap id
    bimap f g = second g . first f

instance Bifunctor (,) where
    bimap f g ~(a,b) = (f a, g b)

instance Bifunctor Either where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right b) = Right (g b)

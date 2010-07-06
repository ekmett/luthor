module Text.Luthor.Silhouette
    ( Silhouette
    , silhouette
    , isDistinguishedBy
    , isEdgeOf
    ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.Monoid

type Silhouette = IntSet

-- the silhouette of a CharSet is formed by the edges of each of its subsets.
-- silhouettes of sets can unioned to obtain, implicitly, the equivalence
-- classes of char ranges between a set of character sets

-- Note that the silhouette of a set and its complement are the same.

silhouette :: CharSet -> Silhouette
silhouette = IntSet.insert 0 . IntSet.fromDistinctAscList . lo . IntSet.toAscList . snd . fromCharSet
  where
    -- we are looking for a char
    lo [] = []
    lo (x:xs)  = fromEnum x : hi x xs

    -- we have seen a lo-hi edge 
    hi x []
      | x == maxBound = []
      | otherwise = [fromEnum (succ x)]
    -- succ x must exist, as there is an element after it
    hi x (y:ys) 
      | x' == y   = hi y ys
      | otherwise = fromEnum x' : hi y ys
        where 
          x' = succ x
{-# INLINE silhouette #-}

isDistinguishedBy :: CharSet -> Silhouette -> Bool
isDistinguishedBy = IntSet.isSubsetOf . silhouette
{-# INLINE isDistinguishedBy #-}

isEdgeOf :: Char -> Silhouette -> Bool
isEdgeOf = IntSet.member . fromEnum
{-# INLINE isEdgeOf #-}

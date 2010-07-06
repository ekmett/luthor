-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Luthor.Sort
-- Copyright   :  (c) Edward Kmett 2010,
--                (c) Chris Dornan 1993-1995
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides properly parameterised merge sort functions,
-- complete with associated functions for inserting and merging.  
-- 
-- The merge sort is based on a Bob Buckley's (Bob Buckley 18-AUG-95) coding 
-- of Knuth's natural merge sort (see Vol. 2).  It seems to be fast in the 
-- average case; it makes use of natural runs in the data becomming
-- linear on ordered data; and it completes in worst time /O(n log(n))/. It is
-- divinely elegant.

-- 'nubBy'' is an /O(n log n)/ version of `nub' and `groupSort' sorts a list into
-- strictly ascending order, using a combining function in its arguments to
-- amalgamate duplicates.

-- Chris Dornan, 14-Aug-93, 17-Nov-94, 29-Dec-95
------------------------------------------------------------------------------}

module Text.Luthor.Sort 
    ( mergeSort
    , nubBy'
    ) where

import Prelude hiding ((<=))

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _    [] = [] -- (foldb f []) is undefined
mergeSort (<=) xs = foldb (merge (<=)) (runs (<=) xs)
{-# INLINE mergeSort #-}

runs :: (a->a->Bool) -> [a] -> [[a]]
runs (<=) xs0 = foldr op [] xs0
      where
	op z xss@(xs@(x:_):xss') 
        | z <= x    = (z:xs):xss'
        | otherwise = [z]:xss
	op z xss        = [z]:xss
{-# INLINE runs #-}

foldb :: (a -> a -> a) -> [a] -> a
foldb _ [x] = x
foldb f xs0 = foldb f (fold xs0)
      where
	fold (x1:x2:xs) = f x1 x2 : fold xs
	fold xs         = xs
{-# INLINE foldb #-}

merge:: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _    [] l = l
merge _    l@(_:_) [] = l
merge (<=) l1@(h1:t1) l2@(h2:t2) 
    | h1 <= h2  = h1 : merge (<=) t1 l2
	| otherwise = h2 : merge (<=) l1 t2

nubBy' :: (a -> a -> Bool) -> [a] -> [a]
nubBy' (<=) l = groupSort (<=) const l
{-# INLINE nubBy' #-}

groupSort:: (a -> a -> Bool) -> (a -> [a] -> b) -> [a] -> [b]
groupSort le cmb l = s_m (mergeSort le l)
	where
	s_m [] = []
	s_m (h:t) = cmb h (takeWhile (`le` h) t) : s_m (dropWhile (`le` h) t)
{-# INLINE groupSort #-}

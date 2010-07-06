-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Luthor.IntTrie
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- fast unsized integer tries, designed to be imported qualified
-----------------------------------------------------------------------------

module Text.Luthor.IntTrie
    ( IntTrie
    , empty
    , null
    , singleton
    , lookup
    , member
    , insert
    , insertWith
    , fromList
    , fromListWith
    , toListR
    , toList
    , lookupPartial
    , lookupPartialInAscendingTrie
    ) where

import Data.Traversable
import Data.Foldable
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Text.Luthor.Bifunctor

-- sadly there isn't a single pass way to insert and check if we inserted when inserting
-- into an IntMap, so we don't bother tracking the size of the structure here

data IntTrie a = IntTrie !(Maybe a) !(IntMap (IntTrie a))

instance Functor IntTrie where
    fmap f (IntTrie mv t) = IntTrie (fmap f mv) (fmap (fmap f) t)
    
instance Foldable IntTrie where
    foldMap f (IntTrie (Just v) t) = f v `mappend` foldMap (foldMap f) t
    foldMap f (IntTrie Nothing t) = foldMap (foldMap f) t 

instance Traversable IntTrie where
    traverse f (IntTrie mv t) = IntTrie <$> traverse f mv <*> traverse (traverse f) t

empty :: IntTrie a
empty = IntTrie Nothing IntMap.empty
{-# INLINE empty #-}

null :: IntTrie a -> Bool
null (IntTrie mv t) = isNothing mv && IntMap.null t
{-# INLINE null #-}

singleton :: [Int] -> a -> IntTrie a
singleton [] a     = IntTrie (Just a) IntMap.empty
singleton (x:xs) a = IntTrie Nothing (IntMap.singleton x (singleton xs a))

lookup :: [Int] -> IntTrie a -> Maybe a
lookup []     (IntTrie _ mv _) = mv
lookup (x:xs) (IntTrie _ _ t) = IntMap.lookup x t >>= lookup xs

member :: [Int] -> IntTrie a -> Bool
member ks t = isJust (lookup ks t)
{-# INLINE member #-}

insertWith :: (a -> a -> a) -> [Int] -> a -> IntTrie a -> IntTrie a
insertWith f [] v (IntTrie Nothing t)   = IntTrie (Just v) t
insertWith f [] v (IntTrie (Just v') t) = IntTrie (Just (f v v')) t
insertWith f (x:xs) v (IntTrie mv t) = 
    IntTrie mv (insertWith (const (IntMap.insertWith f xs v)) x (singleton xs v) t)

insert :: [Int] -> a -> IntTrie a -> IntTrie a
insert [] v (IntTrie Nothing t)   = IntTrie (Just v) t
insert [] v (IntTrie (Just v') t) = IntTrie (Just v') t
insert (x:xs) v (IntTrie mv t) = 
    IntTrie mv (insertWith (const (IntMap.insert xs v)) x (singleton xs v) t)

fromList :: [([Int], a)] -> IntTrie a
fromList = foldr (uncurry insert) empty
{-# INLINE fromList #-}

fromListWith :: (a -> a -> a) -> [([Int]),a)] -> IntTrie a
fromListWith f = foldr (uncurry (insertWith f)) empty
{-# INLINE fromListWith #-}

toListWithReversedKey :: IntTrie a -> [([Int],a)]
toListWithReversedKey = go []
    where
        go ks (IntTrie Nothing t)  = do
            (k, trie) <- IntMap.toList
            go (k:ks) trie
        go ks (IntTrie (Just v) t) = (ks,v) : do
            (k, trie) <- IntMap.toList 
            go (k:ks) trie
{-# INLINE toListWithReversedKey #-}

toList :: IntTrie a -> [([Int],a)]
toList = map (first reverse) . toListR
{-# INLINE toList #-}

-- given one 'Int' of the key, find all members with keys that contain that value.
lookupPartial :: Int -> IntTrie a -> [a]
lookupPartial n (IntTrie _ t) = IntMap.foldWithKey go t []
    where
        go n' (IntTrie mv t)
            | n == n'   = maybe id (:) mv . getEndo (foldMap (\a -> Endo (a:)) t)
            | otherwise = IntMap.foldWithKey go t 
{-# INLINE lookupPartial #-}

-- given one 'Int' of the key, find all members with keys that contain that value.
-- subject to the constraint that all keys in the trie are structured in ascending order

lookupPartialInAscendingTrie :: Int -> IntTrie a -> [a]
lookupPartialInAscendingTrie n (IntTrie _ t) = IntMap.foldWithKey go t []
    where
        go n' (IntTrie mv t) =
            case compare n n' of
                LT -> id
                EQ -> maybe id (:) mv . getEndo (foldMap (\a -> Endo (a:)) t)
                GT -> IntMap.foldWithKey go t 
{-# INLINE lookupPartialInAscendingTrie #-}

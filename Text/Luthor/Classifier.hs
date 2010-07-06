module Text.Luthor.Classifier
    ( Classifier(..)
    , classifier
    , classify
    , classes
    , classSet
    , classMap
    -- * Classifier internals
    , Run
    , runChar
    , runClass
    , packRun

    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Text.Luthor.Silhouette
import Data.Array.Base (Array, numElements, unsafeAt, listArray, elems)

type Run = Word32

runChar :: Run -> Char
runChar i = toEnum (fromEnum (i .&. 0x1fffff))
{-# INLINE runChar #-}

runClass :: Run -> Int
runClass i = fromEnum (i `shiftR` 21)
{-# INLINE runClass #-}

packRun :: Char -> Int -> Start
packRun c i = toEnum (fromEnum c) .|. (toEnum i `shiftL` 21)
{-# INLINE packRun #-}

newtype Classifier = Classifier { classifierArray :: UArray Int Run }
    deriving (Show, Data, Typeable)

-- | we limit ourselves to classifiers that have fewer than 2048 equivalence classes
maxClass :: Int
maxClass = 2048

-- | We use two distinct tries during the construction of a classifier:
--
-- * the trie of the silhouettes to assign unique identifiers to each distinct silhouette
--
-- * the trie of signatures for each exemplar to count the number of distinct fragmented silhouettes
--
-- TODO: This mechanism could be improved to try to find ranges that are common split points
-- and factor them (and all ranges that share their signature) out of the signature calculation
-- increasing the number of equivalence classes by one, and reducing the number of signatures under 
-- consideration by a factor of two at the expense of longer compile times and expending a lot more 
-- brainpower. Optimal selection is quite expensive, but we could do this greedily, and achieve a
-- k-optimal result.

classifierSize :: Classifier -> Int
classifierSize (Classifier arr) = numElements arr
{-# INLINE classifierSize #-}

classifier :: [CharSet] -> Classifier
classifier charSets 
    | exemplarCount > maxClass && signatureCount > maxClass = error "Too many equivalence classes"
    | signatureCount > exemplarCount = classifyIntSet exemplars
    | otherwise                      = classifyIntSet (IntMap.keysSet starts)
    where 
        classifyIntSet :: IntSet a -> Classifier
        classifyIntSet s = 
            Classifier $
            listArray (0, IntSet.size s - 1) $
            zipWith (packStart . toEnum) (IntSet.elems s) [0..]

        -- find all the characters that matter
        -- exemplars :: IntSet
        exemplars     = mconcat $ map silhouette charSets
        exemplarCount = IntSet.size exemplars
        
        -- map each a silhouette to a charset with that silhouette and unique identifier
        -- silhouetteCount :: Int
        -- silhouetteTrie  :: IntTrie (CharSet, Int)
        (silhouetteCount, silhouetteTrie) = 
            Traversable.mapAccumL enumerate 0 $  -- enumerate
            IntTrie.fromList $       -- distinct silhouettes
            [ (IntSet.toAscList (silhouette cs), cs) | cs <- charSets ]

        -- map each character to a unique set of silhouettes
        exemplarSignatures :: IntMap [Int]
        exemplarSignatures = IntMap.fromListWith cons1
            [ (c, [n])
            | c <- IntSet.toList exemplars
            , (cs, n) <- Foldable.toList silhouetteTrie
            , c `CharSet.member` cs ]

        -- signatureCount :: Int
        -- signatureTrie  :: IntTrie ([Int], Int)
        (signatureCount, signatureTrie)
            Traversable.mapAccumL enumerate 0 $ 
            IntTrie.fromListWith cons1 $
            [ (sig, [c]) | (c, sig) <- IntMap.toList exemplarSignatures ]

        -- starts :: IntMap Int -- maps starts to signatures
        starts = IntMap.fromList
            [ (c, signature)
            | (exemplars, signature) <- Foldable.toList signatureTrie
            , c <- exemplars
            ]
{-# INLINE classifier #-}

cons1 :: [Int] -> [Int] -> [Int]
cons1 [x] xs = (x:xs)
cons1 _ _    = error "classifier: logic error"
{-# INLINE cons1 #-}

enumerate :: Int -> a -> (Int, (a, Int))
enumerate !n a = (n + 1, (a, n))
{-# INLINE enumerate #-}

classify :: Char -> Classifier -> Int
classify c (Classifier arr) = go 0 (len - 1)
    where
        len = numElements arr
        go !l !h 
            | l == h          = runClass ms
            | runChar ms <= c = go l m
            | otherwise       = go (m + 1) h
            where 
                -- not concerned about overflow
                m = (l + h) `div` 2
                ms = unsafeAt arr m 
{-# INLINE classify #-}

classes :: Classifier -> [(Char,Char)]
classes (Classifier arr) = go (elems arr)
    where
        go (x:yys@(y:_)) = (x, pred y) : go yys
        go [x] = (x,maxBound)
        go _   = error "classes: empty classifier"
{-# INLINE classes #-}

-- | assumes that the 'CharSet' is distinguished by the 'Classifier'
classSet :: CharSet -> Classifier -> IntSet
classSet cs cls = IntSet.fromList $ go $ IntSet.toList $ silhouette cs
    where
        go [] = []
        go (s:ss) 
            | s `CharSet.member` cs = classify s cls : go ss
            | otherwise             =                  go ss
{-# INLINE classSet #-}

intSetToMap :: IntSet -> e -> IntMap e
intSetToMap is e = 
    IntMap.fromDistinctAscList $ 
    map (\i -> (i,e)) $
    IntSet.toAscList is
{-# INLINE intSetToMap #-}

classMap :: [(CharSet,e)] -> Classifier -> IntMap [e]
classMap cse cls = foldr (IntMap.unionWith cons1) IntMap.empty $ map swizzle cse
    where
        swizzle (cs,e) = intSetToMap (classSet cs cls) e
{-# INLINE classMap #-}

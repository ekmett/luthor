-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Luthor.Graph
-- Copyright   :  (c) Edward Kmett 2010,
--                (c) Chris Dornan 1994-1997
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is a portable version of the ghc-specific `DFS.g.hs', which is
-- itself a straightforward encoding of the Launchbury/King paper on linear graph
-- algorithms.  This module uses balanced binary trees instead of mutable arrays
-- to implement the depth-first search so the complexity of the algorithms is
-- n.log(n) instead of linear.
--
-- The vertices of the graphs manipulated by these modules are labelled with the
-- integers from 0 to n-1 where n is the number of vertices in the graph.
------------------------------------------------------------------------------}

module Text.Luthor.Graph
    ( Graph
    , tabulate
    , transitiveClosure
    , scc
    , successors
    ) where

import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Array ( (!), accumArray, listArray )

-- The result of a depth-first search of a graph is a list of trees,
-- `Forest'.  `post_order' provides a post-order traversal of a forest.

type Forest = [Tree]
data Tree   = Node Int Forest

postOrder :: Forest -> [Int]
postOrder ts = po ts []
	where
	po ts' l = foldr po_tree l ts'

	po_tree (Node a ts') l = po ts' (a:l)

listTree :: Tree -> [Int]
listTree t = l_t t []
	where
	l_t (Node x ts) l = foldr l_t (x:l) ts

-- Graphs are represented by a pair of an integer, giving the number of nodes
-- in the graph, and function mapping each vertex (0..n-1, n=size of graph) to
-- its neighbouring nodes.  `tabulate' takes a size and an edge list and
-- constructs a graph.

type Graph = (Int, Int -> [Int])
type Edge = (Int, Int)

tabulate :: Int -> [Edge] -> Graph
tabulate sz edges = (sz, (ar !))
	where
	ar = accumArray (flip (:)) [] (0, sz-1) [ (v,v') | (v,v') <- edges]

vertices :: Graph -> [Int]
vertices (sz,_) = [0 .. sz - 1]

successors :: Graph -> Int -> [Int]
successors (_,f) = f

edges :: Graph -> [Edge]
edges g = [(v,v') | v <- vertices g, v' <- successors g v]

revEdges :: Graph -> [Edge]
revEdges g = [(v',v) | v <- vertices g, v' <- successors g v]

reverseGraph :: Graph -> Graph
reverseGraph g@(sz,_) = tabulate sz (revEdges g)

-- | `transitiveClosure' takes the transitive closure of a graph
-- graph.  

transitiveClosure :: Graph -> Graph
transitiveClosure g@(sz,_) = (sz, (ar!))
	where
	ar = listArray (0,sz) ([postOrder (dff' [v] g) | v <- vertices g] ++ und)
	und = [error "transitiveClosure"]

-- | strongly connected components of a graph
scc :: Graph -> Forest
scc g = dff' (reverse (topSort (reverseGraph g))) g

-- | topologically sort a graph
topSort :: Graph -> [Int]
topSort = postOrder . dff 

-- `dff' computes the depth-first forest.  It works by unrolling the
-- potentially infinite tree from each of the vertices with `generate_g' and
-- then pruning successors the duplicates.

dff :: Graph -> Forest
dff g = dff' (vertices g) g

dff' :: [Int] -> Graph -> Forest
dff' vs (_bs, f) = prune (map (generate_g f) vs)

generate_g :: (Int -> [Int]) -> Int -> Tree
generate_g f v = Node v (map (generate_g f) (f v))

prune :: Forest -> Forest
prune ts = snd (chop Set.empty ts)

chop :: Set Int -> Forest -> (Set Int, Forest)
chop vstd [] = (vstd, [])
chop vstd (Node v ts:us)
    | v `Set.member` vstd = chop vstd us
	| otherwise = (vstd3, Node v ts' : us')
    where 
        vstd1 = Set.insert v vstd
		(vstd2,ts') = chop vstd1 ts
		(vstd3,us') = chop vstd2 us

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Luthor.NFA
-- Copyright   :  (c) Edward Kmett 2010,
--                (c) Simon Marlow 2003,
--                (c) Chris Dornan 1995-2000
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- See the chapter on `Finite Automata and Lexical Analysis' in the
-- dragon book for an excellent overview of the algorithms in this
-- module.
-----------------------------------------------------------------------------

module Text.Luthor.NFA 
    ( NFA
    , State(..)
    , fromScanner
    ) where

import Control.Monad ( zipWithM, zipWithM_ )

import Data.Array   ( Array, (!), array, listArray, assocs, bounds )
import Data.CharSet ( CharSet )

import Data.IntMap  ( IntMap )
import qualified Data.IntMap as IntMap 

import Text.Luthor.Syntax
import Text.Luthor.Handler
import Text.Luthor.DFS     ( transitiveClosure, successors )

type NFA c = Array Int (State c)

data State c = State 
    { stateHandlers   :: [Handler StateNum c]
    , stateEpsilons   :: [StateNum]
    , stateSuccessors :: [(CharSet, StateNum)]
    } deriving (Show)

-- | Generate an NFA with a start state for each startcode with the same number
-- as that startcode, and epsilon transitions from this state to each
-- of the sub-NFAs for each of the rules acceptable in that startcode.

fromScanner :: Scanner c Int -> Int -> NFA c
fromScanner Scanner { scannerRules = rules } maxStartCode
   = runNFA $ do
          -- make a start state for each start code (these will be
          -- numbered from zero).
          startStates <- sequence $ replicate (maxStartCode + 1) freshState
          -- construct the NFA for each token
          ruleStates <- zipWithM go rules [0..]
          -- make an epsilon edge from each state state to each
          -- token that is acceptable in that state
          zipWithM_ (ruleTransitions (zip rules ruleStates)) 
                [0..maxStartCodes] startStates
        where
          go (Rule _scs pre re post code) priority = do
                b <- freshState
                e <- freshState
                compileRegExp b e re

                post_e <- case post of
                    NoPost -> return NoPost
                    PostCode c -> return (PostCode c)
                    Post re' -> do 
                        r_b <- freshState
                        r_e <- freshState
                        compileRegExp r_b r_e re'
                        accept r_e postHandler
                        return (Post r_b)

                accept e (Handler priority pre post_e code)
                return b

          ruleTransitions rules_with_states start_code start_state = do
                let states = [ s | (Rule scs _ _ _ _, s) <- rules_with_states
                                 , null scs || start_code `elem` map snd scs ]
                mapM_ (epsilonEdge start_state) states

-- -----------------------------------------------------------------------------
-- NFA creation from a regular expression

-- compileRegExp B E R generates an NFA that begins in state B, recognises
-- R, and ends in state E only if R has been recognised. 

compileRegExp :: StateNum -> StateNum -> RegExp -> N c ()
compileRegExp b e Eps    = epsilonEdge b e
compileRegExp b e (Ch p) = charEdge b p e
compileRegExp b e (re1 :%% re2) = do
    s <- freshState
    compileRegExp b s re1
    compileRegExp s e re2
compileRegExp b e (re1 :| re2) = do
    compileRegExp b e re1
    compileRegExp b e re2
compileRegExp b e (Star re) = do
    s <- freshState
    epsilonEdge b s
    compileRegExp s s re
    epsilonEdge s e
compileRegExp b e (Plus re) = do
    s1 <- freshState
    s2 <- freshState
    compileRegExp s1 s2 re
    epsilonEdge b s1
    epsilonEdge s2 s1
    epsilonEdge s2 e
compileRegExp b e (Ques re) = do
    compileRegExp b e re
    epsilonEdge b e

newtype N c a = N { unN :: StateNum -> IntMap (State c) -> (StateNum, IntMap (State c), a) }

instance Monad (N c) where
  return a = N $ \s n -> (s, n, a)
  m >>= k  = N $ \s n -> case unN m s n of
                                 (s', n', a) -> unN (k a) s' n'

runNFA :: N c () -> NFA c
runNFA m = case unN m 0 IntMap.empty of
                (s, nfa_map, ()) -> epsilonClosure (array (0,s-1) (IntMap.toAscList nfa_map))

epsilonClosure:: Array Int (State c) -> NFA c
epsilonClosure ar = listArray bds
                [State accs (successors gr v) outs | (v, State accs _ outs) <-assocs ar]
        where
        gr = transitiveClosure (hi+1, \ v -> stateEpsilons (ar!v))
        bds@(_,hi) = bounds ar

freshState :: N c StateNum
freshState = N $ \s n -> (s+1,n,s)

charEdge :: StateNum -> CharSet -> StateNum -> N c ()
charEdge from charset to = N $ \s n -> (s, addEdge n, ())
 where
   addEdge n =
     case IntMap.lookup from n of
       Nothing -> 
           IntMap.insert from (State [] [] [(charset,to)]) n
       Just (State acc eps trans) ->
           IntMap.insert from (State acc eps ((charset,to):trans)) n

epsilonEdge :: StateNum -> StateNum -> N c ()
epsilonEdge from to 
 | from == to = return ()
 | otherwise  = N $ \s n -> (s, addEdge n, ())
 where
   addEdge n =
     case IntMap.lookup from n of
       Nothing                         -> IntMap.insert from (State [] [to] []) n
       Just (State acc eps trans) -> IntMap.insert from (State acc (to:eps) trans) n

accept :: StateNum -> Handler StateNum c -> N c ()
accept state new_acc = N $ \s n -> (s, addHandler n, ())
 where
   addHandler n = 
     case IntMap.lookup state n of
       Nothing ->
           IntMap.insert state (State [new_acc] [] []) n
       Just (State acc eps trans) ->
           IntMap.insert state (State (new_acc:acc) eps trans) n

postHandler :: Handler StateNum c
postHandler = Acc 0 Nothing Nothing NoPost

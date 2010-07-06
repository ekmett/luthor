-- -----------------------------------------------------------------------------
-- 
-- DFA.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- This module generates a DFA from a scanner by first converting it
-- to an NFA and then converting the NFA with the subset construction.
-- 
-- See the chapter on `Finite Automata and Lexical Analysis' in the
-- dragon book for an excellent overview of the algorithms in this
-- module.
--
-- ----------------------------------------------------------------------------}

module Text.Luthor.DFA 
    ( DFA(..)
    , State(..)
    , StateNum
    , fromScanner
    ) where

import Data.Array ( (!) )
import Data.Maybe ( fromJust )

import Data.CharSet ( CharSet )
import qualified Data.CharSet as CharSet

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Text.Luthor.Syntax
import Text.Luthor.Handler
import Text.Luthor.Sort ( msort, nub' )
import Text.Luthor.NFA (NFA)
import qualified Text.Luthor.NFA as NFA
import Text.Luthor.CharSet (CharSet)
import Text.Luthor.Bifunctor
import Text.Luthor.Classifier
import Text.Luthor.Silhouette

data State s -- ^ state labels
           c -- ^ code type
    = State 
    { stateHandlers   :: [Handler StateNum c] -- ^ state acceptance requirements
    , stateSuccessors :: IntMap s             -- ^ map from equivalence class to successor state
    } deriving (Show)

instance Functor (State s) where
    fmap f (State as ss) = State (fmap (fmap f) as) ss

instance Bifunctor State where
    bimap f g (State as ss) = State (fmap (fmap g) as) (fmap (fmap f) ss)

data DFA c -- ^ code type
    = DFA 
    { dfaClassifier  :: Classifier            -- ^ equivalence classes
    , dfaStartStates :: [Int]                 -- ^ a list of starting states
    , dfaStates      :: IntMap (State Int c)  -- ^ a table of all states
    } deriving (Show)

instance Functor (DFA s) where
    fmap f (DFA ss cs) = DFA ss (fmap (fmap f) cs)

type StateSet = [Int]

data PDFA c
    = PDFA
    { pdfaSilhouette  :: Classifier
    , pdfaStartStates :: [StateSet]
    , pdfaStates      :: Map StateSet (State StateSet c)
    } deriving (Show)

instance Functor (PDFA s) where
    fmap f (PDFA ss cs) = PDFA ss (fmap (fmap f) cs)

fromScanner:: Scanner Int c -> Int -> DFA c
fromScanner scanner scs = fromNFA (NFA.fromScanner scanner scs) scs

fromNFA:: NFC c -> Int -> DFA c
fromNFA nfa scs = mkDFA nfa (addNFA nfa (new_pdfa scs nfa) (dfa_start_states pdfa))

-- | 'addNFA' works by taking the next successorstanding state set to be considered
-- and and ignoring it if the state is already in the partial DFA, otherwise
-- generating all possible transitions from it, adding the new state to the
-- partial DFA and continuing the closure with the extra states.  Note the way
-- it incorporates the trailing context references into the search (by
-- including 'post_ss' in the search).

addNFA :: NFA c -> PDFA c -> [StateSet] -> PDFA c
addNFA _   pdfa [] = pdfa
addNFA nfa pdfa (ss:umkd)
  |  ss `member` pdfa  =  addNFA nfa pdfa  umkd
  |  otherwise         =  addNFA nfa pdfa' umkd'
  where
    pdfa' = insert ss (State handlers (Map.fromList ss_successors)) pdfa
    umkd' = post_sss ++ map snd ss_successors ++ umkd
    -- for each character, the set of states that character would take
    -- us to from the current set of states in the NFA.

    -- TODO: this needs to be seriously reworked!
    ss_successors :: [(Char, StateSet)]
    ss_successors =  [ (ch, mkStateSet nfa ss')
               | ch  <- dfa_alphabet,
                 let ss'  = [ s' | (p,s') <- successors, p ch ],
                 not (null ss')
               ]

    post_sss = [ mkStateSet nfa [s]
               | Handler _ _ _ (Post s) <- handlers ]

    successors :: [(CharSet, StateNum)]
    successors =  [ out | s <- ss, out <- NFA.stateSuccessors (nfa ! s) ]

    handlers = sort_handlers [handler| s<-ss, handler<-nst_handlers (nfa ! s)]

dfa_alphabet:: [Char]
dfa_alphabet = ['\0'..'\255'] -- TODO: [minBound .. maxBound]

-- | 'sort_handlers' sorts a list of handlerept values into decending order of priority,
-- eliminating any elements that follow an unconditional handlerept value.

sort_handlers:: [Handler a] -> [Handler a]
sort_handlers handlers = foldr chk [] (msort le handlers)
        where
        chk handler@(Handler _ _ Nothing NoPost) _   = [handler]
        chk handler                                  rst = handler : rst

        Handler {handlerPrio = n} `le` Handler { handlerPrio = n' } = n <= n'

-- A `PartDFA' is a partially constructed DFA in which the states are
-- represented by sets of states of the original NFA.  It is represented by a
-- triple consisting of the start state of the partial DFA, the NFA from which
-- it is derived and a map from state sets to states of the partial DFA.  The
-- state set for a given list of NFA states is calculated by taking the epsilon
-- closure of all the states, sorting the result with duplicates eliminated.

type StateSet = [StateNum]

new_pdfa:: Int -> NFA c -> PDFA c
new_pdfa starts -- ^ the number of start states
         nfa 
    = PDFA start_ss Map.empty
  where
    start_ss = [ msort (<=) (NFA.stateEpsilons (nfa ! n)) 
               | n <- [0 .. starts - 1] 
               ]

-- constructs the epsilon-closure of a set of NFA states
mkStateSet:: NFA -> [StateNum] -> StateSet
mkStateSet nfa l = nub' (<=) [s' | s <- l, s' <- NFA.stateEpsilons (nfa ! s)]

insert :: StateSet -> State StateSet a -> PDFA a -> PDFA a
insert ss pst (PDFA st mp) = PDFA st (Map.insert ss pst mp)

member :: StateSet -> PDFA a -> Bool
member ss (PDFA _ mp) = ss `Map.member` mp

-- Construct a DFA with numbered states, from a DFA whose states are
-- sets of states from the original NFA.

mkDFA:: NFA a -> PDFA a -> DFA a
mkDFA nfa (PDFA start_states mp)
    = DFA [0 .. length start_states - 1] $
      IntMap.fromList [ (lookup' st, convert pds) | (st, pds) <- Map.toAscList mp]
    where
        mp' = Map.fromList (zip (start_states ++ 
                                (map fst . Map.toAscList) (foldr Map.delete mp start_states)) [0..])

        lookup' = fromJust . flip Map.lookup mp'

        convert :: State StateSet a -> State StateNum a
        convert (State handlers as) = State handlers' as'
            where
                as'   = Map.mapWithKey (\_ch s -> lookup' s) as

                handlers' = map convertHandler handlers
                convertHandler (Handler p pre post a) = Handler p pre post' a
                    where post' =        
                        case post of
                            Post s -> Post (lookup' (mkStateSet nfa [s])) 
                            other -> other


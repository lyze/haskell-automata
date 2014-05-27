{-# OPTIONS -Wall -fwarn-tabs #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}

module FA.DirectState
  ( GenFAState(..)
  , FAStateType
  , StateType(..)
  , NFAState
  , nfaState
  , NFATransitions
  , DFAState
  , dfaState
  , DFATransitions
  , directToIndirectState
  ) where

import Data.Map (Map)
import Data.MultiMap (MultiMap)
import FA.IndirectState (GenFAState')
import qualified FA.IndirectState as Indirect
import FA.Mapping (Mapping)
import qualified FA.Mapping as Mapping
import FA.StateType

data GenFAState n t map a where -- we use a GADT to hide the constraints
  GenFAState :: (Eq n, Eq t, Eq a, Mapping map a) =>
                { stateId :: n
                , stateType :: t
                , transitions :: map a (GenFAState n t map a)
               } -> GenFAState n t map a

directToIndirectState                        :: (Eq a, Mapping map a) =>
                                                GenFAState id t map a
                                             -> GenFAState' id t map a
directToIndirectState (GenFAState n t aToSt) =
  Indirect.GenFAState' n t $ Mapping.map FA.DirectState.stateId aToSt

instance (Eq a, Mapping map a, Show n, Show t, Show a, Show (map a n)) => Show (GenFAState n t map a) where
  show = show . directToIndirectState

instance Eq n => Eq (GenFAState n t map a) where
  st == st' = stateId st == stateId st'

instance Ord n => Ord (GenFAState n t map a) where
  compare st st' = compare (stateId st) $ stateId st'

type NFATransitions = MultiMap
type NFAState a = GenFAState Int StateType NFATransitions a

nfaState :: (Ord a, Eq a) => Int -> StateType -> NFATransitions a (NFAState a) -> NFAState a
nfaState = GenFAState

type DFATransitions = Map
type DFAState a = GenFAState Int StateType DFATransitions a

dfaState :: (Ord a, Eq a) => Int -> StateType -> DFATransitions a (DFAState a) -> DFAState a
dfaState = GenFAState

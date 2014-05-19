{-# OPTIONS -Wall -fwarn-tabs -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, GADTs, FlexibleContexts,
  StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances,
  FunctionalDependencies #-}

module FA.Automaton
  ( GenFA(..)
  , DFA
  , dfa
  , NFA
  , nfa
  , accepts
  , equivs
  , directToIndirectFA
  , directToIndirectStates
  , indirectToDirectStates
  , indirectToDirectFA
  ) where

import Control.Applicative
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import FA.DirectState
import qualified FA.DirectState as DS
import FA.IndirectState
import FA.MapMaybeable (MapMaybeable)
import qualified FA.MapMaybeable as MapMaybeable
import FA.Mapping (Mapping)
import qualified FA.Mapping as Mapping

data GenFA coll s where
  GenFA :: Traversable coll =>
           { states :: coll s, startState :: s } -> GenFA coll s

instance (Show s, Show n, Show (map s n), Show t) =>
    Show (GenFA [] (GenFAState n t map s)) where
  show = show . directToIndirectFA

deriving instance (Show t, Show n, Show (map s n), Show s)
           => Show (GenFA [] (GenFAState' n t map s))

instance (Show k, Show a) => Show (MultiMap k a) where
  show = show . MultiMap.toMap

deriving instance (Traversable coll, Read (coll s), Read s)
             => Read (GenFA coll s)

type DFA a = GenFA [] (DFAState a)

dfa :: [DFAState a] -> DFAState a -> DFA a
dfa = GenFA

type NFA a = GenFA [] (NFAState a)

nfa :: [NFAState a] -> NFAState a -> NFA a
nfa = GenFA

class Automaton m a where
  accepts :: m -> [a] -> Bool

instance Ord a => Automaton (DFA a) a where
  accepts (GenFA _ start) = accepts' start
    where accepts' (GenFAState _ Final _) []     = True
          accepts' (GenFAState _ NonFinal _) []  = False
          accepts' (GenFAState _ _ aToSt) (x:xs) =
            case Map.lookup x aToSt of
              Just st -> accepts' st xs
              Nothing -> False

instance Automaton (NFA String) String where
  accepts (GenFA _ start) = accepts' start
    where accepts' st []  = any (\x -> DS.stateType x == Final) (equivs "" st)
          accepts' st (x:xs) = any (\st' -> accepts' st' xs) (equivs x st)

-- Finds states in an NFA that are equivalent to st with s-transitions
-- used in NFAReduction
equivs :: String -> NFAState String -> [NFAState String]
equivs s st = Set.toList $ snd $ aReachable s [st] Set.empty

aReachable :: String -> [NFAState String] -> Set (NFAState String)
                     -> ([NFAState String], Set (NFAState String))
aReachable _ [] seen = ([], seen)
aReachable "" (st:sts) seen
    | Set.member st seen = aReachable "" sts seen
    | otherwise = aReachable "" (aTrans ++ sts)
                  (Set.union seen $ Set.fromList $ st:aTrans)
    where aTrans = map snd $ filter (\x -> fst x == "") $ MultiMap.toList
                         $ DS.transitions st
aReachable s (st:sts) seen = aReachable s sts (Set.union seen
                                                      $ Set.fromList aTrans)
    where aTrans = map snd $ filter (\x -> fst x == s) $ MultiMap.toList
                         $ DS.transitions st

directToIndirectStates :: (Eq a, Mapping map a, Traversable coll)
                          => coll (GenFAState id t map a)
                              -> coll (GenFAState' id t map a)
directToIndirectStates = fmap go
  where go (GenFAState n t aToSt) = GenFAState' n t
                                    $ Mapping.map FA.DirectState.stateId aToSt

directToIndirectFA :: GenFA coll (GenFAState id t map a)
                   -> GenFA coll (GenFAState' id t map a)
directToIndirectFA (GenFA directStates (GenFAState id0 _ _)) =
  GenFA indirectStates start
    where indirectStates = directToIndirectStates directStates
          Just start     = lookupState id0 indirectStates
          lookupState k = Foldable.find $ \(GenFAState' n _ _) -> k == n

indirectToDirectStates :: forall coll id t map a. (Traversable coll, MapMaybeable map a) => coll (GenFAState' id t map a) -> coll (GenFAState id t map a)
indirectToDirectStates indirectStates = directStates
  where directStates :: coll (GenFAState id t map a)
        directStates = toDirect <$> indirectStates

        toDirect :: GenFAState' id t map a -> GenFAState id t map a
        toDirect (GenFAState' n t aToId) =
          GenFAState n t $ MapMaybeable.mapMaybe (`lookupState` directStates) aToId

        lookupState :: Foldable coll => id -> coll (GenFAState id t map a)
                    -> Maybe (GenFAState id t map a)
        lookupState k = Foldable.find $ \(GenFAState n _ _) -> k == n

indirectToDirectFA :: MapMaybeable map a => GenFA coll (GenFAState' id t map a) -> GenFA coll (GenFAState id t map a)
indirectToDirectFA (GenFA indirectStates (GenFAState' id0 _ _)) =
  GenFA directStates start
    where directStates = indirectToDirectStates indirectStates
          Just start   = lookupState id0 directStates
          lookupState k = Foldable.find $ \(GenFAState n _ _) -> k == n

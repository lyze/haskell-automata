
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, FlexibleContexts,
  StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances #-}

module FA.Automaton
  ( GenFA(..)
  , DFA
  , dfa
  , NFA
  , nfa
  , accepts
  , equivs
  , removeUnreachable
  , directToIndirectFA
  , FA.DirectState.directToIndirectState
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
import qualified FA.DirectState as Direct
import FA.IndirectState
import FA.MapMaybeable (MapMaybeable)
import qualified FA.MapMaybeable as MapMaybeable
import FA.Mapping (Mapping)

instance (Show k, Show a) => Show (MultiMap k a) where -- orphan instance
  show = show . MultiMap.toMap

data GenFA coll s where
  GenFA :: Traversable coll =>
           { states :: coll s, startState :: s } -> GenFA coll s

instance (Show s, Show n, Show (map s n), Show t) =>
    Show (GenFA [] (GenFAState n t map s)) where
  show = show . directToIndirectFA

deriving instance (Show t, Show n, Show (map s n), Show s)
           => Show (GenFA [] (GenFAState' n t map s))

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
    where accepts' st []     = any (\x -> Direct.stateType x == Final)
                               (equivs "" st)
          accepts' st (x:xs) = any (`accepts'` xs) (equivs x st)

-- Finds states in an NFA that are equivalent to st with s-transitions
-- used in NFAReduction
equivs :: String -> NFAState String -> [NFAState String]
equivs s st = Set.toList $ snd $ aReachable s [st] Set.empty

aReachable                  :: String
                            -> [NFAState String]
                            -> Set (NFAState String)
                            -> ([NFAState String], Set (NFAState String))
aReachable _ [] seen        = ([], seen)
aReachable "" (st:sts) seen
    | Set.member st seen = aReachable "" sts seen
    | otherwise = aReachable "" (aTrans ++ sts)
                  (Set.union seen $ Set.fromList $ st:aTrans)
    where aTrans = map snd $ filter (\x -> fst x == "") $ MultiMap.toList
                         $ Direct.transitions st
aReachable s (st:sts) seen = aReachable s sts (Set.union seen
                                                      $ Set.fromList aTrans)
    where aTrans = map snd $ filter (\x -> fst x == s) $ MultiMap.toList
                         $ Direct.transitions st

removeUnreachable              :: Ord a => DFA a -> DFA a
removeUnreachable (GenFA _ q0) = pruneTransitions $ go (Set.singleton q0) [q0]
  where pruneTransitions qs =
          let qs' = Set.toList
                    . Set.map (mapTransitions $ Map.filter (`Set.member` qs))
                    $ qs
          in GenFA qs' q0
        mapTransitions k (GenFAState n t d) = GenFAState n t $ k d
        go r []     = r
        go r (q:qs) =
          let (r', qs') = foldr f (r, qs) . Map.elems $ Direct.transitions q
          in go r' qs'
        f q o@(r, qs)
          | Set.notMember q r = (Set.insert q r, q : qs)
          | otherwise         = o


directToIndirectStates :: (Eq a, Mapping map a, Traversable coll)
                          => coll (GenFAState id t map a)
                              -> coll (GenFAState' id t map a)
directToIndirectStates = fmap directToIndirectState

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

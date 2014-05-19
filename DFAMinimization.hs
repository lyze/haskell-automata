{-# OPTIONS -Wall -fwarn-tabs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DFAMinimization
  ( minimizeDFA
  ) where

import Data.List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA.Automaton
import FA.DirectState

minimizeDFA     :: (Eq a, Ord a) => DFA a -> DFA a
minimizeDFA fa = GenFA { states = mergeEquivStates allStates finalSet allStates
                       , startState = (\(x,_,_) -> x) $ mergeState
                                      (startState fa) allStates finalSet
                                      allStates }
    where allStates = states fa
          initialSet = Set.fromList [Set.fromList [x,y] | x <- allStates,
                                     y <- allStates, oneFinal x y]
          oneFinal :: DFAState a -> DFAState a -> Bool
          oneFinal st st' = case (stateType st, stateType st') of
                              (Final, Final) -> False
                              (NonFinal, NonFinal) -> False
                              _ -> True
          finalSet = repeatUntilUnchanged allStates initialSet

mergeEquivStates :: Ord a => [DFAState a] -> Set (Set (DFAState a))
                 -> [DFAState a] -> [DFAState a]
mergeEquivStates [] _ allStates = allStates
mergeEquivStates (x:xs) set allStates = mergeEquivStates xs set states'
    where (_,_,states') = mergeState x xs set allStates

-- state, list of possible merges, set of inequiv states, (merged state, list of remaining states to check, modified states)
mergeState :: Ord a => DFAState a -> [DFAState a] -> Set (Set (DFAState a))
           -> [DFAState a] -> (DFAState a, [DFAState a], [DFAState a])
mergeState st [] _ allStates = (st,[],allStates)
mergeState st (x:xs) set allStates
    | Set.member (Set.fromList [st,x]) set = mergeState st xs set allStates
    | otherwise = mergeState (removeTrans x st st) xs set $ map
                  (removeTrans x st) (delete x allStates)

-- toremove, equiv state, state to modify, modified state
removeTrans :: Ord a => DFAState a -> DFAState a -> DFAState a -> DFAState a
removeTrans x eq st = GenFAState { stateId = stateId st
                                 , stateType = stateType st
                                 , transitions = Map.map modTrans
                                                 $ transitions st }
    where modTrans y
              | stateId y == stateId x = eq
              | otherwise = y

repeatUntilUnchanged :: (Eq a) => [DFAState a] -> Set (Set (DFAState a))
                     -> Set (Set (DFAState a))
repeatUntilUnchanged l s
    | Set.size newS == Set.size s = s
    | otherwise                   = repeatUntilUnchanged l newS
    where newS = modifyInequivSet l s

modifyInequivSet :: (Eq a) => [DFAState a] -> Set (Set (DFAState a))
                 -> Set (Set (DFAState a))
modifyInequivSet sts curSet = Set.union curSet $ Set.filter
                              (inequivPair curSet) allPossiblePairs
    where allPossiblePairs = Set.fromList [Set.fromList [x,y] | x <- sts,
                                           y <- sts, Set.notMember
                                           (Set.fromList [x,y]) curSet]

inequivPair :: (Eq a) => Set (Set (DFAState a)) -> Set (DFAState a) -> Bool
inequivPair curSet pair = any inequivTrans $ Map.toList $ transitions x
    where x = Set.findMin pair
          y = Set.findMax pair
          inequivTrans (s, st) =
              case find (\z -> fst z == s) (Map.toList $ transitions y) of
                Nothing -> True
                Just (_,st') -> Set.notMember (Set.fromList [st,st']) curSet

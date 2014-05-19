{-# OPTIONS -Wall -fwarn-tabs #-}

module NFAReduction where

import FA.DirectState
import qualified FA.IndirectState as IS
import FA.Automaton
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.MultiMap as MultiMap
import qualified Data.Map as Map
import Data.Map (Map)

reduceNFA   :: NFA String -> DFA String
reduceNFA n = indirectToDirectFA $ GenFA
              { states = map convertState newStates
              , startState = convertState $ (equivs "") $ startState n }
    where newStates = powerlist $ states n
          stateIdMap :: Map (Set Int) Int
          stateIdMap = Map.fromList $ zip
                       (map Set.fromList $ map (map stateId) newStates) [1,2..]
          convertState :: [NFAState String] ->
                          IS.GenFAState' Int StateType DFATransitions String
          convertState list =
              let (sId, sTp, trans) = accumStates list [] NonFinal Map.empty in
              case Map.lookup (Set.fromList sId) stateIdMap of
                Nothing -> error "Unknown set of Ids"
                Just k  -> IS.GenFAState'
                           { IS.stateId = k
                           , IS.stateType = sTp
                           , IS.transitions = Map.filterWithKey
                                              (\key _ -> not (null key))
                                              $ Map.map getId trans }
          getId x = case Map.lookup (Set.fromList x) stateIdMap of
                      Nothing -> error "Unknown transition"
                      Just k' -> k'

powerlist :: [a] -> [[a]]
powerlist []     = [[]]
powerlist (x:xs) = let pow = powerlist xs in
                   pow ++ map (\y -> x:y) pow

accumStates :: [NFAState String] -> [Int] -> StateType -> Map String [Int]
            -> ([Int], StateType, Map String [Int])
accumStates [] a b c               = (a, b, c)
accumStates (x:xs) ids sType trans =
    accumStates xs (setAdd (stateId x) ids) (orType (stateType x) sType) $
                    accumTransitions (MultiMap.toList $ transitions x) trans
    where orType Final _ = Final
          orType _ Final = Final
          orType _ _     = NonFinal

setAdd :: Int -> [Int] -> [Int]
setAdd n' l
    | not (any (==n') l) = n':l
    | otherwise          = l

accumTransitions :: [(String, NFAState String)] -> Map String [Int]
                 -> Map String [Int]
accumTransitions [] curmap          = curmap
accumTransitions ((s, n):xs) curmap =
  accumTransitions xs $ flip (Map.insert s) curmap $
                   case Map.lookup s curmap of
                     Nothing -> [stateId n]
                     Just l  -> setAdd (stateId n) l

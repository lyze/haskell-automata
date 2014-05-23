{-# OPTIONS -Wall -fwarn-tabs #-}

module NFAReduction where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap
import Data.Set (Set)
import qualified Data.Set as Set
import FA.Automaton
import FA.DirectState (StateType(..), NFAState, DFATransitions)
import qualified FA.DirectState as DS
import qualified FA.IndirectState as IS

reduceNFA   :: NFA String -> DFA String
reduceNFA n = indirectToDirectFA $ GenFA
              { states     = map convertState newStates
              , startState = convertState . (equivs "") $ startState n }
    where newStates = powerlist $ states n
          stateIdMap :: Map (Set Int) Int
          stateIdMap = Map.fromList
                       $ zip (map (Set.fromList . map DS.stateId) newStates) [1..]
          convertState     :: [NFAState String]
                           -> IS.GenFAState' Int StateType DFATransitions String
          convertState sts =
            let (sIds, sTp, trans) = processStates (Set.empty, NonFinal, Map.empty) sts
            in case Map.lookup sIds stateIdMap of
              Nothing -> error "NFAReduction.reduceNFA: unknown set of IDs"
              Just k  -> IS.GenFAState'
                         { IS.stateId     = k
                         , IS.stateType   = sTp
                         , IS.transitions = Map.filterWithKey
                                            (const . not . null)
                                            $ Map.map targetId trans } -- some finessing can be done here by using a SetMultiMap instead
          targetId     :: [Int] -> Int
          targetId ids = case Map.lookup (Set.fromList ids) stateIdMap of
                           Nothing -> error "Unknown transition"
                           Just k  -> k

powerlist        :: [a] -> [[a]]
powerlist []     = [[]]
powerlist (x:xs) = xss ++ map (x:) xss
  where xss = powerlist xs

processStates :: (Set Int, StateType, Map String [Int])
              -> [NFAState String]
              -> (Set Int, StateType, Map String [Int])
processStates = foldr f
  where f st (ids, tp, trs) = (newIds, newTp, newTrs)
          where newIds = Set.insert (DS.stateId st) ids
                newTp  = DS.stateType st `orStateType` tp
                newTrs = processTransitions trs $ DS.transitions st

orStateType         :: StateType -> StateType -> StateType
orStateType Final _ = Final
orStateType _ Final = Final
orStateType _ _     = NonFinal

processTransitions :: Map String [Int]
                   -> MultiMap String (NFAState String)
                   -> Map String [Int]
processTransitions = MultiMap.foldrWithKey f
  where f s n trs = flip (Map.insert s) trs $
                    case Map.lookup s trs of
                      Nothing -> [DS.stateId n]
                      Just l  -> setAdd (DS.stateId n) l -- here's where a SetMultiMap would be nice

setAdd      :: Eq a => a -> [a] -> [a]
setAdd x xs
  | not $ any (== x) xs = x : xs
  | otherwise           = xs

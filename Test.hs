{-# OPTIONS -fwarn-tabs -Wall -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Test where

import Control.Applicative
import DFAMinimization
import Data.List
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import FA.Automaton
import FA.DirectState (DFAState)
import qualified FA.DirectState as Direct
import FA.IndirectState (GenFAState'(..))
import qualified FA.IndirectState as Indirect
import FA.Mapping
import FA.StateType
import NFAReduction
import Test.QuickCheck

runTests :: IO ()
runTests = do quickCheck prop_DFAGenOneTransitionPerSymbol
              quickCheck prop_NumStatesNFA
              quickCheck prop_OneTransitionPerSymbolNFA
              quickCheck prop_AcceptTheSameStringsNFA
              quickCheck prop_NumStatesDFA
              quickCheck prop_RemoveUnreachableIsIdempotent
              quickCheck prop_AcceptTheSameStringsAfterRemovingUnreachable
              quickCheck prop_CannotBeReduced
              quickCheck prop_OneTransitionPerSymbolDFA
              quickCheck prop_AcceptTheSameStringsDFA

runTests' :: IO Bool
runTests' = $quickCheckAll

-- DFA generator test
prop_DFAGenOneTransitionPerSymbol :: DFA Char -> Bool
prop_DFAGenOneTransitionPerSymbol fa = all oneTransPerSymbolState $ states fa

instance Arbitrary StateType where
  arbitrary = frequency [(9, return NonFinal), (1, return Final)]

chooseWithoutReplacement :: Eq a => Int -> [a] -> Gen [a]
chooseWithoutReplacement = chooseWithoutReplacement' []
  where chooseWithoutReplacement' acc n as
          | n < 0     = return acc
          | null as   = return acc
          | otherwise = do a <- elements as
                           chooseWithoutReplacement' (a:acc) (n - 1) (delete a as)

deleteFirst   :: (a -> Bool) -> [a] -> [a]
deleteFirst p = go
  where go [] = []
        go (x:xs)
          | p x       = xs
          | otherwise = x : go xs

-- genFAState'Gen           :: (Mapping map a, Eq a, Eq t, Eq n) =>
--                             Gen n
--                          -> Gen t
--                          -> Gen (map a n)
--                          -> Gen (GenFAState' n t map a)
-- genFAState'Gen an at ats = GenFAState' <$> an <*> at <*> ats

ensureOneFinal              :: (Mapping map a, Eq a) =>
                               Int
                            -> Gen (map a Int)
                            -> Gen [GenFAState' Int StateType map a]
                            -> Gen [GenFAState' Int StateType map a]
ensureOneFinal n atrans aqs = do
  qs <- aqs
  if all ((== NonFinal) . Indirect.stateType) qs
    then do
      i <- choose (1, n)
      q <- GenFAState' <$> pure i <*> pure Final <*> atrans
      let qs' = deleteFirst ((== i) . stateId) qs
      return $ qs' ++ [q]
    else return qs

instance Arbitrary (NFA String) where
  arbitrary =
    sized $ \k ->
      let n = max (floor . log . fromIntegral $ k) 1 in
      do let f i = GenFAState' <$> i <*> arbitrary <*> transitionsArb n
         sts <- ensureOneFinal n (transitionsArb n) $ mapM (f . return) [1..n]
         return . indirectToDirectFA . GenFA sts $ head sts
    where transitionsArb n =
            fmap MultiMap.fromList $
                 zip <$> listOf1 arbitrary <*> listOf1 (choose (1, n))

instance Arbitrary (DFA Char) where
  arbitrary =
    sized $ \k ->
      let n = max (fromIntegral . floor . log $ fromIntegral k) 1 in
      do let f i = GenFAState' <$> i <*> arbitrary <*> transitionsArb n
         sts <- ensureOneFinal n (transitionsArb n) $ mapM (f . return) [1..n]
         return . indirectToDirectFA . GenFA sts $ head sts
    where transitionsArb n = do
            n' <- choose (1, n)
            fmap Map.fromList $
                 zip <$> chooseWithoutReplacement n' ['a'..'z']
                       <*> chooseWithoutReplacement n' [1..n]

instance (Enum a, Ord a, Bounded a) => Arbitrary (DFA a) where
  arbitrary = sized $ \k -> let n = max (floor . log . fromIntegral $ k) 1 in
    do let f i = GenFAState' <$> i <*> arbitrary <*> transitionsArb n
       sts <- ensureOneFinal n (transitionsArb n) $ mapM (f . return) [1..n]
       return . indirectToDirectFA . GenFA sts $ head sts
    where transitionsArb n = do
            n' <- choose (1, n)
            fmap Map.fromList $
                 zip <$> chooseWithoutReplacement n' [minBound..maxBound]
                   <*> chooseWithoutReplacement n' [1..n]

-- reduceNFA tests
prop_NumStatesNFA     :: NFA String -> Bool
prop_NumStatesNFA aut =
  2 ^ length (states aut) == (length . states $ reduceNFA aut)

prop_OneTransitionPerSymbolNFA :: NFA String -> Bool
prop_OneTransitionPerSymbolNFA = all oneTransPerSymbolState . states . reduceNFA

oneTransPerSymbolState    :: Eq a => DFAState a -> Bool
oneTransPerSymbolState st = all validTrans trans
  where trans             = Map.toList $ Direct.transitions st
        validTrans (s, _) = 1 == length (filter (\x -> fst x == s) trans)

prop_AcceptTheSameStringsNFA       :: NFA String -> [String] -> Bool
prop_AcceptTheSameStringsNFA aut s =
  accepts aut s == accepts (reduceNFA aut) (filter (not . null) s)

-- removeUnreachable tests
prop_RemoveUnreachableIsIdempotent     :: DFA Char -> Bool
prop_RemoveUnreachableIsIdempotent aut =
  let thePrunedDFA = removeUnreachable aut
      thePrunedTwiceDFA = removeUnreachable thePrunedDFA
  in length (states thePrunedDFA) == length (states thePrunedTwiceDFA)
     && startState thePrunedDFA == startState thePrunedTwiceDFA

prop_AcceptTheSameStringsAfterRemovingUnreachable       :: DFA Char -> String -> Bool
prop_AcceptTheSameStringsAfterRemovingUnreachable aut s =
  accepts aut s == accepts (minimizeDFA aut) s

-- minimizeDFA tests
prop_NumStatesDFA     :: DFA Char -> Bool
prop_NumStatesDFA aut =
  let theDFA = removeUnreachable aut
      theMinimizedDFA = minimizeDFA aut
  in length (states theDFA) >= length (states theMinimizedDFA)

prop_CannotBeReduced     :: DFA Char -> Bool
prop_CannotBeReduced aut =
  let theMinimizedDFA = minimizeDFA aut
      theMinimizedTwiceDFA = minimizeDFA theMinimizedDFA
  in length (states theMinimizedDFA) == length (states theMinimizedTwiceDFA)
     && startState theMinimizedDFA == startState theMinimizedTwiceDFA

prop_OneTransitionPerSymbolDFA :: DFA Char -> Bool
prop_OneTransitionPerSymbolDFA =
  all oneTransPerSymbolState . states . minimizeDFA

prop_AcceptTheSameStringsDFA       :: DFA Char -> String -> Bool
prop_AcceptTheSameStringsDFA aut s =
  accepts aut s == accepts (minimizeDFA aut) s

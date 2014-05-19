{-# OPTIONS -fwarn-tabs -Wall -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
  OverlappingInstances #-}

module Test where

import NFAReduction
import DFAMinimization
import Control.Applicative
import Data.List
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import FA.Automaton
import FA.DirectState (DFAState)
import qualified FA.DirectState as Direct
import qualified FA.IndirectState as Indirect
import FA.StateType
import Test.QuickCheck

runTests :: IO ()
runTests = do quickCheck prop_DFAGenOneTransitionPerSymbol
              quickCheck prop_NumStatesNFA
              quickCheck prop_OneTransitionPerSymbolNFA
              quickCheck prop_AcceptTheSameStringsNFA
              quickCheck prop_NumStatesDFA
              quickCheck prop_CannotBeReduced
              quickCheck prop_OneTransitionPerSymbolDFA
              quickCheck prop_AcceptTheSameStringsDFA

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

instance Arbitrary (DFA Char) where
    arbitrary = sized $ \k -> let n = max (fromIntegral . floor . log $ fromIntegral k) 1 in
       do n' <- choose (1, n)
          let f i = Indirect.GenFAState' <$> i <*> arbitrary <*>
                    (Map.fromList <$>
                     (zip <$> chooseWithoutReplacement n' ['a'..'z']
                            <*> chooseWithoutReplacement n' [1..n]))
          sts <- sequence . map f . map return $ [1..n]
          return $ indirectToDirectFA $ GenFA sts $ head sts


instance Arbitrary (NFA String) where
    arbitrary = sized $ \k -> let n = max (floor . log . fromIntegral $ k) 1 in
       do let f i = Indirect.GenFAState' <$> i <*> arbitrary <*>
                    (MultiMap.fromList <$>
                     (zip <$> listOf1 arbitrary <*> listOf1 (choose (1, n))))
          sts <- sequence . map f . map return $ [1..n]
          return $ indirectToDirectFA $ GenFA sts $ head sts

instance (Enum a, Ord a, Bounded a) => Arbitrary (DFA a) where
  arbitrary = sized $ \k -> let n = max (floor . log . fromIntegral $ k) 1 in
    do n' <- choose (1, n)
       let f i = Indirect.GenFAState' <$> i <*> arbitrary <*>
                 (Map.fromList <$>
                  (zip <$> chooseWithoutReplacement n' [minBound..maxBound]
                         <*> chooseWithoutReplacement n' [1..n]))
       sts <- sequence . map f . map return $ [1..n]
       return $ indirectToDirectFA $ GenFA sts $ head sts


-- reduceNFA tests
prop_NumStatesNFA :: NFA String -> Bool
prop_NumStatesNFA fa = 2^(length $ states fa) == (length $ states $ reduceNFA fa)

prop_OneTransitionPerSymbolNFA :: NFA String -> Bool
prop_OneTransitionPerSymbolNFA = (all oneTransPerSymbolState) . states . reduceNFA

oneTransPerSymbolState :: Eq a => DFAState a -> Bool
oneTransPerSymbolState st = all validTrans trans
    where trans = Map.toList $ Direct.transitions st
          validTrans (s, _) = 1 == (length $ filter (\x -> fst x == s) trans)

prop_AcceptTheSameStringsNFA :: NFA String -> [String] -> Bool
prop_AcceptTheSameStringsNFA fa s = accepts fa s == accepts (reduceNFA fa) (filter (not . null) s)


-- minimizeDFA tests
prop_NumStatesDFA :: DFA Char -> Bool
prop_NumStatesDFA fa = (length $ states fa) >= (length $ states $ minimizeDFA fa)

prop_CannotBeReduced :: DFA Char -> Bool
prop_CannotBeReduced fa = (length $ states $ minimizeDFA fa) == (length $ states $ minimizeDFA $ minimizeDFA fa)

prop_OneTransitionPerSymbolDFA :: DFA Char -> Bool
prop_OneTransitionPerSymbolDFA = (all oneTransPerSymbolState) . states . minimizeDFA

prop_AcceptTheSameStringsDFA :: DFA Char -> String -> Bool
prop_AcceptTheSameStringsDFA fa s = accepts fa s == accepts (minimizeDFA fa) s

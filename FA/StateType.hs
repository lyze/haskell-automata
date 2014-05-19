{-# OPTIONS -Wall -fwarn-tabs #-}

module FA.StateType where

class FAStateType t where
  isFinal :: t -> Bool
  isNonFinal :: t -> Bool

data StateType = Final | NonFinal deriving (Show, Read, Eq)

instance FAStateType StateType where
  isFinal = (Final ==)
  isNonFinal = (NonFinal ==)

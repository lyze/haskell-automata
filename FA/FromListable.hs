{-# OPTIONS -Wall -fwarn-tabs #-}

module FA.FromListable
  ( FromListable(..)
  ) where

class FromListable coll where
  fromList :: [a] -> coll a

instance FromListable [] where
  fromList = id

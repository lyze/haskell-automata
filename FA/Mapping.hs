{-# OPTIONS -Wall -fwarn-tabs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module FA.Mapping
  ( Mapping(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap

class Mapping m k where
  fromList :: [(k, v)] -> m k v
  map :: (a -> b) -> m k a -> m k b

instance Ord k => Mapping Map k where
  fromList = Map.fromList
  map = Map.map

instance Ord k => Mapping MultiMap k where
  fromList = MultiMap.fromList
  map = MultiMap.map

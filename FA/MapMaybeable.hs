{-# OPTIONS -Wall -fwarn-tabs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module FA.MapMaybeable
  ( MapMaybeable(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap

class MapMaybeable m k where
  mapMaybe :: (a -> Maybe b) -> m k a -> m k b

instance Ord k => MapMaybeable Map k where
  mapMaybe = Map.mapMaybe

instance Ord k => MapMaybeable MultiMap k where
  mapMaybe f = mapFromTo $ Map.mapMaybe $ listToMaybeList . Maybe.mapMaybe f
    where mapFromTo   :: (Map k [a] -> Map k [b]) -> MultiMap k a -> MultiMap k b
          mapFromTo g = MultiMap.fromMap . g . MultiMap.toMap
          listToMaybeList [] = Nothing
          listToMaybeList xs = Just xs

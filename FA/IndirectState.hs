{-# OPTIONS -Wall -fwarn-tabs #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
  StandaloneDeriving #-}

module FA.IndirectState
  ( GenFAState'(..)
  ) where

import FA.Mapping

data GenFAState' n t map a where -- we use a GADT to hide the constraints
  GenFAState' :: (Eq n, Eq t, Eq a, Mapping map a) =>
                 { stateId :: n
                 , stateType :: t
                 , transitions :: map a n
                 } -> GenFAState' n t map a

deriving instance (Show n, Show t, Show (map a n), Show a)
           => Show (GenFAState' n t map a)

deriving instance (Read n, Read t, Read a, Eq n, Eq t, Eq a, Mapping map a,
                   Read (map a n))
           => Read (GenFAState' n t map a)

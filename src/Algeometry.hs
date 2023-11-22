{-# LANGUAGE
  DerivingVia
, TypeFamilies
, DataKinds
, StandaloneDeriving
, MultiParamTypeClasses
, FlexibleInstances
, TemplateHaskell #-}

module Algeometry
  ( module Algeometry.GeometricAlgebra
  , module Algeometry.Types
  , Cl (..)
  , VGA2 (..)
  , PGA2 (..)
  , PGA3 (..)
  , PGA4 (..)
  , Outer (..)
  )  where

import Algeometry.GeometricAlgebra
import Algeometry.Types
import GHC.TypeLits

------------------------------------------------------------

type family Cl (p :: Nat) (q :: Nat) (r :: Nat) where
  Cl 0 0 0 = Double
  Cl 0 n 0 = Outer n
  Cl n 0 0 = VGA n
  Cl 2 1 0 = PGA 2
  Cl 3 1 0 = PGA 3
  Cl n 1 0 = PGA n
  Cl p q r = CA p q r

------------------------------------------------------------

newtype VGA2 = VGA2 MapLS
$(tabulateGA "VGA" 2)

newtype PGA2 = PGA2 MapLS
$(tabulateGA "PGA" 2)

newtype PGA3 = PGA3 MapLS
$(tabulateGA "PGA" 3)

newtype PGA4 = PGA4 MapLS
$(tabulateGA "PGA" 4)

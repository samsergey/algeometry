{-# LANGUAGE
  DerivingVia
, TypeFamilies
, DataKinds
, StandaloneDeriving
, MultiParamTypeClasses
, FlexibleInstances
, TemplateHaskell
, FlexibleContexts #-}

module Algeometry
  ( module Algeometry.GeometricAlgebra
  , module Algeometry.Types
  , Cl (..)
  , VGA2 (..)
  , PGA2 (..)
  , PGA3 (..)
  , PGA4 (..)
  , Outer (..)
  ,e0,e1,e2,e3,e4
  ,e01,e02,e03,e04,e12,e13,e14,e23,e24,e34
  ,e012,e013,e014,e023,e024,e034,e123,e124,e134,e234
  ,e1234,e0234,e0134,e0124,e0123
  ,e01234
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

$(defineElements (basis :: [PGA 4]))

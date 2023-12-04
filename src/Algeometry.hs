{-|
Module      : Algeometry 
Description : Reexporting all internal stuff.
Stability   : experimental
-}
{-# LANGUAGE
  DerivingVia
, TypeFamilies
, DataKinds
, StandaloneDeriving
, MultiParamTypeClasses
, FlexibleInstances
, TemplateHaskell
, FlexibleContexts
, UndecidableInstances
, TypeOperators #-}

module Algeometry
  ( -- * Classes

    -- ** Linear space
    LinSpace (..)
  , Basis
  , scale
  , elems
  , coefs
  , terms
  , lerp

  -- ** Clifford algebras
  , CliffAlgebra (..)
  , Generator
  , (∧)
  , (|-)
  , (-|)
  , (∙)
  , (•)

  -- *** Elements
  , e
  , e_
  , scalar
  , kvec
  , avec
  , nvec

  -- *** Parts
  , getGrade
  , weight
  , bulk
  , components

  -- *** Predicates
  , isScalar
  , isHomogeneous
  , isSingular

  -- *** Normalization
  , norm
  , norm2
  , normalize

  -- *** Inversion
  , isInvertible
  , reciprocal
  
  -- ** Geometric algebras
  , GeomAlgebra (..)
  -- *** Elements
  , pseudoScalar
  , basis

  -- *** Predicates
  , isPoint
  , isLine
  , isPlane
  -- *** Geometric tools
  , (∨)
  , meet
  , join
  , projectOn
  , (->|)
  , antiprojectTo
  , (<-|)
  , segmentMeet
  , reflectAt
  , rotateAt
  , angle
  , shiftAlong
  , shiftAlong'
  , rescale
  , stretch  
  
  -- * Types
  , Cl (..)
  , Dual, Outer (..), Outer'
  , VGA (..), VGA', PGA (..), PGA'
  -- ** Effective realisations of geometric algebras
  , VGA2 (..), VGA3 (..), PGA2 (..), PGA3 (..), PGA4 (..)
  , Tabulated (..), TabulatedGA (..)
  , defineElements
  , tabulateGA
  -- ** Basis elements for geometric algebras. 
  , e0, e1, e2, e3, e4
  , e01, e02, e03, e04, e12, e13, e14, e23, e24, e34
  , e012, e013, e014, e023, e024, e034, e123, e124, e134, e234
  , e1234, e0234, e0134, e0124, e0123
  , e01234
  )  where

import Algeometry.GeometricAlgebra
import Algeometry.Types
import GHC.TypeLits

------------------------------------------------------------

-- | Type family for Clifford algebras with given signature.
type family Cl (p :: Nat) (q :: Nat) (r :: Nat) where
  Cl 0 0 0 = Double
  Cl 0 n 0 = Outer n
  Cl n 0 0 = VGA n
  Cl n 1 0 = PGA n
  Cl p q r = CA p q r

------------------------------------------------------------

-- | Tabulated 2D affine geometric algebra.
newtype VGA2 = VGA2 ListLS
$(tabulateGA "VGA'" 2 "VGA2")

-- | Tabulated 3D affine geometric algebra.
newtype VGA3 = VGA3 ListLS
$(tabulateGA "VGA'" 3 "VGA3")

-- | Tabulated 2D projective geometric algebra.
newtype PGA2 = PGA2 ListLS
$(tabulateGA "PGA'" 2 "PGA2")

-- | Tabulated 3D projective geometric algebra.
newtype PGA3 = PGA3 ListLS
$(tabulateGA "PGA'" 3 "PGA3")

-- | Tabulated 4D projective geometric algebra.
newtype PGA4 = PGA4 ListLS
$(tabulateGA "PGA'" 4 "PGA4")

$(defineElements (basis :: [PGA 4]))

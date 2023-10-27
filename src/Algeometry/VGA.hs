{-# LANGUAGE DerivingVia
, StandaloneDeriving
, DataKinds
, KindSignatures
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, FlexibleInstances #-}

module Algeometry.VGA
  ( module Algeometry.GeometricAlgebra
  , VGA (..)) where

import Algeometry.GeometricAlgebra
import GHC.TypeLits (Nat, KnownNat, natVal)

newtype VGA (n :: Nat) = VGA MV
  deriving (Num, Eq, Fractional)

deriving via MV instance LinSpace [Int] (VGA n)
deriving via MV instance GeomAlgebra Int (VGA n)
deriving via MV instance Show (VGA n)

instance KnownNat n => FiniteGeomAlgebra Int (VGA n) where
  generators = res
    where
      res = e <$> [1 .. fromIntegral n]
      n = natVal (head res)
  dim x = fromIntegral (natVal x) - grade x - 1
  toPoint mv = [coeff [k] mv' | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv

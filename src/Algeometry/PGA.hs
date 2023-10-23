{-# LANGUAGE DerivingVia
, StandaloneDeriving
, DataKinds
, KindSignatures
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, FlexibleInstances #-}

module Algeometry.PGA
  ( module Algeometry.GeometricAlgebra
  , PGA (..)) where

import Algeometry.GeometricAlgebra
import GHC.TypeLits (Nat, KnownNat, natVal)

newtype PGA (n :: Nat) = PGA MV
  deriving (Num, Eq, Fractional)

deriving via MV instance LinSpace [Int] (PGA n)
deriving via MV instance GeomAlgebra Int (PGA n)
deriving via MV instance Show (PGA n)

instance KnownNat n => FiniteGeomAlgebra Int (PGA n) where
  basis = res
    where
      res = e <$> [0 .. fromIntegral n]
      n = natVal (head res)
  dim x = fromIntegral (natVal x) - grade x
  point x = dual $ vec (1:x)
  toPoint mv =
    if h == 0 then [] else [coeff [k] mv' / h | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv
      h = coeff [0] mv'

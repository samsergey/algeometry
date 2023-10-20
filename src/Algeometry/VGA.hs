{-# LANGUAGE DataKinds, KindSignatures#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Algeometry.VGA
  ( module Algeometry.GeometricAlgebra
  , VGA (..)) where

import Algeometry.LinSpace
import Algeometry.GeometricAlgebra
import GHC.TypeLits (Nat, KnownNat, natVal)

newtype VGA (n :: Nat) = VGA MV
  deriving (LinSpace, Num, Eq, Fractional)

instance Show (VGA n) where
  show (VGA v) = show v

instance KnownNat n => GeometricAlgebra (VGA n) where
  basis = res
    where
      res = e <$> [1 .. fromIntegral n]
      n = natVal (head res)
  dim x = fromIntegral (natVal x) - grade x - 1
  toPoint mv = [coefficient [k] mv' | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv

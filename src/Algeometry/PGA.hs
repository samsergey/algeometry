{-# LANGUAGE DataKinds,  KindSignatures#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Algeometry.PGA
  ( module Algeometry.GeometricAlgebra
  , PGA (..)) where

import Algeometry.GeometricAlgebra
import GHC.TypeLits (Nat, KnownNat, natVal)

newtype PGA (n :: Nat) = PGA MV
  deriving (LinSpace, Num, Eq, Fractional)

instance Show (PGA n) where
  show (PGA v) = show v

instance KnownNat n => GeometricAlgebra (PGA n) where
  basis = res
    where
      res = e <$> [0 .. fromIntegral n]
      n = natVal (head res)
  dim x = fromIntegral (natVal x) - grade x
  point x = dual $ vec (1:x)
  toPoint mv =
    if h == 0 then [] else [coefficient [k] mv' / h | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv
      h = coefficient [0] mv'

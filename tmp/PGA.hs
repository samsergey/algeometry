{-# LANGUAGE DerivingVia
, StandaloneDeriving
, DataKinds
, KindSignatures
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, FlexibleInstances #-}

module Algeometry.PGA
  ( module Algeometry.GeometricAlgebra
  , PGA (..), PGA2 (..), PGA3 (..)
  ) where

import Algeometry.GeometricAlgebra
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Coerce

------------------------------------------------------------
  
newtype PGA (n :: Nat) = PGA (CA n 1 0)
  deriving (Num, Eq, Fractional)

deriving via CA n 1 0
  instance LinSpace [Int] (PGA n)

deriving via CA n 1 0
  instance KnownNat n => CliffAlgebra Int (PGA n)

deriving via CA n 1 0
  instance KnownNat n => Show (PGA n)

instance KnownNat n => GeomAlgebra Int (PGA n) where
  dim x = fromIntegral (natVal x) - grade x
  point x = dual $ vec (1:x)
  toPoint mv =
    if h == 0 then [] else [coeff [k] mv' / h | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv
      h = coeff [0] mv'

------------------------------------------------------------

newtype PGA2 = PGA2 (CA 2 1 0)
  deriving (Eq, Num, Fractional)

deriving via PGA 2 instance Show PGA2
deriving via PGA 2 instance LinSpace [Int] PGA2
deriving via PGA 2 instance GeomAlgebra Int PGA2
deriving via TabulatedGA PGA2 instance CliffAlgebra Int PGA2

instance Tabulated Int PGA2 where
  signatureT _ = algebraSignature (1 :: PGA 2)
  indexT = mkIndexMap (coerce <$> (basis :: [PGA 2]))
  generatorsT = coerce <$> (generators :: [PGA 2])
  geomT = mkTable2 geom (basis :: [PGA 2])
  outerT = mkTable2 outer (basis :: [PGA 2])
  innerT = mkTable2 inner (basis :: [PGA 2])
  lcontractT = mkTable2 lcontract (basis :: [PGA 2])
  rcontractT = mkTable2 rcontract (basis :: [PGA 2])
  revT = mkTable rev (basis :: [PGA 2])
  invT = mkTable inv (basis :: [PGA 2])
  conjT = mkTable conj (basis :: [PGA 2])
  dualT = mkTable dual (basis :: [PGA 2])

------------------------------------------------------------

newtype PGA3 = PGA3 (CA 3 1 0)
  deriving (Eq, Num, Fractional)

deriving via PGA 3 instance Show PGA3
deriving via PGA 3 instance LinSpace [Int] PGA3
deriving via PGA 3 instance GeomAlgebra Int PGA3
deriving via TabulatedGA PGA3 instance CliffAlgebra Int PGA3

instance Tabulated Int PGA3 where
  signatureT _ = algebraSignature (1 :: PGA 3)
  indexT = mkIndexMap (coerce <$> (basis :: [PGA 3]))
  generatorsT = coerce <$> (generators :: [PGA 3])
  geomT = mkTable2 geom (basis :: [PGA 3])
  outerT = mkTable2 outer (basis :: [PGA 3])
  innerT = mkTable2 inner (basis :: [PGA 3])
  lcontractT = mkTable2 lcontract (basis :: [PGA 3])
  rcontractT = mkTable2 rcontract (basis :: [PGA 3])
  revT = mkTable rev (basis :: [PGA 3])
  invT = mkTable inv (basis :: [PGA 3])
  conjT = mkTable conj (basis :: [PGA 3])
  dualT = mkTable dual (basis :: [PGA 3])


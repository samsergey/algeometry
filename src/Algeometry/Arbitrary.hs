{-|
Module      : Algeometry.Arbitrary
Description : Definitions of Arbritrary instanses for multivectors.
Stability   : experimental
-}
{-# LANGUAGE DerivingVia
, StandaloneDeriving
, FlexibleInstances
, GeneralisedNewtypeDeriving
, MultiParamTypeClasses
, DataKinds
#-}

module Algeometry.Arbitrary
  ( Monom (..)
  , Vector (..)
  , Bivector (..)
  , Trivector (..)
  , Multivector (..)
  , Homogeneous (..)
  ) where
  
import Test.QuickCheck hiding (scale)
import Algeometry
import Data.List (delete)
import Data.Foldable

------------------------------------------------------------

abitraryMV :: (Num a, CliffAlgebra e a) => ([a] -> [a]) -> Gen a
abitraryMV f = res `suchThat` (not . isScalar)
  where
    res = do
      let els = take 5 $ f basis
          coeff = fromInteger <$> choose (-3,3)
      vs <- sublistOf els
      cs <- traverse (const coeff) vs
      return $ sum $ zipWith scale cs vs

shrinkMV :: (Num a, CliffAlgebra e a) => a -> [a]
shrinkMV v | isScalar v = []
           | isMonom v = []
           | otherwise = (v -) <$> terms v

------------------------------------------------------------

type MV = Cl 3 1 3

-- | Wrapper for algebra basis elements.
newtype Monom = Monom MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Monom
deriving via MV instance CliffAlgebra Int Monom

instance Arbitrary Monom where
  arbitrary = elements basis
  shrink mv = e_ . (`delete` ix) <$> ix
    where
      ix = foldMap toList (elems mv)

------------------------------------------------------------

-- | Wrapper for 1-vector.
newtype Vector = Vector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Vector
deriving via MV instance CliffAlgebra Int Vector

instance Arbitrary Vector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 1)
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for 2-vector.
newtype Bivector = Bivector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Bivector
deriving via MV instance CliffAlgebra Int Bivector

instance Arbitrary Bivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 2)
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for 3-vector.
newtype Trivector = Trivector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Trivector
deriving via MV instance CliffAlgebra Int Trivector

instance Arbitrary Trivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 3)
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for a general multivector.
newtype Multivector = Multivector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Multivector
deriving via MV instance CliffAlgebra Int Multivector

instance Arbitrary Multivector where
  arbitrary = abitraryMV id
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for a k-vector.
newtype Homogeneous = Homogeneous MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Homogeneous
deriving via MV instance CliffAlgebra Int Homogeneous

instance Arbitrary Homogeneous where
  arbitrary = do
    k <- choose (1,5)
    abitraryMV $ filter (\x -> grade x == k)
        
  shrink  = shrinkMV


{-# LANGUAGE DerivingVia
, StandaloneDeriving
, FlexibleInstances
, GeneralisedNewtypeDeriving
, MultiParamTypeClasses
#-}

module Algeometry.Arbitrary where
  
import Test.QuickCheck hiding (scale)
import Algeometry.Experiment.GeometricAlgebra
import Data.List (delete)
import Data.Foldable

------------------------------------------------------------

abitraryMV :: (Num a, FiniteGeomAlgebra e a) => ([a] -> [a]) -> Gen a
abitraryMV f = res `suchThat` (not . isScalar)
  where
    res = do
      let els = take 5 $ f algebraElements
          coeff = fromInteger <$> choose (-3,3)
      vs <- sublistOf els
      cs <- traverse (const coeff) vs
      return $ sum $ zipWith scale cs vs

shrinkMV :: (Num a, FiniteGeomAlgebra e a) => a -> [a]
shrinkMV v | isScalar v = []
           | isMonom v = []
           | otherwise = (v -) <$> terms v

------------------------------------------------------------

newtype Monom = Monom MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Monom
deriving via MV instance GeomAlgebra Int Monom
deriving via MV instance FiniteGeomAlgebra Int Monom

instance Arbitrary Monom where
  arbitrary = elements algebraElements
  shrink mv = e_ . (`delete` ix) <$> ix
    where
      ix = foldMap toList (elems mv)

------------------------------------------------------------

newtype Vector = Vector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Vector
deriving via MV instance GeomAlgebra Int Vector
deriving via MV instance FiniteGeomAlgebra Int Vector

instance Arbitrary Vector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 1)
  shrink  = shrinkMV

tst = \(Vector a) (Vector b) -> (a*b == a `inner` b + a ∧ b)

------------------------------------------------------------

newtype Bivector = Bivector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Bivector
deriving via MV instance GeomAlgebra Int Bivector
deriving via MV instance FiniteGeomAlgebra Int Bivector

instance Arbitrary Bivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 2)
  shrink  = shrinkMV

------------------------------------------------------------

newtype Trivector = Trivector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Trivector
deriving via MV instance GeomAlgebra Int Trivector
deriving via MV instance FiniteGeomAlgebra Int Trivector

instance Arbitrary Trivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 3)
  shrink  = shrinkMV

------------------------------------------------------------

newtype Multivector = Multivector MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Multivector
deriving via MV instance GeomAlgebra Int Multivector
deriving via MV instance FiniteGeomAlgebra Int Multivector

instance Arbitrary Multivector where
  arbitrary = abitraryMV id
  shrink  = shrinkMV

------------------------------------------------------------

newtype Homogeneous = Homogeneous MV
  deriving ( Show, Eq, Num, Fractional)

deriving via MV instance LinSpace [Int] Homogeneous
deriving via MV instance GeomAlgebra Int Homogeneous
deriving via MV instance FiniteGeomAlgebra Int Homogeneous

instance Arbitrary Homogeneous where
  arbitrary = do
    k <- choose (1,5)
    abitraryMV $ filter (\x -> grade x == k)
        
  shrink  = shrinkMV


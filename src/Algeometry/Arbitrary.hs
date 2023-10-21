{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Algeometry.Arbitrary where
  
import Test.QuickCheck hiding (scale)
import Algeometry.GeometricAlgebra
import Data.List (delete)
import Data.Foldable

------------------------------------------------------------

abitraryMV :: GeometricAlgebra a => ([a] -> [a]) -> Gen a
abitraryMV f = res `suchThat` (not . isScalar)
  where
    res = do
      let els = take 5 $ f algebraElements
          coeff = fromInteger <$> choose (-3,3)
      vs <- sublistOf els
      cs <- traverse (const coeff) vs
      return $ sum $ zipWith scale cs vs

shrinkMV :: GeometricAlgebra a => a -> [a]
shrinkMV v | isScalar v = []
           | isMonom v && norm2 v == 1 = []
           | isMonom v = [normalize v]
           | otherwise = (v -) <$> terms v

newtype Monom = Monom MV
  deriving ( Show, Eq, Num, Fractional
           , LinSpace, GeometricAlgebra)
  
instance Arbitrary Monom where
  arbitrary = elements algebraElements
  shrink mv = e_ . (`delete` ix) <$> ix
    where
      ix = foldMap toList (blades mv)

------------------------------------------------------------

newtype Vector = Vector MV
  deriving ( Show, Eq, Num, Fractional
           , LinSpace, GeometricAlgebra)

instance Arbitrary Vector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 1)
  shrink  = shrinkMV

------------------------------------------------------------

newtype Bivector = Bivector MV
  deriving ( Show, Eq, Num, Fractional
           , LinSpace, GeometricAlgebra)

instance Arbitrary Bivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 2)
  shrink  = shrinkMV

------------------------------------------------------------

newtype Trivector = Trivector MV
  deriving ( Show, Eq, Num, Fractional
           , LinSpace, GeometricAlgebra)

instance Arbitrary Trivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 3)
  shrink  = shrinkMV

------------------------------------------------------------

newtype Multivector = Multivector MV
  deriving ( Show, Eq, Num, Fractional
           , LinSpace, GeometricAlgebra)

instance Arbitrary Multivector where
  arbitrary = abitraryMV id
  shrink  = shrinkMV

------------------------------------------------------------

newtype Homogeneous = Homogeneous MV
  deriving ( Show, Eq, Num, Fractional
           , LinSpace, GeometricAlgebra)

instance Arbitrary Homogeneous where
  arbitrary = do
    k <- choose (1,5)
    abitraryMV $ filter (\x -> grade x == k)
        
  shrink  = shrinkMV

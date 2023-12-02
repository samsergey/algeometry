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
, TypeFamilies
, UndecidableInstances
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

abitraryMV :: (Num a, CliffAlgebra a) => ([a] -> [a]) -> Gen a
abitraryMV f = res `suchThat` (not . isScalar)
  where
    res = do
      let els = take 5 $ f basis
          coeff = fromInteger <$> choose (-3,3)
      vs <- sublistOf els
      cs <- traverse (const coeff) vs
      return $ sum $ zipWith scale cs vs

shrinkMV :: (Num a, CliffAlgebra a) => a -> [a]
shrinkMV v | isScalar v = []
           | isMonom v = []
           | otherwise = (v -) <$> terms v

------------------------------------------------------------

type MV = Cl 3 1 3

------------------------------------------------------------

-- | Wrapper for algebra basis elements.
newtype Monom = Monom MV
  deriving (Show, Eq, Num, Fractional, LinSpace, CliffAlgebra)
type instance Basis Monom = Basis MV
type instance Generator Monom = Generator MV

instance Arbitrary Monom where
  arbitrary = elements basis
  shrink mv = e_ . (`delete` ix) <$> ix
    where
      ix = foldMap toList (elems mv)

------------------------------------------------------------

-- | Wrapper for 1-vector.
newtype Vector = Vector MV
  deriving (Show, Eq, Num, Fractional, LinSpace, CliffAlgebra)
type instance Basis Vector = Basis MV
type instance Generator Vector = Generator MV

instance Arbitrary Vector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 1)
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for 2-vector.
newtype Bivector = Bivector MV
  deriving (Show, Eq, Num, Fractional, LinSpace, CliffAlgebra)
type instance Basis Bivector = Basis MV
type instance Generator Bivector = Generator MV

instance Arbitrary Bivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 2)
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for 3-vector.
newtype Trivector = Trivector MV
  deriving (Show, Eq, Num, Fractional, LinSpace, CliffAlgebra)
type instance Basis Trivector = Basis MV
type instance Generator Trivector = Generator MV

instance Arbitrary Trivector where
  arbitrary = abitraryMV $ filter (\x -> grade x == 3)
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for a general multivector.
newtype Multivector = Multivector MV
  deriving (Show, Eq, Num, Fractional, LinSpace, CliffAlgebra)
type instance Basis Multivector = Basis MV
type instance Generator Multivector = Generator MV

instance Arbitrary Multivector where
  arbitrary = abitraryMV id
  shrink  = shrinkMV

------------------------------------------------------------

-- | Wrapper for a k-vector.
newtype Homogeneous = Homogeneous MV
  deriving (Show, Eq, Num, Fractional, LinSpace, CliffAlgebra)
type instance Basis Homogeneous = Basis MV
type instance Generator Homogeneous = Generator MV

instance Arbitrary Homogeneous where
  arbitrary = do
    k <- choose (1,5)
    abitraryMV $ filter (\x -> grade x == k)
        
  shrink  = shrinkMV


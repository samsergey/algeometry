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
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.List
import Data.Ord
import Data.Maybe
  
newtype PGA (n :: Nat) = PGA MV
  deriving (Num, Eq, Fractional)

deriving via MV instance LinSpace [Int] (PGA n)
deriving via MV instance GeomAlgebra Int (PGA n)
deriving via MV instance Show (PGA n)

instance KnownNat n => FiniteGeomAlgebra Int (PGA n) where
  generators = res
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

------------------------------------------------------------

table2 :: GeomAlgebra e a => (a -> a -> a) -> [a]
  -> A.Array (Int, Int) (Maybe ([e], Double))
table2 op b = A.listArray ((0,0),(n-1,n-1)) $
  (\x -> listToMaybe . assoc . op x) <$> b <*> b
  where n = length b

table :: GeomAlgebra e a => (a -> a) -> [a]
  -> A.Array Int (Maybe ([e], Double))
table op b = A.listArray (0,n-1) $
  (listToMaybe . assoc . op) <$> b
  where n = length b

lookup1 tbl ix = lmap (\a -> tbl A.! (ix M.! a))
lookup2 tbl ix = lapp (\a b -> tbl A.! (ix M.! a, ix M.! b))

------------------------------------------------------------

newtype PGA2 = PGA2 MV

deriving via MV instance Show PGA2
deriving via MV instance Num PGA2
deriving via MV instance LinSpace [Int] PGA2

instance GeomAlgebra Int PGA2 where
  square _ i = squares_PGA2 A.! i
  grade m
    | isZero m = 0
    | otherwise = length $ maximumBy (comparing length) (elems m)
  geom = lookup2 geom_PGA2 index_PGA2
  outer = lookup2 outer_PGA2 index_PGA2
  inner = lookup2 inner_PGA2 index_PGA2
  lcontract = lookup2 lcontract_PGA2 index_PGA2
  rcontract = lookup2 rcontract_PGA2 index_PGA2
  rev = lookup1 rev_PGA2 index_PGA2
  inv = lookup1 inv_PGA2 index_PGA2
  conj = lookup1 conj_PGA2 index_PGA2

instance FiniteGeomAlgebra Int PGA2 where
  generators = [e 0, e 1, e 2]
  dual = lookup1 dual_PGA2 index_PGA2
  dim x = 2 - grade x
  point x = dual $ vec (1:x)
  toPoint mv =
    if h == 0 then [] else [coeff [k] mv' / h | k <- [1..2] ]
    where
      mv' = dual mv
      h = coeff [0] mv'


els_PGA2 = A.listArray (0,7) $ (basis :: [PGA 2]) >>= elems
index_PGA2 = M.fromList $ zip ((basis :: [PGA 2]) >>= elems) [0..]
squares_PGA2 = A.listArray (0,7) [1.0,0,1.0,1.0,0,0,-1.0,0]
geom_PGA2 = table2 geom (basis :: [PGA 2])
outer_PGA2 = table2 outer (basis :: [PGA 2])
inner_PGA2 = table2 inner (basis :: [PGA 2])
lcontract_PGA2 = table2 lcontract (basis :: [PGA 2])
rcontract_PGA2 = table2 rcontract (basis :: [PGA 2])
rev_PGA2 = table rev (basis :: [PGA 2])
inv_PGA2 = table inv (basis :: [PGA 2])
conj_PGA2 = table conj (basis :: [PGA 2])
dual_PGA2 = table dual (basis :: [PGA 2])

------------------------------------------------------------

newtype PGA3 = PGA3 MV

deriving via MV instance Show PGA3
deriving via MV instance Num PGA3
deriving via MV instance LinSpace [Int] PGA3

instance GeomAlgebra Int PGA3 where
  square _ i = squares_PGA3 A.! i
  grade m
    | isZero m = 0
    | otherwise = length $ maximumBy (comparing length) (elems m)
  geom = lookup2 geom_PGA3 index_PGA3
  outer = lookup2 outer_PGA3 index_PGA3
  inner = lookup2 inner_PGA3 index_PGA3
  lcontract = lookup2 lcontract_PGA3 index_PGA3
  rcontract = lookup2 rcontract_PGA3 index_PGA3
  rev = lookup1 rev_PGA3 index_PGA3
  inv = lookup1 inv_PGA3 index_PGA3
  conj = lookup1 conj_PGA3 index_PGA3

instance FiniteGeomAlgebra Int PGA3 where
  generators = [e 0, e 1, e 2, e 3]
  dual = lookup1 dual_PGA3 index_PGA3
  dim x = 3 - grade x
  point x = dual $ vec (1:x)
  toPoint mv =
    if h == 0 then [] else [coeff [k] mv' / h | k <- [1..3] ]
    where
      mv' = dual mv
      h = coeff [0] mv'

els_PGA3 = A.listArray (0,7) $ (basis :: [PGA 3]) >>= elems
index_PGA3 = M.fromList $ zip ((basis :: [PGA 3]) >>= elems) [0..]
squares_PGA3 = A.listArray (0,7) [1.0,0,1.0,1.0,0,0,-1.0,0]
geom_PGA3 = table2 geom (basis :: [PGA 3])
outer_PGA3 = table2 outer (basis :: [PGA 3])
inner_PGA3 = table2 inner (basis :: [PGA 3])
lcontract_PGA3 = table2 lcontract (basis :: [PGA 3])
rcontract_PGA3 = table2 rcontract (basis :: [PGA 3])
rev_PGA3 = table rev (basis :: [PGA 3])
inv_PGA3 = table inv (basis :: [PGA 3])
conj_PGA3 = table conj (basis :: [PGA 3])
dual_PGA3 = table dual (basis :: [PGA 3])


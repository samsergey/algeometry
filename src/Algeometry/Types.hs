{-# LANGUAGE UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , KindSignatures
  , StandaloneDeriving
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , FunctionalDependencies
  , TypeFamilies, DataKinds #-}

module Algeometry.Types
  ( Re, CA, Cl, Outer, VGA, PGA, PGA2 (..), PGA3 (..)
  , GeometricNum,  Tabulated (..) , TabulatedGA (..)
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import qualified Data.Array as A
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Coerce
import Algeometry.GeometricAlgebra
  
------------------------------------------------------------
-- geometric numbers
------------------------------------------------------------

newtype GeometricNum a = GeometricNum a

deriving instance LinSpace e a => LinSpace e (GeometricNum a)
deriving instance CliffAlgebra b a => CliffAlgebra b (GeometricNum a)

instance CliffAlgebra b a => Eq (GeometricNum a) where
  a == b = isZero (a - b)
  
instance CliffAlgebra b a => Num (GeometricNum a) where
  fromInteger 0 = zero
  fromInteger n = scalar (fromInteger n)
  (+) = add
  (*) = geom
  negate = scale (-1)
  abs = undefined
  signum = undefined

instance CliffAlgebra b a => Fractional (GeometricNum a) where
  fromRational = scalar . fromRational 
  recip  = reciprocal

instance CliffAlgebra Int a => Show (GeometricNum a) where
  show m = if isZero m then "0" else strip $ sscal <> svecs
    where
      trms mv = sortOn (length . fst) $ assoc mv
      strip x = tail $ fromMaybe x $ stripPrefix " +" x
      scal = getGrade 0 m
      sscal = let c = scalarPart scal
              in if c == 0 then "" else ssig c <> show (abs c)
      svecs = do (b, c) <- trms (m - scal)
                 ssig c <> snum c <> showBlade b
      ssig n = if n < 0 then " - " else " + "
      snum n = if abs n == 1 then "" else show (abs n)

      showBlade b = if null b then "1" else 'e':foldMap showIx b
        where
          showIx k | k < 0 = '₋' : index (-k)
                   | otherwise = index k
          index k = ["₀₁₂₃₄₅₆₇₈₉" !! k]
          
------------------------------------------------------------
-- map-based instance
------------------------------------------------------------

instance Ord e => LinSpace e (M.Map e Double) where
  zero = mempty
  isZero m = M.null $ clean m
  monom = M.singleton
  add m1 = clean . M.unionWith (+) m1
  coeff k = fromMaybe 0 . M.lookup k
  assoc = M.toList
  lfilter = M.filterWithKey

  lmap f m = clean $ M.fromListWith (+) $ do
    (x, a) <- M.toList m
    maybeToList (fmap (a *) <$> f x)

  lapp f m1 m2 = clean $ M.fromListWith (+) $ do
    (x, a) <- M.toList m1
    (y, b) <- M.toList m2
    maybeToList (fmap ((a * b) *) <$> f x y)

clean :: M.Map k Double -> M.Map k Double
clean = M.filter (\x -> abs x >= 1e-10)

------------------------------------------------------------

newtype Pos a (p :: Nat) = Pos a
newtype Zero a (q :: Nat) = Zero a
newtype Neg a (r :: Nat) = Neg a

newtype CA (p :: Nat) (q :: Nat) (r :: Nat)
  = CA (Pos (Zero (Neg (M.Map [Int] Double) r) q) p)

instance (KnownNat p, KnownNat q, KnownNat r)
         => CliffAlgebra Int (CA p r q) where
  algebraSignature x = let { CA p = x; Pos z = p; Zero n = z}
           in ( fromIntegral $ natVal p
              , fromIntegral $ natVal z
              , fromIntegral $ natVal n)
  square _ i = fromIntegral (signum i)
  generators = res
    where
      res = (\x -> monom [x] 1) <$> (reverse negs <> zeros <> pos)
      (p,q,r) = algebraSignature (head res)
      pos   = [k  | p > 0, k <- [1..p]]
      zeros = [0  | q > 0]
      negs  = [-k | r > 0, k <- [1..r]]
      
deriving via M.Map [Int] Double instance LinSpace [Int] (CA p q r)

deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Eq (CA p q r)
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Show (CA p q r)
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Num (CA p q r)
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Fractional (CA p q r)

------------------------------------------------------------

newtype Outer (n :: Nat) = Outer (M.Map [Int] Double)

instance KnownNat n => CliffAlgebra Int (Outer n) where
  algebraSignature x = (0, fromIntegral $ natVal x, 0)
  square _ _ = 0
  generators = res
    where
      res = (\x -> monom [x] 1) <$> ix
      ix = [1 .. fromIntegral $ natVal (head res)]
      
deriving via M.Map [Int] Double instance LinSpace [Int] (Outer n)

deriving via GeometricNum (Outer n)
  instance KnownNat n => Eq (Outer n)
deriving via GeometricNum (Outer n)
  instance KnownNat n => Show (Outer n)
deriving via GeometricNum (Outer n)
  instance KnownNat n => Num (Outer n)
deriving via GeometricNum (Outer n)
  instance KnownNat n => Fractional (Outer n)

------------------------------------------------------------

newtype VGA (n :: Nat) = VGA (M.Map [Int] Double)

instance KnownNat n => CliffAlgebra Int (VGA n) where
  algebraSignature x = (fromIntegral $ natVal x, 0, 0)
  square _ _ = 1
  generators = res
    where
      res = (\x -> monom [x] 1) <$> ix
      ix = [1 .. fromIntegral $ natVal (head res)]
      
deriving via M.Map [Int] Double instance LinSpace [Int] (VGA n)

deriving via GeometricNum (VGA n)
  instance KnownNat n => Eq (VGA n)
deriving via GeometricNum (VGA n)
  instance KnownNat n => Show (VGA n)
deriving via GeometricNum (VGA n)
  instance KnownNat n => Num (VGA n)
deriving via GeometricNum (VGA n)
  instance KnownNat n => Fractional (VGA n)

------------------------------------------------------------

newtype Re = Re Double
  deriving (Eq, Num, Fractional)

instance Show Re where
  show (Re x) = show x

instance LinSpace [Int] Re where
  zero = 0
  isZero = (== 0)
  monom _ = Re
  add = (+)
  assoc (Re x) = [([],x)]
  lmap = undefined
  lapp = undefined
  lfilter = undefined

instance CliffAlgebra Int Re where
  algebraSignature _ = (0,0,0)
  square _ _ = 1
  grade _ = 0
  generators = []
  geom = (*)
  outer = (*)
  inner = (*)
  lcontract = (*)
  rcontract = (*)
  rev = id
  inv = id
  conj = id
  dual = id

------------------------------------------------------------
-- tabulated instance
------------------------------------------------------------

newtype Table e a i = Table (A.Array i (Maybe ([e], Double)))
newtype IndexMap e a = IndexMap (M.Map e Int)

class Tabulated e a | a -> e where
  signatureT  :: a -> (Int,Int,Int)
  generatorsT :: [a]
  indexT      :: IndexMap [e] a
  geomT       :: Table e a (Int,Int)
  outerT      :: Table e a (Int,Int)
  innerT      :: Table e a (Int,Int)
  lcontractT  :: Table e a (Int,Int)
  rcontractT  :: Table e a (Int,Int)
  revT        :: Table e a Int
  invT        :: Table e a Int
  conjT       :: Table e a Int
  dualT       :: Table e a Int

mkTable2 :: CliffAlgebra e a
         => (a -> a -> a) -> [a] -> Table e b (Int, Int)
mkTable2 op b = mkArray $ f <$> b <*> b
  where
    mkArray = Table . A.listArray ((0,0),(n-1,n-1))
    f x = listToMaybe . assoc . op x
    n = length b

mkTable :: CliffAlgebra e a
        => (a -> a) -> [a] -> Table e b Int
mkTable op b = mkArray $ f <$> b
  where
    mkArray = Table . A.listArray (0,n-1)
    f = listToMaybe . assoc . op
    n = length b

mkIndexMap :: (Ord e, LinSpace [e] a) => [a] -> IndexMap [e] a
mkIndexMap b = IndexMap $ M.fromList $ zip (b >>= elems) [0..]

lookup1 :: CliffAlgebra e a
        => Table e a Int -> IndexMap [e] a -> a -> a
lookup1 (Table tbl) (IndexMap ix) =
  lmap (\x -> tbl A.! (ix M.! x))

lookup2 :: CliffAlgebra e a
        => Table e a (Int,Int) -> IndexMap [e] a -> a -> a -> a
lookup2 (Table tbl) (IndexMap ix) =
  lapp (\a b -> tbl A.! (ix M.! a, ix M.! b))

newtype TabulatedGA a = TabulatedGA a
  deriving Eq

deriving instance LinSpace e a => LinSpace e (TabulatedGA a)
 
instance (CliffAlgebra e a, Tabulated e a ) =>
         CliffAlgebra e (TabulatedGA a) where
  algebraSignature (TabulatedGA a) = signatureT a
  square     = undefined
  generators = TabulatedGA <$> generatorsT
  geom       = tab2 $ lookup2 geomT indexT
  outer      = tab2 $ lookup2 outerT indexT
  inner      = tab2 $ lookup2 innerT indexT
  lcontract  = tab2 $ lookup2 lcontractT indexT
  rcontract  = tab2 $ lookup2 rcontractT indexT
  rev        = tab  $ lookup1 revT indexT
  inv        = tab  $ lookup1 invT indexT
  conj       = tab  $ lookup1 conjT indexT
  dual       = tab  $ lookup1 dualT indexT

tab :: (a -> a) -> TabulatedGA a -> TabulatedGA a
tab f (TabulatedGA a) = TabulatedGA $ f a

tab2 :: (a -> a -> a)
     -> TabulatedGA a -> TabulatedGA a -> TabulatedGA a
tab2 f (TabulatedGA a) (TabulatedGA b) = TabulatedGA $ f a b

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

------------------------------------------------------------

type family Cl (p :: Nat) (q :: Nat) (r :: Nat) where
  Cl 0 0 0 = Re
  Cl 0 n 0 = Outer n
  Cl n 0 0 = VGA n
  Cl 2 1 0 = PGA2
  Cl 3 1 0 = PGA3
  Cl n 1 0 = PGA n
  Cl p q r = CA p q r
  

{-|
Module      : Algeometry.Types
Description : Definitions for exact geometric algebras.
Stability   : experimental
-}
{-# LANGUAGE UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , KindSignatures
  , StandaloneDeriving
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , FunctionalDependencies
  , TypeFamilies, DataKinds
  , TemplateHaskell #-}

module Algeometry.Types
  ( Pos(..), Neg (..), Zero (..)
  , CA (..)
  , Outer (..)
  , VGA (..)
  , PGA (..)
  , GeometricNum
  , Tabulated (..)
  , TabulatedGA (..)
  , MapLS
  , defineElements
  , tabulateGA
  )
where

import Algeometry.GeometricAlgebra
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.Maybe
import Data.List
import GHC.TypeLits
import Data.Coerce
import Language.Haskell.TH
import Language.Haskell.TH.Lib.Internal (Decs)

 
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
  abs = error "GeometricNum: abs is undefined!"
  signum 0 = 0
  signum x = scalar (signum (head (reverse (coefs x))))

instance CliffAlgebra b a => Fractional (GeometricNum a) where
  fromRational = scalar . fromRational 
  recip  = reciprocal

instance CliffAlgebra Int a => Show (GeometricNum a) where
  show m = if isZero m then "0" else strip $ sscal <> svecs
    where
      trms mv = sortOn (length . fst) $ assoc mv
      strip x = tail $ fromMaybe x $ stripPrefix " +" x
      scal = getGrade 0 m
      sscal = let c = trace scal
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
-- map-based linear space
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

-- | Representation of linear space as a map, indexed by integer indexes.
type MapLS = M.Map [Int] Double

-- | Outer (Grassmann) algebra of given dimention.
newtype Outer (n :: Nat) = Outer MapLS

instance KnownNat n => CliffAlgebra Int (Outer n) where
  algebraSignature x = (0, fromIntegral $ natVal x, 0)
  square _ _ = 0
  generators = res
    where
      res = (\x -> monom [x] 1) <$> ix
      ix = [1 .. fromIntegral $ natVal (head res)]
      
deriving via MapLS instance LinSpace [Int] (Outer n)

deriving via GeometricNum (Outer n)
  instance KnownNat n => Eq (Outer n)
deriving via GeometricNum (Outer n)
  instance KnownNat n => Show (Outer n)
deriving via GeometricNum (Outer n)
  instance KnownNat n => Num (Outer n)
deriving via GeometricNum (Outer n)
  instance KnownNat n => Fractional (Outer n)

------------------------------------------------------------

-- | Affine vector geometric algebra of given dimention.
newtype VGA (n :: Nat) = VGA (CA n 0 0)
  deriving (Num, Eq, Fractional)

deriving via CA n 0 0 instance LinSpace [Int] (VGA n)
deriving via CA n 0 0 instance KnownNat n => CliffAlgebra Int (VGA n)
deriving via CA n 0 0 instance KnownNat n => Show (VGA n)

instance KnownNat n => GeomAlgebra Int (VGA n) where
  dim x = fromIntegral (natVal x) - grade x - 1
  fromXY = avec 1
  toXY mv = [coeff [k] mv' | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv
      
------------------------------------------------------------
  
-- | Projective geometric algebra of given dimention.
newtype PGA (n :: Nat) = PGA (CA n 1 0)
  deriving (Num, Eq, Fractional)

deriving via CA n 1 0 instance LinSpace [Int] (PGA n)
deriving via CA n 1 0 instance KnownNat n => CliffAlgebra Int (PGA n)
deriving via CA n 1 0 instance KnownNat n => Show (PGA n)

instance KnownNat n => GeomAlgebra Int (PGA n) where
  dim x = fromIntegral (natVal x) - grade x
  fromXY x = avec 1 (1:x)
  toXY mv =
    if h == 0 then [] else [coeff [k] mv' / h | k <- [1..n] ]
    where
      n = fromIntegral $ natVal mv
      mv' = dual mv
      h = coeff [0] mv'

------------------------------------------------------------

newtype Pos a (p :: Nat) = Pos a
newtype Zero a (q :: Nat) = Zero a
newtype Neg a (r :: Nat) = Neg a

newtype CA (p :: Nat) (q :: Nat) (r :: Nat)
  = CA (Pos (Zero (Neg MapLS r) q) p)

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
      
deriving via MapLS instance LinSpace [Int] (CA p q r)

deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Eq (CA p q r)
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Show (CA p q r)
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Num (CA p q r)
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Fractional (CA p q r)

------------------------------------------------------------
-- tabulated instance
------------------------------------------------------------

newtype Table e a i = Table (A.Array i (Maybe ([e], Double)))
newtype IndexMap e a = IndexMap (M.Map e Int)

class Tabulated e a | a -> e where
  squareT     :: a -> e -> Double
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
  rcomplT     :: Table e a Int
  lcomplT     :: Table e a Int
  
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
  square (TabulatedGA x) = squareT x
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
  rcompl     = tab  $ lookup1 rcomplT indexT
  lcompl     = tab  $ lookup1 lcomplT indexT

tab :: (a -> a) -> TabulatedGA a -> TabulatedGA a
tab f (TabulatedGA a) = TabulatedGA $ f a

tab2 :: (a -> a -> a)
     -> TabulatedGA a -> TabulatedGA a -> TabulatedGA a
tab2 f (TabulatedGA a) (TabulatedGA b) = TabulatedGA $ f a b

------------------------------------------------------------

-- | Template which generates aliases for given basis of Clifford algebra. 
defineElements :: CliffAlgebra Int a => [a] -> Q [Dec]
defineElements b = concat <$> (mapM go $ tail $ b >>= elems)
  where
    go lst = do
      let name = mkName $ "e"++ foldMap show lst
          a = mkName "a"
          t = mkName "CliffAlgebra"
      expr <- [| e_ lst |]
      int <- [t| Int |]
      return $
        [ SigD name (ForallT [] [AppT (AppT (ConT t) int) (VarT a)] (VarT a))
        , ValD (VarP name) (NormalB expr) []]

newtypeGA :: Name -> Q [Dec]
newtypeGA name =
  (:[]) <$> dataD
    (cxt [])
    name [] Nothing
    [normalC name
     [bangType (bang noSourceUnpackedness noSourceStrictness)
      [t| MapLS |]]]
    []

-- | Template which generates instanses for tabulated geometric algebra. 
tabulateGA :: String -> Integer -> Q Decs
tabulateGA ga n = let
  o = mkName ga
  t = mkName $ ga ++ show n
  tt = conT t
  ot = appT (conT o) (litT (numTyLit n))
  in [d|deriving via $ot instance Eq $tt 
        deriving via $ot instance Num $tt
        deriving via $ot instance Fractional $tt
        deriving via $ot instance Show $tt
        deriving via $ot instance LinSpace [Int] $tt
        deriving via $ot instance GeomAlgebra Int $tt
        deriving via TabulatedGA $tt instance CliffAlgebra Int $tt
        instance Tabulated Int $tt where
          squareT _ = fromIntegral . signum
          signatureT _ = algebraSignature (1 :: $ot)
          indexT = mkIndexMap (coerce <$> (basis :: [$ot]))
          generatorsT = coerce <$> (generators :: [$ot])
          geomT = mkTable2 geom (basis :: [$ot])
          outerT = mkTable2 outer (basis :: [$ot])
          innerT = mkTable2 inner (basis :: [$ot])
          lcontractT = mkTable2 lcontract (basis :: [$ot])
          rcontractT = mkTable2 rcontract (basis :: [$ot])
          revT = mkTable rev (basis :: [$ot])
          invT = mkTable inv (basis :: [$ot])
          conjT = mkTable conj (basis :: [$ot])
          dualT = mkTable dual (basis :: [$ot])
          rcomplT = mkTable rcompl (basis :: [$ot])
          lcomplT = mkTable lcompl (basis :: [$ot]) |] 


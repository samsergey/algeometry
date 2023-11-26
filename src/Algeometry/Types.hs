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
  , CA (..), Dual (..)
  , Outer (..), Outer'
  , VGA (..), VGA'
  , PGA (..), PGA'
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

instance CliffAlgebra Int a => Floating (GeometricNum a) where
  pi = scalar pi

  exp x
    | isScalar x = scalar $ exp $ trace x
    | trace x /= 0 = scale (exp (trace x)) $ exp (x - scalar (trace x))
    | isHomogeneous x = let n = norm x
      in case trace (x*x) `compare` 0 of
        EQ -> 1 + x
        LT -> scalar (cos n) + scale (sin n / n) x
        GT -> scalar (cosh n) + scale (sinh n / n) x
    | otherwise = expSeries x
        
  log x
    | isScalar x = scalar $ log $ trace x
    | otherwise = case norm (x - 1) `compare` 1 of
        EQ -> 0
        LT -> logSeries (x - 1) 
        GT -> - logSeries (1/(x - 1))

  sqrt x
    | isScalar x = scalar $ sqrt $ trace x
    | isHomogeneous x = let n = norm x
      in case trace (x*x) `compare` 0 of
           EQ -> undefined
           LT -> scale (sqrt (n/2)) (1 + scale (1/n) x)
           GT -> error "sqrt for unit multivector is undefined"
    | otherwise = exp (0.5 * log x)
  
  sin x
    | isScalar x = scalar $ sin $ trace x
    | otherwise = let i = head [pseudoScalar, x]
          in (exp x - exp (-x)) / (2*i)
             
  cos x
    | isScalar x = scalar $ cos $ trace x
    | otherwise = scale  0.5 $ exp x + exp (-x)
    
  sinh x
    | isScalar x = scalar $ sinh $ trace x
    | otherwise = scale  0.5 $ exp x - exp (-x)
    
  cosh x
    | isScalar x = scalar $ cosh $ trace x
    | otherwise = scale  0.5 $ exp x + exp (-x)
  
  asin = scalar . asin . trace
  acos = scalar . acos . trace
  atan = scalar . atan . trace
  asinh = scalar . asinh . trace
  acosh = scalar . acosh . trace
  atanh = scalar . atanh . trace

expSeries x = foldr (\i r -> 1 + scale (1/i) x*r) 0 [1..100]
logSeries x = x * foldr (\i r -> scalar ((-1)**(i-1)/i) + x*r) 0 [1..100]

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

-- | Wrapper which represents dual algebra for a given one. 
newtype Dual a = Dual { getDual :: a }

deriving via GeometricNum a
  instance CliffAlgebra e a => Eq (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra e a => Num (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra Int a => Fractional (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra Int a => Floating (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra Int a => Show (Dual a)
deriving via GeometricNum a
  instance LinSpace [Int] a => LinSpace [Int] (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra Int a => CliffAlgebra Int (Dual a)

instance GeomAlgebra Int a => GeomAlgebra Int (Dual a) where
  point = Dual . dual . point
  coord = coord . dual . getDual
  spaceDim = spaceDim . getDual
  dim = dim . dual . getDual

------------------------------------------------------------

-- | Representation of linear space as a map, indexed by integer indexes.
type MapLS = M.Map [Int] Double

-- | Outer (Grassmann) algebra of given dimension.
newtype Outer (n :: Nat) = Outer MapLS

-- | Dual outer (Grassmann) algebra of given dimension.
type Outer' n = Dual (Outer n)

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
deriving via GeometricNum (Outer n)
  instance KnownNat n => Floating (Outer n)

instance KnownNat n => GeomAlgebra Int (Outer n) where
  point = kvec 1
  coord mv = [coeff [k] mv | k <- [1..spaceDim mv] ]
  spaceDim = fromIntegral . natVal
  dim = grade

  
------------------------------------------------------------

-- | Affine vector geometric algebra of given dimension.
newtype VGA (n :: Nat) = VGA (CA n 0 0)
  deriving (Num, Eq, Fractional, Floating)

-- | Dual affine vector geometric algebra of given dimension.
type VGA' n = Dual (VGA n)

deriving via CA n 0 0 instance LinSpace [Int] (VGA n)
deriving via CA n 0 0 instance KnownNat n => CliffAlgebra Int (VGA n)
deriving via CA n 0 0 instance KnownNat n => Show (VGA n)
deriving via Outer n instance KnownNat n => GeomAlgebra Int (VGA n)

------------------------------------------------------------

-- | Projective geometric algebra of given dimension.
newtype PGA (n :: Nat) = PGA (CA n 1 0)
  deriving (Num, Eq, Fractional, Floating)

-- | Dual projective geometric algebra of given dimension.
type PGA' n = Dual (PGA n)

deriving via CA n 1 0 instance LinSpace [Int] (PGA n)
deriving via CA n 1 0 instance KnownNat n => CliffAlgebra Int (PGA n)
deriving via CA n 1 0 instance KnownNat n => Show (PGA n)

instance KnownNat n => GeomAlgebra Int (PGA n) where
  spaceDim = fromIntegral . natVal
  dim x = grade x - 1
  point x = kvec 1 (1:x)
  coord mv =
    if h == 0 then [] else [coeff [k] mv / h | k <- [1..spaceDim mv] ]
    where
      h = coeff [0] mv

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
deriving via GeometricNum (CA p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Floating (CA p q r)

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
tabulateGA :: String -> Integer -> String -> Q Decs
tabulateGA ga n tga = let
  o = mkName ga
  t = mkName $ tga
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


{-|
Module      : Algeometry.Types
Description : Definitions for exact geometric algebras.
Stability   : experimental
-}
{-# LANGUAGE
    UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , KindSignatures
  , StandaloneDeriving
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , FunctionalDependencies
  , TypeFamilies, DataKinds, ScopedTypeVariables
  , TemplateHaskell, TypeOperators  #-}

module Algeometry.Types
  ( CA (..), Dual (..)
  , Outer (..), Outer'
  , VGA (..), VGA'
  , PGA (..), PGA'
  , Tabulated (..), TabulatedGA (..)
  , MapLS
  , defineElements
  , tabulateGA
  )
where

import Algeometry.GeometricAlgebra
import Algeometry.GeometricNum
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.Maybe ( fromMaybe, listToMaybe, maybeToList )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Data.Coerce ( coerce )
import Language.Haskell.TH
import Language.Haskell.TH.Lib.Internal (Decs)

       
------------------------------------------------------------
-- map-based linear space
------------------------------------------------------------

type instance Basis (M.Map e Double) = e

instance Ord e => LinSpace (M.Map e Double) where
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

type instance Basis (Dual a) = Basis a
type instance Generator (Dual a) = Generator a

deriving via GeometricNum a
  instance CliffAlgebra a => Eq (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra a => Num (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra a => Fractional (Dual a)
deriving via GeometricNum a
  instance (Generator a ~ Int, CliffAlgebra a) => Floating (Dual a)
deriving via GeometricNum a
  instance (Generator a ~ Int, CliffAlgebra a) => Show (Dual a)
deriving via GeometricNum a
  instance LinSpace a => LinSpace (Dual a)
deriving via GeometricNum a
  instance CliffAlgebra a => CliffAlgebra (Dual a)

instance GeomAlgebra a => GeomAlgebra (Dual a) where
  point = Dual . dual . point
  coord = coord . dual . getDual
  spaceDim = spaceDim . getDual
  dim = dim . dual . getDual

------------------------------------------------------------

-- | Representation of linear space as a map, indexed by integer indexes.
type MapLS = M.Map [Int] Double
type instance Basis MapLS = [Int]
type instance Generator MapLS = Int

-- | Outer (Grassmann) algebra of given dimension.
newtype Outer (n :: Nat) = Outer MapLS
  deriving LinSpace via MapLS
  deriving ( Show, Num, Eq, Fractional, Floating )
     via GeometricNum (Outer n)

type instance Basis (Outer n) = [Int]
type instance Generator (Outer n) = Int

-- | Dual outer (Grassmann) algebra of given dimension.
type Outer' n = Dual (Outer n)

instance KnownNat n => CliffAlgebra (Outer n) where
  algebraSignature x = (0, fromIntegral $ natVal x, 0)
  square _ _ = 0
  generators = res
    where
      res = (\x -> monom [x] 1) <$> ix
      ix = [1 .. fromIntegral $ natVal (head res)]
  decompose (Outer mv) = (fromMaybe 0 $ s M.!? [], Outer v)
    where
      (s, v) = M.partitionWithKey (\k a -> length k == 0) mv

instance KnownNat n => GeomAlgebra (Outer n) where
  point = kvec 1
  coord mv = [coeff [k] mv | k <- [1..spaceDim mv] ]
  spaceDim = fromIntegral . natVal
  dim = grade

------------------------------------------------------------

newtype CA (p :: Nat) (q :: Nat) (r :: Nat) = CA MapLS
  deriving ( Show, Num, Eq, Fractional, Floating
            ) via GeometricNum (CA p q r)
  deriving LinSpace via MapLS

type instance Basis (CA p q r) = [Int]
type instance Generator (CA p q r) = Int

instance (KnownNat p, KnownNat q, KnownNat r)
         => CliffAlgebra (CA p q r) where
  algebraSignature x = 
    ( fromIntegral $ natVal (Proxy :: Proxy p)
    , fromIntegral $ natVal (Proxy :: Proxy q)
    , fromIntegral $ natVal (Proxy :: Proxy r))

  square _ i = fromIntegral (signum i)

  generators = res
    where
      res = (\x -> monom [x] 1) <$> (reverse negs <> zeros <> pos)
      (p,q,r) = algebraSignature (head res)
      pos   = [k  | p > 0, k <- [1..p]]
      zeros = [0  | q > 0]
      negs  = [-k | r > 0, k <- [1..r]]

  decompose (CA mv) = (fromMaybe 0 $ s M.!? [], CA v)
    where
      (s, v) = M.partitionWithKey (\k a -> length k == 0) mv

------------------------------------------------------------

-- | Affine vector geometric algebra of given dimension.
newtype VGA (n :: Nat) = VGA (CA n 0 0)
  deriving ( Show, Num, Eq, Fractional, Floating
           , LinSpace, CliffAlgebra)

type instance Basis (VGA n) = [Int]
type instance Generator (VGA n) = Int

-- | Dual affine vector geometric algebra of given dimension.
type VGA' n = Dual (VGA n)

deriving via Outer n instance KnownNat n => GeomAlgebra (VGA n)

------------------------------------------------------------

-- | Projective geometric algebra of given dimension.
newtype PGA (n :: Nat) = PGA (CA n 1 0)
  deriving ( Show, Num, Eq, Fractional, Floating
           , LinSpace, CliffAlgebra)

type instance Basis (PGA n) = [Int]
type instance Generator (PGA n) = Int

-- | Dual projective geometric algebra of given dimension.
type PGA' n = Dual (PGA n)

instance KnownNat n => GeomAlgebra (PGA n) where
  spaceDim = fromIntegral . natVal
  dim x = grade x - 1
  point x = kvec 1 (1:x)
  coord mv =
    if h == 0 then [] else [coeff [k] mv / h | k <- [1..spaceDim mv] ]
    where
      h = coeff [0] mv

------------------------------------------------------------
-- tabulated instance
------------------------------------------------------------

newtype Table a i = Table (A.Array i (Maybe ([Generator a], Double)))
newtype IndexMap a = IndexMap (M.Map [Generator a] Int)

class CliffAlgebra a => Tabulated a where
  squareT     :: a -> Generator a -> Double
  signatureT  :: a -> (Int,Int,Int)
  generatorsT :: [a]
  indexT      :: IndexMap a
  geomT       :: Table a (Int,Int)
  outerT      :: Table a (Int,Int)
  innerT      :: Table a (Int,Int)
  lcontractT  :: Table a (Int,Int)
  rcontractT  :: Table a (Int,Int)
  revT        :: Table a Int
  invT        :: Table a Int
  conjT       :: Table a Int
  dualT       :: Table a Int
  rcomplT     :: Table a Int
  lcomplT     :: Table a Int
  
mkTable2 :: (Generator b ~ Generator a, CliffAlgebra a)
         => (a -> a -> a) -> [a] -> Table b (Int, Int)
mkTable2 op b = mkArray $ f <$> b <*> b
  where
    mkArray = Table . A.listArray ((0,0),(n-1,n-1))
    f x = listToMaybe . assoc . op x
    n = length b

mkTable :: (Generator b ~ Generator a, CliffAlgebra a)
        => (a -> a) -> [a] -> Table b Int
mkTable op b = mkArray $ f <$> b
  where
    mkArray = Table . A.listArray (0,n-1)
    f = listToMaybe . assoc . op
    n = length b

mkIndexMap :: (Ord (Basis a), CliffAlgebra a) => [a] -> IndexMap a
mkIndexMap b = IndexMap $ M.fromList $ zip (b >>= elems) [0..]

lookup1 :: CliffAlgebra a
        => Table a Int -> IndexMap a -> a -> a
lookup1 (Table tbl) (IndexMap ix) =
  lmap (\x -> tbl A.! (ix M.! x))

lookup2 :: CliffAlgebra a
        => Table a (Int,Int) -> IndexMap a -> a -> a -> a
lookup2 (Table tbl) (IndexMap ix) =
  lapp (\a b -> tbl A.! (ix M.! a, ix M.! b))

---------------------------------------------------------------------

newtype TabulatedGA a = TabulatedGA a
  deriving (Eq, LinSpace)

type instance Basis (TabulatedGA a) = Basis a
type instance Generator (TabulatedGA a) = Generator a

instance (CliffAlgebra a, Tabulated a ) =>
         CliffAlgebra (TabulatedGA a) where
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
defineElements :: (Generator a ~ Int, CliffAlgebra a)
               => [a] -> Q [Dec]
defineElements b = concat <$> (mapM go $ tail $ b >>= elems)
  where
    go lst = do
      let name = mkName $ "e"++ foldMap show lst
          a = mkName "a"
          t = mkName "CliffAlgebra"
      expr <- [| e_ lst |]
      int <- [t| Int |]
      gen <- [t| Generator |]
      return $
        [ SigD name
          (ForallT []
            [ AppT (ConT t) (VarT a)
            , AppT (AppT EqualityT (AppT gen (VarT a))) int ]
            (VarT a))
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
  in [d|type instance Basis $tt = [Int]
        type instance Generator $tt = Int
        deriving via $ot instance Eq $tt 
        deriving via $ot instance Num $tt
        deriving via $ot instance Fractional $tt
        deriving via $ot instance Floating $tt
        deriving via $ot instance Show $tt
        deriving via $ot instance LinSpace $tt
        deriving via $ot instance GeomAlgebra $tt
        deriving via TabulatedGA $tt instance CliffAlgebra $tt
        instance Tabulated $tt where
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

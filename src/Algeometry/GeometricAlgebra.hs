{-# LANGUAGE UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , KindSignatures
  , StandaloneDeriving
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , FunctionalDependencies #-}

module Algeometry.GeometricAlgebra
  ( LinSpace (..)
  , CliffAlgebra (..)
  , GeomAlgebra (..)
  , e, e_, scalar, scale, vec, pvec
  , elems, coefs, terms
  , isScalar, isHomogeneous
  , scalarPart, getGrade, components
  , pseudoScalar, basis
  , isInvertible, reciprocal
  , weight, bulk
  , norm, norm2, normalize
  , (∧), (∨), (|-), (-|), (∙), (•)
  , meet, join, segmentMeet
  , reflectAt, rotateAt, projectionOf, on, shiftAlong, shiftAlong'
  , line, vector, angle, bisectrissa
  , isPoint, isLine, isPlane
  , Cl, GeometricNum
  , Tabulated (..) , TabulatedGA (..)
  , mkTable, mkTable2, mkIndexMap
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad hiding (join)
import qualified Data.Array as A
import GHC.TypeLits (Nat, KnownNat, natVal)

------------------------------------------------------------
-- LinSpace
------------------------------------------------------------

class Eq e => LinSpace e a | a -> e where
  zero :: a
  isZero :: a -> Bool
  monom :: e  -> Double -> a
  isMonom :: a -> Bool
  add :: a -> a -> a
  lmap :: (e -> Maybe (e, Double)) -> a -> a
  lapp :: (e -> e -> Maybe (e, Double)) -> a -> a -> a
  lfilter :: (e -> Double -> Bool) -> a -> a
  coeff :: e -> a -> Double
  assoc :: a -> [(e, Double)]

  isMonom m = length (assoc m) == 1
  coeff x mv = fromMaybe 0 $ lookup x (assoc mv)

scale :: LinSpace e a => Double -> a -> a
scale a m | a == 0 = zero
          | otherwise = lmap (\k -> Just (k, a)) m

elems :: LinSpace e a => a -> [e]
elems = fmap fst . assoc

coefs :: LinSpace e a => a -> [Double]
coefs = fmap snd . assoc

terms :: LinSpace e a => a -> [a]
terms m = uncurry monom <$> assoc m

------------------------------------------------------------
-- CliffAlgebra
------------------------------------------------------------

class (Eq a, Ord e, LinSpace [e] a) => CliffAlgebra e a  where
  algebraSignature :: a -> (Int,Int,Int)
  square :: a -> e -> Double
  generators :: [a]
  grade :: a -> Int
  geom :: a -> a -> a
  outer :: a -> a -> a
  rcontract :: a -> a -> a
  lcontract :: a -> a -> a
  inner :: a -> a -> a
  rev :: a -> a
  inv :: a -> a
  conj :: a -> a
  dual :: a -> a
  
  -- dim m = length (m:generators) - grade m - 1
  -- point = undefined
  -- toPoint = undefined

  grade m
    | isZero m = 0
    | otherwise = length $ maximumBy (comparing length) (elems m)

  geom m1 = lapp (composeBlades (square m1) (const 1)) m1
  outer = lapp (composeBlades (const 0) (const 1))
  lcontract m1 = lapp (composeBlades (square m1) (const 0)) m1
  rcontract a b = rev (rev b `lcontract` rev a)

  inner a b =
    if grade a <= grade b
    then lcontract a b
    else rcontract a b

  rev = lmap $ \b -> Just (b, (-1)^(length b `div` 2))
  inv = lmap $ \b -> Just (b, (-1)^length b)
  conj = inv . rev
  dual a = lmap (\b -> Just (ps \\ b, 1)) a
    where
      ps = head $ head $ elems <$> [pseudoScalar, a]

composeBlades
  :: Ord b =>
  (b -> Double) -> (b -> Double) -> [b] -> [b] -> Maybe ([b], Double)
composeBlades g h x y = foldM f (y, (-1)^(length x `div` 2)) x
  where
    f (b, p) i = case span (< i) b of
      (l, k:r) | i == k ->
        if g i == 0
        then Nothing
        else Just (l <> r, p*(-1)^length l * g i)
      (l, r) ->
        if h i == 0
        then Nothing
        else Just (l <> (i:r), p*(-1)^length l * h i)

infix 8 -|, |-, ∙
infixr 9 ∧
(∧),(|-),(-|),(∙) :: CliffAlgebra b a => a -> a -> a
(∧) = outer
(-|) = lcontract
(|-) = rcontract
(∙) = inner

infix 9 •
(•) :: CliffAlgebra e a => a -> a -> Double
a • b = scalarPart (inner a b)

getGrade :: CliffAlgebra b a => Int -> a -> a
getGrade n = lfilter (\b _ -> length b == n)

scalarPart :: CliffAlgebra b a => a -> Double
scalarPart = coeff []

weight :: CliffAlgebra b a => a -> a
weight a = lfilter (const . any ((0 ==) . square a)) a

bulk :: CliffAlgebra b a => a -> a
bulk a = lfilter (const . all ((0 /=) . square a)) a

------------------------------
-- constructors

e :: CliffAlgebra e a => e -> a
e k = if elem res generators
      then res
      else zero
  where res = monom [k] 1
        

e_ :: CliffAlgebra e a => [e] -> a
e_ ks  = monom ks 1

scalar :: CliffAlgebra e a => Double -> a
scalar 0 = zero
scalar a = monom [] a

components :: CliffAlgebra e a => a -> [a]
components mv = e <$> sort (foldr union [] (elems mv))

basis :: CliffAlgebra e a => [a]
basis =
  map (foldr outer (scalar 1)) $
  sortOn length $
  filterM (const [True, False]) generators

pseudoScalar :: CliffAlgebra e a => a
pseudoScalar = foldr1 outer generators

------------------------------
-- predicates

isScalar :: CliffAlgebra b a => a -> Bool
isScalar x = grade x == 0

isHomogeneous :: CliffAlgebra b a => a -> Bool
isHomogeneous m = length (nub (length <$> elems m)) <= 1

------------------------------
-- norm

normalize :: CliffAlgebra b a => a -> a
normalize m = scale (1 / norm m) m

norm :: CliffAlgebra b a => a -> Double
norm m | isScalar m = scalarPart m
       | otherwise = sqrt $ abs $ norm2 m 

norm2 :: CliffAlgebra b a => a -> Double
norm2 m
  | isScalar m = scalarPart m ** 2
  | otherwise = scalarPart (rev m `geom` m)

------------------------------
-- reversing

reciprocal :: CliffAlgebra b a => a -> a
reciprocal m
    | not (isInvertible m) = error "Multivector is non-invertible!"
    | isScalar m = scalar $ recip $ scalarPart m
    | isHomogeneous m = scale (1 / scalarPart (m `geom` rev m)) $ rev m
    | otherwise = error "Don't know yet how to invert!"

isInvertible :: CliffAlgebra b a => a -> Bool
isInvertible m
    | isZero m = False
    | isMonom m = not $ isZero $ geom m m
    | otherwise = let m' = geom m (conj m)
                  in grade m' <= 2 && isInvertible m'

------------------------------------------------------------

class CliffAlgebra b a => GeomAlgebra b a where
  point :: [Double] -> a
  toPoint :: a -> [Double]
  dim :: a -> Int

------------------------------
-- geometric objects

vec :: GeomAlgebra b a => [Double] -> a
vec xs = let es = filter ((== 1).grade) basis
  in foldr add zero $ zipWith scale xs es

pvec :: GeomAlgebra b a => [Double] -> a
pvec = dual . vec

line :: GeomAlgebra b a => [Double] -> [Double] -> a
line a b = point a `join` point b

vector :: GeomAlgebra b a => [Double] -> [Double] -> a
vector p1 p2 = point p1 ∨ point p2

angle :: GeomAlgebra b a => a -> a -> Double
angle l1 l2 = acos (l1 • l2)

isPoint :: GeomAlgebra b a => a -> Bool
isPoint x = dim x == 0

isLine :: GeomAlgebra b a => a -> Bool
isLine x = dim x == 1

isPlane :: GeomAlgebra b a => a -> Bool
isPlane x = dim x == 2

------------------------------
-- geometric combibators

infixr 9 ∨
(∨) :: GeomAlgebra b a => a -> a -> a
a ∨ b = dual (dual a ∧ dual b)

meet :: GeomAlgebra b a => a -> a -> a
meet a b = normalize $ a ∧ b

join :: GeomAlgebra b a => a -> a -> a
join a b = normalize $ a ∨ b

segmentMeet :: GeomAlgebra b a => a -> (a, a) -> Maybe a
segmentMeet x (a, b) = let
  p = (a ∨ b) ∧ x
  s = (p ∨ a)•(p ∨ b)
  in if s <= 0 then Just (normalize p) else Nothing

bisectrissa :: (Num a, GeomAlgebra b a) => a -> a -> a
bisectrissa l1 l2 = normalize (normalize l1 + normalize l2)

------------------------------------------------------------
-- transformations
------------------------------------------------------------

reflectAt :: (Num a, GeomAlgebra b a) => a -> a -> a
reflectAt a b = - b * a * reciprocal b

projectionOf :: (Num a, GeomAlgebra b a) => a -> a -> a
projectionOf p l = (p `inner` l)*l

on :: (a -> b) -> a -> b
on = ($)

rotateAt :: (Num a, GeomAlgebra b a) => a -> Double -> a -> a
rotateAt p ang x
  | sin ang == 0 && cos ang == 1 = x
  | otherwise = r * x * rev r
  where
    r = scalar (cos (ang/2)) + scalar (sin (ang/2)) * p

shiftAlong' :: (Num a, GeomAlgebra b a) => a -> Double -> a -> a
shiftAlong' l d p = t * p * rev t
  where
    t = (pseudoScalar + scale (4/(d*norm2 l)) l)^2

shiftAlong :: (Num a, GeomAlgebra b a) => a -> a -> a
shiftAlong l p = t * p * rev t
  where
    t = (pseudoScalar + scale (4/norm2 l) l)^2

   
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

newtype Pos a (p :: Nat) = Pos a
newtype Zero a (q :: Nat) = Zero a
newtype Neg a (r :: Nat) = Neg a

newtype Cl (p :: Nat) (q :: Nat) (r :: Nat)
  = Cl (Pos (Zero (Neg (M.Map [Int] Double) r) q) p)

instance (KnownNat p, KnownNat q, KnownNat r)
         => CliffAlgebra Int (Cl p r q) where
  algebraSignature x = let { Cl p = x; Pos z = p; Zero n = z}
           in ( fromIntegral $ natVal p
              , fromIntegral $ natVal z
              , fromIntegral $ natVal n)
  square _ i = fromIntegral (signum i)
  generators = res
    where
      res = (\x -> monom [x] 1) <$> (negs <> zeros <> pos)
      (p,q,r) = algebraSignature (head res)
      pos = if p == 0 then [] else [1 .. p]
      zeros = if q == 0 then [] else [0]
      negs = if r == 0 then [] else [-r, -r+1..(-1)]
      
deriving via M.Map [Int] Double instance LinSpace [Int] (Cl p q r)

deriving via GeometricNum (Cl p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Eq (Cl p q r)
deriving via GeometricNum (Cl p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Show (Cl p q r)
deriving via GeometricNum (Cl p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Num (Cl p q r)
deriving via GeometricNum (Cl p q r)
  instance (KnownNat p, KnownNat q, KnownNat r) => Fractional (Cl p q r)

------------------------------------------------------------

newtype Table e a i = Table (A.Array i (Maybe ([e], Double)))
newtype IndexMap e a = IndexMap (M.Map e Int)

class Tabulated e a | a -> e where
  signatureT :: a -> (Int,Int,Int)
  generatorsT :: [a]
  indexT :: IndexMap [e] a
  geomT :: Table e a (Int,Int)
  outerT :: Table e a (Int,Int)
  innerT :: Table e a (Int,Int)
  lcontractT :: Table e a (Int,Int)
  rcontractT :: Table e a (Int,Int)
  revT :: Table e a Int
  invT :: Table e a Int
  conjT :: Table e a Int
  dualT :: Table e a Int

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
 
instance ( LinSpace [e] a
         , CliffAlgebra e a
         , Tabulated e a
         ) => CliffAlgebra e (TabulatedGA a) where
  algebraSignature (TabulatedGA a) = signatureT a
  square = undefined
  generators = TabulatedGA <$> generatorsT
  geom = tab2 $ lookup2 geomT indexT
  outer = tab2 $ lookup2 outerT indexT
  inner = tab2 $ lookup2 innerT indexT
  lcontract = tab2 $ lookup2 lcontractT indexT
  rcontract = tab2 $ lookup2 rcontractT indexT
  rev = tab $ lookup1 revT indexT
  inv = tab $ lookup1 invT indexT
  conj = tab $ lookup1 conjT indexT
  dual = tab $ lookup1 dualT indexT

tab f (TabulatedGA a) = TabulatedGA $ f a
tab2 f (TabulatedGA a) (TabulatedGA b) = TabulatedGA $ f a b

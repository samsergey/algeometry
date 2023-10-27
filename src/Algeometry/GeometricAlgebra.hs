{-# LANGUAGE UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , FunctionalDependencies #-}

module Algeometry.GeometricAlgebra
  ( LinSpace (..)
  , GeomAlgebra (..), FiniteGeomAlgebra (..)
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
  , MV, GeometricNum
--  , Prim (..)
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad hiding (join)

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
-- GeomAlgebra
------------------------------------------------------------

class (Ord b, LinSpace [b] a) => GeomAlgebra b a  where
  square :: a -> b -> Double
  grade :: a -> Int
  geom :: a -> a -> a
  outer :: a -> a -> a
  rcontract :: a -> a -> a
  lcontract :: a -> a -> a
  inner :: a -> a -> a
  rev :: a -> a
  inv :: a -> a
  conj :: a -> a

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
(∧),(|-),(-|),(∙) :: GeomAlgebra b a => a -> a -> a
(∧) = outer
(-|) = lcontract
(|-) = rcontract
(∙) = inner

infix 9 •
(•) :: GeomAlgebra e a => a -> a -> Double
a • b = scalarPart (inner a b)

getGrade :: GeomAlgebra b a => Int -> a -> a
getGrade n = lfilter (\b _ -> length b == n)

scalarPart :: GeomAlgebra b a => a -> Double
scalarPart = coeff []

weight :: GeomAlgebra b a => a -> a
weight a = lfilter (const . any ((0 ==) . square a)) a

bulk :: GeomAlgebra b a => a -> a
bulk a = lfilter (const . all ((0 /=) . square a)) a

------------------------------
-- constructors

e :: GeomAlgebra e a => e -> a
e k = monom [k] 1

e_ :: GeomAlgebra e a => [e] -> a
e_ ks  = monom ks 1

scalar :: GeomAlgebra e a => Double -> a
scalar 0 = zero
scalar a = monom [] a

components :: GeomAlgebra e a => a -> [a]
components mv = e <$> sort (foldr union [] (elems mv))

  
------------------------------
-- predicates

isScalar :: GeomAlgebra b a => a -> Bool
isScalar x = grade x == 0

isHomogeneous :: GeomAlgebra b a => a -> Bool
isHomogeneous m = length (nub (length <$> elems m)) <= 1

------------------------------
-- norm

normalize :: GeomAlgebra b a => a -> a
normalize m = scale (1 / norm m) m

norm :: GeomAlgebra b a => a -> Double
norm m | isScalar m = scalarPart m
       | otherwise = sqrt $ abs $ norm2 m 

norm2 :: GeomAlgebra b a => a -> Double
norm2 m
  | isScalar m = scalarPart m ** 2
  | otherwise = scalarPart (rev m `geom` m)

------------------------------
-- reversing

reciprocal :: GeomAlgebra b a => a -> a
reciprocal m
    | not (isInvertible m) = error "Multivector is non-invertible!"
    | isScalar m = scalar $ recip $ scalarPart m
    | isHomogeneous m = scale (1 / scalarPart (m `geom` rev m)) $ rev m
    | otherwise = error "Don't know yet how to invert!"

isInvertible :: GeomAlgebra b a => a -> Bool
isInvertible m
    | isZero m = False
    | isMonom m = not $ isZero $ geom m m
    | otherwise = let m' = geom m (conj m)
                  in grade m' <= 2 && isInvertible m'

------------------------------------------------------------
-- FiniteGeomAlgebra
------------------------------------------------------------

class GeomAlgebra b a => FiniteGeomAlgebra b a where
  generators :: [a]
  point :: [Double] -> a
  toPoint :: a -> [Double]
  dim :: a -> Int
  dual :: a -> a
  
  dim m = length (m:generators) - grade m - 1
  point = undefined
  toPoint = undefined

  dual a = lmap (\b -> Just (ps \\ b, 1)) a
    where
      ps = head $ head $ elems <$> [pseudoScalar, a]

basis :: FiniteGeomAlgebra e a => [a]
basis =
  map (foldr outer (scalar 1)) $
  sortOn length $
  filterM (const [True, False]) generators

pseudoScalar :: FiniteGeomAlgebra e a => a
pseudoScalar = foldr1 outer generators

------------------------------
-- geometric objects

vec :: FiniteGeomAlgebra b a => [Double] -> a
vec xs = let es = filter ((== 1).grade) basis
  in foldr add zero $ zipWith scale xs es

pvec :: FiniteGeomAlgebra b a => [Double] -> a
pvec = dual . vec

line :: FiniteGeomAlgebra b a => [Double] -> [Double] -> a
line a b = point a `join` point b

vector :: FiniteGeomAlgebra b a => [Double] -> [Double] -> a
vector p1 p2 = point p1 ∨ point p2

angle :: FiniteGeomAlgebra b a => a -> a -> Double
angle l1 l2 = acos (l1 • l2)

isPoint :: FiniteGeomAlgebra b a => a -> Bool
isPoint x = dim x == 0

isLine :: FiniteGeomAlgebra b a => a -> Bool
isLine x = dim x == 1

isPlane :: FiniteGeomAlgebra b a => a -> Bool
isPlane x = dim x == 2

------------------------------
-- geometric combibators

infixr 9 ∨
(∨) :: FiniteGeomAlgebra b a => a -> a -> a
a ∨ b = dual (dual a ∧ dual b)

meet :: FiniteGeomAlgebra b a => a -> a -> a
meet a b = normalize $ a ∧ b

join :: FiniteGeomAlgebra b a => a -> a -> a
join a b = normalize $ a ∨ b

segmentMeet :: FiniteGeomAlgebra b a => a -> (a, a) -> Maybe a
segmentMeet x (a, b) = let
  p = (a ∨ b) ∧ x
  s = (p ∨ a)•(p ∨ b)
  in if s <= 0 then Just (normalize p) else Nothing

bisectrissa :: (Num a, FiniteGeomAlgebra b a) => a -> a -> a
bisectrissa l1 l2 = normalize (normalize l1 + normalize l2)

------------------------------------------------------------
-- transformations
------------------------------------------------------------

reflectAt :: (Num a, FiniteGeomAlgebra b a) => a -> a -> a
reflectAt a b = - b * a * reciprocal b

projectionOf :: (Num a, FiniteGeomAlgebra b a) => a -> a -> a
projectionOf p l = (p `inner` l)*l

on :: (a -> b) -> a -> b
on = ($)

rotateAt :: (Num a, FiniteGeomAlgebra b a) => a -> Double -> a -> a
rotateAt p ang x
  | sin ang == 0 && cos ang == 1 = x
  | otherwise = r * x * rev r
  where
    r = scalar (cos (ang/2)) + scalar (sin (ang/2)) * p

shiftAlong' :: (Num a, FiniteGeomAlgebra b a) => a -> Double -> a -> a
shiftAlong' l d p = t * p * rev t
  where
    t = (pseudoScalar + scale (4/(d*norm2 l)) l)^2

shiftAlong :: (Num a, FiniteGeomAlgebra b a) => a -> a -> a
shiftAlong l p = t * p * rev t
  where
    t = (pseudoScalar + scale (4/norm2 l) l)^2
    
------------------------------------------------------------
-- geometric numbers
------------------------------------------------------------

newtype GeometricNum a = GeometricNum a

deriving instance LinSpace e a => LinSpace e (GeometricNum a)
deriving instance GeomAlgebra b a => GeomAlgebra b (GeometricNum a)

instance GeomAlgebra b a => Eq (GeometricNum a) where
  a == b = isZero (a - b)
  
instance GeomAlgebra b a => Num (GeometricNum a) where
  fromInteger 0 = zero
  fromInteger n = scalar (fromInteger n)
  (+) = add
  (*) = geom
  negate = scale (-1)
  abs = undefined
  signum = undefined

instance GeomAlgebra b a => Fractional (GeometricNum a) where
  fromRational = scalar . fromRational 
  recip  = reciprocal

instance GeomAlgebra Int a => Show (GeometricNum a) where
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

newtype MV = MV (M.Map [Int] Double)

instance GeomAlgebra Int MV where
  square _ i = fromIntegral (signum i)
  grade m
    | isZero m = 0
    | otherwise = length $ maximumBy (comparing length) (elems m)

instance FiniteGeomAlgebra Int MV where
  generators = e <$> [-3..3]

deriving via GeometricNum MV instance Eq MV
deriving via GeometricNum MV instance Show MV
deriving via GeometricNum MV instance Num MV
deriving via GeometricNum MV instance Fractional MV
deriving via M.Map [Int] Double instance LinSpace [Int] MV

------------------------------------------------------------

-- data Prim a  = Point ag
--              | Line a
--              | Arrow a
--              | Polygon a
--              | Plane a
--              | Hyperplane a
--   deriving Show

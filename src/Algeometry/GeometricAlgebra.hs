{-# LANGUAGE UndecidableInstances
  , FlexibleInstances
  , FlexibleContexts
  , KindSignatures
  , StandaloneDeriving
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , FunctionalDependencies
  , TypeFamilies, DataKinds #-}

module Algeometry.GeometricAlgebra
  ( LinSpace (..)
  , CliffAlgebra (..)
  , GeomAlgebra (..)
  , e, e_, scalar, scale, vec, pvec
  , elems, coefs, terms
  , isScalar, isHomogeneous, isSingular
  , scalarPart, getGrade, components
  , pseudoScalar, basis
  , isInvertible, reciprocal
  , weight, bulk, norm, norm2, normalize
  , (∧), (∨), (|-), (-|), (∙), (•)
  , meet, join, segmentMeet
  , reflectAt, rotateAt, projectionOf, on
  , shiftAlong, shiftAlong', shift, shift'
  , rescale
  , line, vector, angle, bisectrissa
  , isPoint, isLine, isPlane
  )
where

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
  rcompl :: a -> a
  lcompl :: a -> a

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
  lcompl a = lapp (composeBlades (const 1) (const 1)) pseudoScalar (rev a)
  rcompl a = lapp (composeBlades (const 1) (const 1)) (rev a) pseudoScalar

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
e k = if res `elem` generators then res else zero
  where res = monom [k] 1
      
e_ :: CliffAlgebra e a => [e] -> a
e_ ks = foldl outer (scalar 1) $ e <$> ks

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

isSingular :: (Num a, CliffAlgebra b a) => a -> Bool
isSingular x = x*x == 0

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

shift' :: (GeomAlgebra Int a, Num a) => a -> Double -> a -> a
shift' l d x = t * x * rev t
  where t = 1 + scale (s * d/2) (e 0) * (point [] `inner` l)
        (p,q,r) = algebraSignature x
        s = (-1)^((p+q) `div` 2 + 1)

shift :: (GeomAlgebra Int a, Num a) => a -> a -> a
shift l = shift' l 1

shiftAlong' :: (Num a, GeomAlgebra b a) => a -> Double -> a -> a
shiftAlong' l d p = t * p * rev t
  where
    t = (pseudoScalar + scale (4/(d*norm2 l)) l)^2

shiftAlong :: (Num a, GeomAlgebra b a) => a -> a -> a
shiftAlong l = shiftAlong' l 1

rescale :: (Num a, CliffAlgebra b a) => Double -> a -> a
rescale s a = scale s (weight a) + bulk a

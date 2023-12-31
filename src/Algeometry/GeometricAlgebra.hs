{-|
Module      : Algeometry.GeometricAlgebraOld
Description : Definitions of classes and general operations.
Stability   : experimental
-}
{-# LANGUAGE UndecidableInstances
, FlexibleInstances
, FlexibleContexts
, KindSignatures
, StandaloneDeriving
, DerivingVia
, GeneralizedNewtypeDeriving
, TypeFamilies
, TypeFamilyDependencies
, DataKinds
, TypeOperators #-}

module Algeometry.GeometricAlgebra
( Basis, Generator, LinSpace (..)
  , CliffAlgebra (..)
  , GeomAlgebra (..)
  , e, e_, scalar, kvec, nvec, avec, angle
  , elems, coefs, terms, lerp
  , isScalar, isHomogeneous, isSingular
  , getGrade, components
  , pseudoScalar, basis
  , isInvertible, reciprocal, isSquare
  , scale, weight, bulk, norm, norm2, normalize
  , (∧), (∨), (|-), (-|), (∙), (•), (->|), (<-|)
  , meet, join, segmentMeet, clipPoly
  , reflectAt, rotateAt, projectOn, antiprojectTo
  , shiftAlong, shiftAlong'
  , rescale, stretch
  , isPoint, isLine, isPlane
  ) where

import Data.Maybe ( fromMaybe, mapMaybe )
import Data.List ( maximumBy, (\\), nub, sort, sortOn, union )
import Data.Ord ( comparing )
import Control.Monad ( filterM, foldM )

-- | Type family for basis element  of LinearSpace
type family Basis a

{- | The class representing a general linear space. -}
class Eq (Basis el) => LinSpace el where
  {-# MINIMAL zero, isZero, monom, add, lmap, lapp, lfilter, assoc #-}
  -- | The zero element.
  zero :: el

  -- | Predicate for the zero element.
  isZero :: el -> Bool

  -- | Constructor for the monom.
  monom :: Basis el -> Double -> el

  -- | Predicate for the monom.
  isMonom :: el -> Bool
  isMonom m = length (assoc m) == 1

  -- | Addition for two elements.
  add :: el -> el -> el

  -- | Mapping of partial unary linear function through an element.
  lmap :: (Basis el -> Maybe (Basis el, Double)) -> el -> el

  -- | Distribution of partial binary linear function through two elements.
  lapp :: (Basis el -> Basis el -> Maybe (Basis el, Double)) -> el -> el -> el

  -- | Extraction of elements with given predicate.
  lfilter :: (Basis el -> Double -> Bool) -> el -> el

  -- | Scalar coefficient of an element 
  coeff :: Basis el -> el -> Double
  coeff x mv = fromMaybe 0 $ lookup x (assoc mv)

  -- | Representation of an element as assoclist.
  assoc :: el -> [(Basis el, Double)]

-- | Returns an element of a linear space, scaled by given factor.
scale :: LinSpace el => Double -> el -> el
scale a m | a == 0 = zero
          | otherwise = lmap (\k -> Just (k, a)) m

-- | Returns list of basis elements for an element.
elems :: LinSpace el => el -> [Basis el]
elems = fmap fst . assoc

-- | Returns list of coefficients for an element.
coefs :: LinSpace el => el -> [Double]
coefs = fmap snd . assoc

-- | Returns list of monoms for an element.
terms :: LinSpace el => el -> [el]
terms m = uncurry monom <$> assoc m

-- | Linear interpolation for two elements, parameterized for interval [0,1].
lerp :: LinSpace el => el -> el -> Double -> el
lerp a b t = a `add` scale t (b `add` scale (-1) a)

------------------------------------------------------------

-- | Type family for generator of Clifford algebra
type family Generator a

{- | The class representing Clifford algebra. -}
class ( Ord (Generator mv)
      , Basis mv ~ [Generator mv]
      , LinSpace mv
      , Eq mv ) => CliffAlgebra mv where
  -- | The signature of Clifford algebra. The first argument is proxy needed to specify the class.
  algebraSignature :: mv -> (Int,Int,Int)

  -- | Squares of the generators, which define the Clifford algebra. The first argument is proxy needed to resolve the dependence for `Generator` type instance.
  square :: mv -> Generator mv -> Double

  -- | List of generators of the Clifford algebra.
  generators :: [mv]

  -- | Returns the grade of a multivector.
  grade :: mv -> Int
  grade m
    | isZero m = 0
    | otherwise = length $ maximumBy (comparing length) (elems m)

  -- | The geometric product.
  {-# INLINE geom #-}
  geom :: mv -> mv -> mv
  geom m = lapp (composeBlades (square m) (const 1)) m

  -- | The outer product (same as `(∧)`).
  {-# INLINE outer #-}
  outer :: mv -> mv -> mv
  outer = lapp (composeBlades (const 0) (const 1))

  -- | The right contraction (same as `(|-)`).
  {-# INLINE rcontract #-}
  rcontract :: mv -> mv -> mv
  rcontract a b = rev (rev b `lcontract` rev a)

  -- | The left contraction (same as `(-|)`).
  {-# INLINE lcontract #-}
  lcontract :: mv -> mv -> mv
  lcontract m1 = lapp (composeBlades (square m1) (const 0)) m1

  -- | The inner product  (same as `(·)`).
  {-# INLINE inner #-}
  inner :: mv -> mv -> mv
  inner a b =
    if grade a <= grade b
    then lcontract a b
    else rcontract a b

  {-# INLINE rev #-}
  -- | The reverse of a multivector.
  rev :: mv -> mv
  rev = lmap $ \b -> Just (b, (-1)^(length b `div` 2))

  {-# INLINE inv #-}
  -- | The inversion of a multivector.
  inv :: mv -> mv
  inv = lmap $ \b -> Just (b, (-1)^length b)

  {-# INLINE conj #-}
  -- | The conjugate of a multivector
  conj :: mv -> mv
  conj = inv . rev

  {-# INLINE dual #-}
  -- | The dual of a multivector.
  dual :: mv -> mv
  dual a = lmap (\b -> Just (ps \\ b, 1)) a
    where
      ps = head $ head $ elems <$> [pseudoScalar, a]

  {-# INLINE rcompl #-}
  -- | The right complement of a multivector.
  rcompl :: mv -> mv
  rcompl a = lapp (composeBlades (const 1) (const 1)) (rev a) pseudoScalar

  {-# INLINE lcompl #-}
  -- | The left complement of a multivector.
  lcompl :: mv -> mv
  lcompl a = lapp (composeBlades (const 1) (const 1)) pseudoScalar (rev a)

  {-# INLINE decompose #-}
  -- | Separates scalar and non-scalar part of a multivector.
  decompose :: mv -> (Double, mv)
  decompose x = (coeff [] x, lfilter (\b _ -> not (null b)) x)

  {-# INLINE trace #-}
  -- | Extracts a scalar part from a multivector.
  trace :: mv -> Double
  trace = fst . decompose

  {-# INLINE nonScalar #-}
  -- | Extracts a non-scalar part from a multivector.
  nonScalar :: mv -> mv
  nonScalar = snd . decompose 
  
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

------------------------------------------------------------

-- | The class representing Clifford algebra that have geometric representation.
class CliffAlgebra mv => GeomAlgebra mv where
  -- | Representation of a point, given as coordinate list, as a k-vector.
  point :: [Double] -> mv

  -- | Representation of a k-vector as a point, given as coordinate list.
  coord :: mv -> [Double]

  -- | Returns a geometric dimension for the embedding space of the algebra. The multivector is used as a type witness.
  spaceDim :: mv -> Int

  -- | Returns a geometric dimension for a multivector. It does not alwais correspond to grade, and depends on the dimension of the embedding space for dual algebras.   
  dim :: mv -> Int

------------------------------------------------------------

infixr 9 ∧

-- | The infix operator for the `outer` product
(∧) :: CliffAlgebra mv => mv -> mv -> mv
(∧) = outer

infix 8 -|, |-, ∙

-- | The infix operator for the `lcontract`.
(-|) :: CliffAlgebra mv => mv -> mv -> mv
(-|) = lcontract

-- | The infix operator for `rcontract`.
(|-) :: CliffAlgebra mv => mv -> mv -> mv
(|-) = rcontract

-- | The infix operator for the `inner` product
(∙) :: CliffAlgebra mv => mv -> mv -> mv
(∙) = inner

infix 9 •
-- | The infix operator for the scalar product
(•) :: CliffAlgebra mv => mv -> mv -> Double
a • b = trace (inner a b)

-- | Extracts k-blade from a multivector.
getGrade :: CliffAlgebra mv => Int -> mv -> mv
getGrade k = lfilter (\b _ -> length b == k)

-- | Extracts vanishing part from a multivector.
weight :: CliffAlgebra mv => mv -> mv
weight a = lfilter (const . any ((0 ==) . square a)) a

-- | Extracts non-vanishing part from a multivector.
bulk :: CliffAlgebra mv => mv -> mv
bulk a = lfilter (const . all ((0 /=) . square a)) a

------------------------------
-- constructors

-- | Returns a generator of the algebra.
e :: CliffAlgebra mv => Generator mv -> mv
e k = if res `elem` generators then res else zero
  where res = monom [k] 1
      
-- | Returns a monomial element of the algebra (outer product of generators).
e_ :: CliffAlgebra mv => [Generator mv] -> mv
e_ ks = foldl outer (scalar 1) $ e <$> ks

{- | Returns a scalar element of the algebra.

>>> scalar (sin 1) + scalar (cos 1) * e12 :: VGA 2
0.8414709848078965 + 0.5403023058681398e₁₂
-}
scalar :: CliffAlgebra mv => Double -> mv
scalar 0 = zero 
scalar a = monom [] a

-- | Returns a list of monomial components in the multivector.
components :: CliffAlgebra mv => mv -> [mv]
components mv = e <$> sort (foldr union [] (elems mv))

{- | Returns a list of all monomial components in the algebra.

>>> basis :: [VGA 3]
[1.0,e₁,e₂,e₃,e₁₂,e₁₃,e₂₃,e₁₂₃]
>>> basis :: [PGA 3]
[1.0,e₀,e₁,e₂,e₃,e₀₁,e₀₂,e₀₃,e₁₂,e₁₃,e₂₃,e₀₁₂,e₀₁₃,e₀₂₃,e₁₂₃,e₀₁₂₃]
-}
basis :: CliffAlgebra mv => [mv]
basis =
  map (foldr outer (scalar 1)) $
  sortOn length $
  filterM (const [True, False]) generators

{- | Returns a pseudoscalar of the algebra.

>>> pseudoScalar :: VGA 3
e₁₂₃
>>> pseudoScalar :: PGA 3
e₀₁₂₃
-}
pseudoScalar :: CliffAlgebra mv => mv
pseudoScalar = foldr1 outer generators

{- | Returns a k-vector with given grade and coefficients.

>>>  kvec 1 [1,2,3] :: VGA 2
e₁ + 2.0e₂ + 3.0e₃
>>> kvec 2 [1,2,3] :: VGA 3
e₁₂ + 2.0e₁₃ + 3.0e₂₃
-}
kvec :: GeomAlgebra mv => Int -> [Double] -> mv
kvec k xs = let es = filter ((== k).grade) basis
  in foldr add zero $ zipWith scale xs es

-- | Returns a normalized k-vector with given grade and coefficients.
nvec :: GeomAlgebra mv => Int -> [Double] -> mv
nvec k = normalize . kvec k

{- | Returns a normalized k-antivector with given grade and coefficients.

>>> avec 1 [1,2,3] :: VGA 3
3.0e₁₂ + 2.0e₁₃ + e₂₃
>>> avec 2 [1,2,3] :: VGA 3
3.0e₁ + 2.0e₂ + e₃
-}
avec :: GeomAlgebra mv => Int -> [Double] -> mv
avec k = dual . kvec k

-- | Returns an angle between two multivectors.
angle :: GeomAlgebra mv => mv -> mv -> Double
angle l1 l2 = acos (l1 • l2)

------------------------------
-- predicates

-- | Returns @True@ if  multivector is scalar and @False@ otherwise.
isScalar :: CliffAlgebra mv => mv -> Bool
isScalar x = grade x == 0

-- | Returns @True@ if  multivector is a k-vector and @False@ otherwise..
isHomogeneous :: CliffAlgebra mv => mv -> Bool
isHomogeneous m = length (nub (length <$> elems m)) <= 1

-- | Returns @True@ if  multivector is singular (non-invertible) and @False@ otherwise.
isSingular :: CliffAlgebra mv => mv -> Bool
isSingular x = isZero $ x `geom` x

------------------------------
-- norm

-- | Returns normalized multivector for given nonsingular multivector.
normalize :: CliffAlgebra mv => mv -> mv
normalize m = scale (1 / norm m) m

-- | Returns norm of a multivector.
norm :: CliffAlgebra mv => mv -> Double
norm m | isScalar m = trace m
       | otherwise = sqrt $ abs $ norm2 m 

-- | Returns square norm of a multivector.
norm2 :: CliffAlgebra mv => mv -> Double
norm2 m
  | isScalar m = trace m ** 2
  | otherwise = trace (rev m `geom` m)

------------------------------
-- reversing

-- | Returns reciprocal for nonsingular k-vector.
reciprocal :: CliffAlgebra mv => mv -> mv
reciprocal m
    | not (isInvertible m) = error "Multivector is non-invertible!"
    | isScalar m = scalar $ recip $ trace m
    | otherwise = scale (1 / trace (m `geom` rev m)) $ rev m

-- | Returns @True@ if  multivector is not singular (invertible) and @False@ otherwise.
isInvertible :: CliffAlgebra mv => mv -> Bool
isInvertible m
    | isZero m = False
    | isMonom m = not $ isZero $ geom m m
    | otherwise = let m' = geom m (conj m)
                  in grade m' <= 2 && isInvertible m'

-- | Returns @True@ if  multivector could be expressed as square of another mulivector, and @False@ otherwise.
isSquare :: CliffAlgebra mv => mv -> Bool
isSquare x =
  any (\a -> trace (geom a a) < 0) [x, pseudoScalar]


-- |  Returns @True@ if  k-vector represents a point.
isPoint :: GeomAlgebra mv => mv -> Bool
isPoint x = dim x == 0

-- |  Returns @True@ if  k-vector represents a line.
isLine :: GeomAlgebra mv => mv -> Bool
isLine x = dim x == 1

-- |  Returns @True@ if  k-vector represents a plane.
isPlane :: GeomAlgebra mv => mv -> Bool
isPlane x = dim x == 2

------------------------------
-- geometric combinators

infixr 9 ∨
-- | Regressive product of two multivectors.
(∨) :: GeomAlgebra mv => mv -> mv -> mv
a ∨ b = dual (dual a ∧ dual b)

-- | Normalized outer product of two multivectors.
meet :: GeomAlgebra mv => mv -> mv -> mv
meet a b = normalize $ a ∧ b

-- | Normalized regressive product of two multivectors.
join :: GeomAlgebra mv => mv -> mv -> mv
join a b = normalize $ a ∨ b

-- | Returns intersection of an object and a segment.
segmentMeet :: GeomAlgebra mv => mv -> (mv, mv) -> Maybe mv
segmentMeet x (a, b) = let
  p = (a ∨ b) ∧ x
  s = (p ∨ a)•(p ∨ b)
  in if s <= 0 then Just (normalize p) else Nothing

-- | For polygon and a line returns list of intersection points.
clipPoly :: GeomAlgebra mv => [mv] -> mv -> [mv]
clipPoly pts mv = let
  frame = zip pts (tail pts)
  in mapMaybe (segmentMeet mv) frame

------------------------------------------------------------
-- transformations
------------------------------------------------------------

-- | Returns reflection of object @a@ against object @b@.
reflectAt :: GeomAlgebra mv => mv -> mv -> mv
reflectAt a b = b `geom` inv a `geom` reciprocal b

-- | Returns projection of object @a@ on object @b@.
projectOn :: GeomAlgebra mv => mv -> mv -> mv
projectOn a b = (a `inner` b) `geom` reciprocal b

-- | Infix operator for projection.
(->|) :: GeomAlgebra mv => mv -> mv -> mv
(->|) = projectOn

-- | Returns antiprojection of object @a@ on object @b@.
antiprojectTo :: GeomAlgebra mv => mv -> mv -> mv
antiprojectTo a b = (a `inner` b) `geom` reciprocal a

-- | Infix operator for antiprojection.
(<-|) :: GeomAlgebra mv => mv -> mv -> mv
(<-|) = antiprojectTo

-- | Rotates object @x@ against the object @p@ by given angle.
rotateAt :: GeomAlgebra mv => mv -> Double -> mv -> mv
rotateAt p ang x
  | sin ang == 0 && cos ang == 1 = x
  | otherwise = r `geom` x `geom` rev r
  where
    r = scalar (cos (ang/2)) `add` scale (sin (ang/2)) p

-- | Translates object @x@ along the object @l@ by given distance @d@.
shiftAlong' :: GeomAlgebra mv => mv -> Double -> mv -> mv
shiftAlong' l d x
  | isZero l = x
  | otherwise = q `geom` x `geom` rev q
  where q' = pseudoScalar `add` scale (1/d*4/norm2 l) l
        q = q' `geom` q'

-- | Translates an object along the object @l@ by distance, given by norm of @l@.
shiftAlong :: GeomAlgebra mv => mv -> mv -> mv
shiftAlong l = shiftAlong' l 1

-- | Rescales an object @a@ by given value.
rescale :: CliffAlgebra mv => Double -> mv -> mv
rescale s a = scale s (weight a) `add` bulk a

-- | Rescales magnitude of object @a@ by given value.
stretch :: CliffAlgebra mv => Double -> mv -> mv
stretch s a = weight a `add` scale s (bulk a)

------------------------------------------------------------

type instance Basis Double = [()]

instance LinSpace Double where
  zero = 0
  isZero = (== 0)
  monom _ b = b
  isMonom _ = True
  add  = (+)
  lmap f x = x * maybe 0 snd (f [])
  lapp f x y = x * y * maybe 0 snd (f [] [])
  lfilter f x = if f [] x then x else 0 
  coeff _ x = x
  assoc x = [([],x)]

type instance Generator Double = ()

instance CliffAlgebra Double where
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
  rcompl = id
  lcompl = id
  decompose x = (x, 0)
  

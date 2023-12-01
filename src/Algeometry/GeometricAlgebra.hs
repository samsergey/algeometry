{-|
Module      : Algeometry.GeometricAlgebra
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
  , FunctionalDependencies
  , TypeFamilies, DataKinds #-}

module Algeometry.GeometricAlgebra
  ( LinSpace (..)
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
  )
where

import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad hiding (join)

------------------------------------------------------------
-- LinSpace
------------------------------------------------------------

{- | The class representing a general linear space with basis of type @b@ and element (vector) of type @el@.-}
class Eq b => LinSpace b el | el -> b where
  {-# MINIMAL zero, isZero, monom, add, lmap, lapp, lfilter, assoc #-}
  -- | The zero element.
  zero :: el

  -- | Predicate for the zero element.
  isZero :: el -> Bool

  -- | Constructor for the monom.
  monom :: b -> Double -> el

  -- | Predicate for the monom.
  isMonom :: el -> Bool
  isMonom m = length (assoc m) == 1

  -- | Addition for two elements.
  add :: el -> el -> el

  -- | Mapping of partial unary linear function through an element.
  lmap :: (b -> Maybe (b, Double)) -> el -> el

  -- | Distribution of partial binary linear function through two elements.
  lapp :: (b -> b -> Maybe (b, Double)) -> el -> el -> el

  -- | Extraction of elements with given predicate.
  lfilter :: (b -> Double -> Bool) -> el -> el

  -- | Scalar coefficient of an element 
  coeff :: b -> el -> Double
  coeff x mv = fromMaybe 0 $ lookup x (assoc mv)

  -- | Representation of an element as assoclist.
  assoc :: el -> [(b, Double)]

-- | Returns an element of a linear space, scaled by given factor.
scale :: LinSpace b el => Double -> el -> el
scale a m | a == 0 = zero
          | otherwise = lmap (\k -> Just (k, a)) m

-- | Returns list of basis elements for an element.
elems :: LinSpace b el => el -> [b]
elems = fmap fst . assoc

-- | Returns list of coefficients for an element.
coefs :: LinSpace b el => el -> [Double]
coefs = fmap snd . assoc

-- | Returns list of monoms for an element.
terms :: LinSpace b el => el -> [el]
terms m = uncurry monom <$> assoc m

-- | Linear interpolation for two elements, parameterized for interval [0,1].
lerp :: (Num el, LinSpace b el) => el -> el -> Double -> el
lerp a b t = a + scale t (b - a)

------------------------------------------------------------
-- CliffAlgebra
------------------------------------------------------------

{- | The class representing Clifford algebra with generators of type @g@
and element (multivector) of type @mv@. -}
class (Eq mv, Ord g, LinSpace [g] mv) => CliffAlgebra g mv  where
  -- | The signature of Clifford algebra. The first argument is proxy needed to resolve the functional dependence for class instance.
  algebraSignature :: mv -> (Int,Int,Int)

  -- | Squares of the generators, which define the Clifford algebra. The first argument is proxy needed to resolve the functional dependence for class instance.
  square :: mv -> g -> Double

  -- | List of generators of the Clifford algebra.
  generators :: [mv]

  -- | Returns the grade of a multivector.
  grade :: mv -> Int
  grade m
    | isZero m = 0
    | otherwise = length $ maximumBy (comparing length) (elems m)
    
  -- | The geometric product.
  geom :: mv -> mv -> mv
  geom m1 = lapp (composeBlades (square m1) (const 1)) m1

  -- | The outer product (same as `(∧)`).
  outer :: mv -> mv -> mv
  outer = lapp (composeBlades (const 0) (const 1))

  -- | The right contraction (same as `(|-)`).
  rcontract :: mv -> mv -> mv
  rcontract a b = rev (rev b `lcontract` rev a)

  -- | The left contraction (same as `(-|)`).
  lcontract :: mv -> mv -> mv
  lcontract m1 = lapp (composeBlades (square m1) (const 0)) m1

  -- | The inner product  (same as `(·)`).
  inner :: mv -> mv -> mv
  inner a b =
    if grade a <= grade b
    then lcontract a b
    else rcontract a b

  -- | The reverse of a multivector.
  rev :: mv -> mv
  rev = lmap $ \b -> Just (b, (-1)^(length b `div` 2))

  -- | The inversion of a multivector.
  inv :: mv -> mv
  inv = lmap $ \b -> Just (b, (-1)^length b)

  -- | The conjugate of a multivector
  conj :: mv -> mv
  conj = inv . rev

  -- | The dual of a multivector.
  dual :: mv -> mv
  dual a = lmap (\b -> Just (ps \\ b, 1)) a
    where
      ps = head $ head $ elems <$> [pseudoScalar, a]

  -- | The right complement of a multivector.
  rcompl :: mv -> mv
  rcompl a = lapp (composeBlades (const 1) (const 1)) (rev a) pseudoScalar

  -- | The left complement of a multivector.
  lcompl :: mv -> mv
  lcompl a = lapp (composeBlades (const 1) (const 1)) pseudoScalar (rev a)

  -- | Separates scalar and non-scalar part of a multivector.
  decompose :: mv -> (Double, mv)
  decompose x = (coeff [] x, lfilter (\b _ -> not (null b)) x)

  -- | Extracts a scalar part from a multivector.
  trace :: mv -> Double
  trace = fst . decompose

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

infixr 9 ∧

-- | The infix operator for the `outer` product
(∧) :: CliffAlgebra g mv => mv -> mv -> mv
(∧) = outer

infix 8 -|, |-, ∙

-- | The infix operator for the `lcontract`.
(-|) :: CliffAlgebra g mv => mv -> mv -> mv
(-|) = lcontract

-- | The infix operator for `rcontract`.
(|-) :: CliffAlgebra g mv => mv -> mv -> mv
(|-) = rcontract

-- | The infix operator for the `inner` product
(∙) :: CliffAlgebra g mv => mv -> mv -> mv
(∙) = inner

infix 9 •
-- | The infix operator for the scalar product
(•) :: CliffAlgebra g mv => mv -> mv -> Double
a • b = trace (inner a b)

-- | Extracts k-blade from a multivector.
getGrade :: CliffAlgebra g mv => Int -> mv -> mv
getGrade k = lfilter (\b _ -> length b == k)

-- | Extracts vanishing part from a multivector.
weight :: CliffAlgebra g mv => mv -> mv
weight a = lfilter (const . any ((0 ==) . square a)) a

-- | Extracts non-vanishing part from a multivector.
bulk :: CliffAlgebra g mv => mv -> mv
bulk a = lfilter (const . all ((0 /=) . square a)) a

------------------------------
-- constructors

-- | Returns a generator of the algebra.
e :: CliffAlgebra g mv => g -> mv
e k = if res `elem` generators then res else zero
  where res = monom [k] 1
      
-- | Returns a monomial element of the algebra (outer product of generators).
e_ :: CliffAlgebra g mv => [g] -> mv
e_ ks = foldl outer (scalar 1) $ e <$> ks

{- | Returns a scalar element of the algebra.

>>> scalar (sin 1) + scalar (cos 1) * e12 :: VGA 2
0.8414709848078965 + 0.5403023058681398e₁₂
-}
scalar :: CliffAlgebra g mv => Double -> mv
scalar 0 = zero 
scalar a = monom [] a

-- | Returns a list of monomial components in the multivector.
components :: CliffAlgebra g mv => mv -> [mv]
components mv = e <$> sort (foldr union [] (elems mv))

{- | Returns a list of all monomial components in the algebra.

>>> basis :: [VGA 3]
[1.0,e₁,e₂,e₃,e₁₂,e₁₃,e₂₃,e₁₂₃]
>>> basis :: [PGA 3]
[1.0,e₀,e₁,e₂,e₃,e₀₁,e₀₂,e₀₃,e₁₂,e₁₃,e₂₃,e₀₁₂,e₀₁₃,e₀₂₃,e₁₂₃,e₀₁₂₃]
-}
basis :: CliffAlgebra g mv => [mv]
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
pseudoScalar :: CliffAlgebra g mv => mv
pseudoScalar = foldr1 outer generators

{- | Returns a k-vector with given grade and coefficients.

>>>  kvec 1 [1,2,3] :: VGA 2
e₁ + 2.0e₂ + 3.0e₃
>>> kvec 2 [1,2,3] :: VGA 3
e₁₂ + 2.0e₁₃ + 3.0e₂₃
-}
kvec :: GeomAlgebra g mv => Int -> [Double] -> mv
kvec k xs = let es = filter ((== k).grade) basis
  in foldr add zero $ zipWith scale xs es

-- | Returns a normalized k-vector with given grade and coefficients.
nvec :: GeomAlgebra g mv => Int -> [Double] -> mv
nvec k = normalize . kvec k

{- | Returns a normalized k-antivector with given grade and coefficients.

>>> avec 1 [1,2,3] :: VGA 3
3.0e₁₂ + 2.0e₁₃ + e₂₃
>>> avec 2 [1,2,3] :: VGA 3
3.0e₁ + 2.0e₂ + e₃
-}
avec :: GeomAlgebra g mv => Int -> [Double] -> mv
avec k = dual . kvec k

-- | Returns an angle between two multivectors.
angle :: GeomAlgebra g mv => mv -> mv -> Double
angle l1 l2 = acos (l1 • l2)

------------------------------
-- predicates

-- | Returns @True@ if  multivector is scalar and @False@ otherwise.
isScalar :: CliffAlgebra g mv => mv -> Bool
isScalar x = grade x == 0

-- | Returns @True@ if  multivector is a k-vector and @False@ otherwise..
isHomogeneous :: CliffAlgebra g mv => mv -> Bool
isHomogeneous m = length (nub (length <$> elems m)) <= 1

-- | Returns @True@ if  multivector is singular (non-invertible) and @False@ otherwise.
isSingular :: (Num mv, CliffAlgebra g mv) => mv -> Bool
isSingular x = x*x == 0

------------------------------
-- norm

-- | Returns normalized multivector for given nonsingular multivector.
normalize :: CliffAlgebra g mv => mv -> mv
normalize m = scale (1 / norm m) m

-- | Returns norm of a multivector.
norm :: CliffAlgebra g mv => mv -> Double
norm m | isScalar m = trace m
       | otherwise = sqrt $ abs $ norm2 m 

-- | Returns square norm of a multivector.
norm2 :: CliffAlgebra g mv => mv -> Double
norm2 m
  | isScalar m = trace m ** 2
  | otherwise = trace (rev m `geom` m)

------------------------------
-- reversing

-- | Returns reciprocal for nonsingular k-vector.
reciprocal :: CliffAlgebra g mv => mv -> mv
reciprocal m
    | not (isInvertible m) = error "Multivector is non-invertible!"
    | isScalar m = scalar $ recip $ trace m
    | otherwise = scale (1 / trace (m `geom` rev m)) $ rev m

-- | Returns @True@ if  multivector is not singular (invertible) and @False@ otherwise.
isInvertible :: CliffAlgebra g mv => mv -> Bool
isInvertible m
    | isZero m = False
    | isMonom m = not $ isZero $ geom m m
    | otherwise = let m' = geom m (conj m)
                  in grade m' <= 2 && isInvertible m'

-- | Returns @True@ if  multivector could be expressed as square of another mulivector, and @False@ otherwise.
isSquare :: CliffAlgebra g mv => mv -> Bool
isSquare x =
  any (\a -> trace (geom a a) < 0) [x, pseudoScalar]
  
------------------------------------------------------------

-- | The class representing Clifford algebra that have geometric representation.
class CliffAlgebra g mv => GeomAlgebra g mv where
  -- | Representation of a point, given as coordinate list, as a k-vector.
  point :: [Double] -> mv

  -- | Representation of a k-vector as a point, given as coordinate list.
  coord :: mv -> [Double]

  -- | Returns a geometric dimension for the embedding space of the algebra. The multivector is used as a type witness.
  spaceDim :: mv -> Int

  -- | Returns a geometric dimension for a multivector. It does not alwais correspond to grade, and depends on the dimension of the embedding space for dual algebras.   
  dim :: mv -> Int

-- |  Returns @True@ if  k-vector represents a point.
isPoint :: GeomAlgebra g mv => mv -> Bool
isPoint x = dim x == 0

-- |  Returns @True@ if  k-vector represents a line.
isLine :: GeomAlgebra g mv => mv -> Bool
isLine x = dim x == 1

-- |  Returns @True@ if  k-vector represents a plane.
isPlane :: GeomAlgebra g mv => mv -> Bool
isPlane x = dim x == 2

------------------------------
-- geometric combinators

infixr 9 ∨
-- | Regressive product of two multivectors.
(∨) :: GeomAlgebra g mv => mv -> mv -> mv
a ∨ b = dual (dual a ∧ dual b)

-- | Normalized outer product of two multivectors.
meet :: GeomAlgebra g mv => mv -> mv -> mv
meet a b = normalize $ a ∧ b

-- | Normalized regressive product of two multivectors.
join :: GeomAlgebra g mv => mv -> mv -> mv
join a b = normalize $ a ∨ b

-- | Returns intersection of an object and a segment.
segmentMeet :: GeomAlgebra g mv => mv -> (mv, mv) -> Maybe mv
segmentMeet x (a, b) = let
  p = (a ∨ b) ∧ x
  s = (p ∨ a)•(p ∨ b)
  in if s <= 0 then Just (normalize p) else Nothing

-- | For polygon and a line returns list of intersection points.
clipPoly :: GeomAlgebra g mv => [mv] -> mv -> [mv]
clipPoly pts mv = let
  frame = zip pts (tail pts)
  in mapMaybe (segmentMeet mv) frame

------------------------------------------------------------
-- transformations
------------------------------------------------------------

-- | Returns reflection of object @a@ against object @b@.
reflectAt :: (Num mv, GeomAlgebra g mv) => mv -> mv -> mv
reflectAt a b = - b * a * reciprocal b

-- | Returns projection of object @a@ on object @b@.
projectOn :: GeomAlgebra g mv => mv -> mv -> mv
projectOn a b = (a `inner` b) `geom` reciprocal b

-- | Infix operator for projection.
(->|) :: GeomAlgebra g mv => mv -> mv -> mv
(->|) = projectOn

-- | Returns antiprojection of object @a@ on object @b@.
antiprojectTo :: GeomAlgebra g mv => mv -> mv -> mv
antiprojectTo a b = (a `inner` b) `geom` reciprocal a

-- | Infix operator for antiprojection.
(<-|) :: GeomAlgebra g mv => mv -> mv -> mv
(<-|) = antiprojectTo

-- | Rotates object @x@ against the object @p@ by given angle.
rotateAt :: (Num mv, GeomAlgebra g mv) => mv -> Double -> mv -> mv
rotateAt p ang x
  | sin ang == 0 && cos ang == 1 = x
  | otherwise = r * x * rev r
  where
    r = scalar (cos (ang/2)) + scale (sin (ang/2)) p

-- | Translates object @x@ along the object @l@ by given distance @d@.
shiftAlong' :: (GeomAlgebra g mv, Fractional mv)
            => mv -> Double -> mv -> mv
shiftAlong' l d x
  | l == 0 = x
  | otherwise = q * x * rev q
  where q = (pseudoScalar + scale (1/d*4/norm2 l) l)^2

-- | Translates an object along the object @l@ by distance, given by norm of @l@.
shiftAlong :: (GeomAlgebra g mv, Fractional mv)
           => mv -> mv -> mv
shiftAlong l = shiftAlong' l 1

-- | Rescales an object @a@ by given value.
rescale :: (Num mv, CliffAlgebra g mv) => Double -> mv -> mv
rescale s a = scale s (weight a) + bulk a

-- | Rescales magnitude of object @a@ by given value.
stretch :: (Num mv, CliffAlgebra g mv) => Double -> mv -> mv
stretch s a = weight a + scale s (bulk a)

------------------------------------------------------------

instance LinSpace [Double] Double where
  zero = 0
  isZero = (== 0)
  monom _ b = b
  isMonom _ = True
  add  = (+)
  lmap f x = x * maybe 0 snd (f [])
  lapp f x y = x * y * maybe 0 snd (f [] [])
  lfilter f x = if f [] x then x else 0 
  coeff [a] x = x/a
  coeff _ x = x
  assoc x = [([],x)]


instance CliffAlgebra Double Double where
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
  

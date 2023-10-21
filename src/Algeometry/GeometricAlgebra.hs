{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algeometry.GeometricAlgebra
    ( module Algeometry.LinSpace
    , GeometricAlgebra (..)
    , MV
    , vec, pvec
    , pseudoScalar, algebraElements
    , isInvertible
    , weight, bulk
    , norm, norm2, normalize
    , rev, inv, conj, dual
    , (∧), (∨), (⊢), (⊣), (∙), (•)
    , geom, outer, inner, rcontract, lcontract, reciprocal
    , meet, join, segmentMeet
    , reflectAt, rotateAt, projectionOf, on, shiftAlong
    , line, angle, bisectrissa
    , isPoint, isLine, isPlane
    ) where

import Algeometry.LinSpace

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad as CM
import Data.Maybe (fromMaybe)
import Data.List (sortOn, stripPrefix)
import Control.Monad hiding (join)


------------------------------------------------------------
-- GeometricAlgebra
------------------------------------------------------------

class LinSpace a => GeometricAlgebra a where
  basis :: [a]
  dim :: a -> Int
  point :: [Double] -> a
  toPoint :: a -> [Double]

  point = dual . vec

isPoint :: GeometricAlgebra a => a -> Bool
isPoint x = dim x == 0

isLine :: GeometricAlgebra a => a -> Bool
isLine x = dim x == 1

isPlane :: GeometricAlgebra a => a -> Bool
isPlane x = dim x == 2

weight :: GeometricAlgebra a => a -> a
weight mv =
  sum $ filter (\x -> isZero $ x * x) $ terms mv

bulk :: GeometricAlgebra a => a -> a
bulk mv =
  sum $ filter (\x -> not $ isZero $ x * x) $ terms mv
  
------------------------------
-- products

infix 8 ⊣, ⊢, ∙
infixr 9 ∧
(∧),(⊢),(⊣),(∙) :: GeometricAlgebra a => a -> a -> a
(∧) = outer
(⊣) = lcontract
(⊢) = rcontract
(∙) = inner

mulBlades :: ((Bool, Component) -> [Term]) -> Term -> Term -> [Term]
mulBlades mul (a, sa) (b, sb) =
  foldM (insertWith mul) (b, sa*sb*(-1)^(S.size a `div` 2)) a

insertWith :: ((Bool, Component) -> [Term]) -> Term -> Component -> [Term]
insertWith mul (z, p) i = do
  let (l,c,r) = S.splitMember i z
  (k, s) <- mul (c, i)
  return (l <> k <> r, p*s*(-1)^S.size l)

geom :: GeometricAlgebra a => a -> a -> a
geom = productWith $ \case  
  (True,  0) -> []
  (True,  i) -> [(mempty, signum (fromIntegral i))]
  (False, i) -> [(S.singleton i, 1)]

outer :: GeometricAlgebra a => a -> a -> a
outer = productWith $ \case
  (True,  _) -> []
  (False, i) -> [(S.singleton i, 1)]

inner :: GeometricAlgebra a => a -> a -> a
inner a b = sum $ imul <$> terms a <*> terms b 
  where
    imul x y = getGrade (abs (grade x - grade y)) (geom x y)

rcontract :: GeometricAlgebra a => a -> a -> a
rcontract a b = sum $ rmul <$> terms a <*> terms b 
  where
    rmul x y = getGrade (grade x - grade y) (geom x y)

lcontract :: GeometricAlgebra a => a -> a -> a
lcontract a b = rev (rev b `rcontract` rev a)

infix 9 •
(•) :: GeometricAlgebra a => a -> a -> Double
a • b = scalarPart (a `inner` b)

------------------------------
-- norm

normalize :: GeometricAlgebra a => a -> a
normalize m = scale (1 / norm m) m

norm :: GeometricAlgebra a => a -> Double
norm m | isScalar m = scalarPart m
       | isMonom m = head (coefficients m)
       | otherwise = sqrt $ abs $ norm2 m 

norm2 :: GeometricAlgebra a => a -> Double
norm2 m
  | isScalar m = scalarPart m ** 2
  | isMonom m = head (coefficients m) ** 2
  | otherwise = scalarPart (rev m * m)

------------------------------
-- involutions and duality

rev :: GeometricAlgebra a => a -> a
rev = mapTerms $
  \(b, c) -> [(b, c * (-1)^(S.size b `div` 2))]

inv :: GeometricAlgebra a => a -> a  
inv = mapTerms $
  \(b, c) -> [(b, c * (-1)^S.size b)]

conj :: GeometricAlgebra a => a -> a
conj = inv . rev

------------------------------
-- reversing

reciprocal :: GeometricAlgebra a => a -> a
reciprocal m
    | not (isInvertible m) = error $ "Multivector is non-invertible: " <> show m
    | isScalar m = scalar $ recip $ scalarPart m
    | isHomogeneous m = scale (1 / scalarPart (m * rev m)) $ rev m
    | otherwise = error $ "Don't know yet how to invert" <> show m

isInvertible :: GeometricAlgebra a => a -> Bool
isInvertible m
    | isZero m = False
    | isMonom m = not $ isZero $ geom m m
    | otherwise = let m' = geom m (conj m)
                  in grade m' <= 2 && isInvertible m'

------------------------------
-- constructors

pseudoScalar :: GeometricAlgebra a => a
pseudoScalar = product basis

algebraElements :: GeometricAlgebra a => [a]
algebraElements =
  map product $
  sortOn length $
  filterM (const [True, False]) basis

------------------------------
-- involutions and duality  

dual :: GeometricAlgebra a => a -> a
dual = productWith diff pseudoScalar . rev
  where
    diff = \case
      (True,  _) -> [(mempty, 1)]
      (False, i) -> [(S.singleton i, 1)]

------------------------------
-- geometric objects

vec :: GeometricAlgebra a => [Double] -> a
vec xs = let es = filter ((== 1).grade) algebraElements
  in sum $ zipWith scale xs es

pvec :: GeometricAlgebra a => [Double] -> a
pvec = dual . vec

line :: GeometricAlgebra a => [Double] -> [Double] -> a
line a b = point a `join` point b

angle :: GeometricAlgebra a => a -> a -> Double
angle l1 l2 = acos (l1 • l2)

------------------------------
-- geometric combibators

infixr 9 ∨
(∨) :: GeometricAlgebra a => a -> a -> a
a ∨ b = dual (dual a ∧ dual b)

meet :: GeometricAlgebra a => a -> a -> a
meet a b = normalize $ a ∧ b

join :: GeometricAlgebra a => a -> a -> a
join a b = normalize $ a ∨ b

segmentMeet :: GeometricAlgebra a => a -> (a, a) -> Maybe a
segmentMeet x (a, b) = let
  p = (a ∨ b) ∧ x
  s = (p ∨ a)•(p ∨ b)
  in if s <= 0 then Just (normalize p) else Nothing

bisectrissa :: GeometricAlgebra a => a -> a -> a
bisectrissa l1 l2 = normalize (normalize l1 + normalize l2)

------------------------------------------------------------
-- transformations
------------------------------------------------------------

reflectAt :: GeometricAlgebra a => a -> a -> a
reflectAt a b = - b * a * reciprocal b

projectionOf :: GeometricAlgebra a => a -> a -> a
projectionOf p l = (p `inner` l)*l

on :: (a -> b) -> a -> b
on = ($)

rotateAt :: GeometricAlgebra a => a -> Double -> a -> a
rotateAt p ang x = r * x * rev r
  where
    r = scalar (cos (ang/2)) + scalar (sin (ang/2)) * p

shiftAlong :: GeometricAlgebra a => a -> Double -> a -> a
shiftAlong l d p = t * p * rev t
  where
    t = dual (pseudoScalar + scale d (bulk l))
    
------------------------------------------------------------
-- instances
------------------------------------------------------------

newtype MV = MV (M.Map Blade Double)

instance Eq MV where
  a == b = all (< 1e-8) $ abs <$> coefficients (a - b) 
  
instance Show MV where
  show m = if m == 0 then "0" else strip $ sscal <> svecs
    where
      trms (MV mv) = sortOn (S.size . fst) $ M.toList mv
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

instance LinSpace MV where
  zero = MV mempty

  isZero (MV m) = M.null m

  monom xs = MV $ M.singleton (foldMap S.singleton xs) 1

  isMonom (MV m) = M.size m == 1

  blades (MV m) = M.keys m

  coefficients (MV m) = M.elems m

  coefficient b (MV m) =
    fromMaybe 0 $ M.lookup (foldMap S.singleton b) m

  terms (MV m) =
    MV . uncurry M.singleton <$> M.toList m

  scale 0 _ = zero
  scale a (MV m) = MV $ (a *) <$> m

  mapTerms f (MV a) =
    MV $ M.fromListWith (+) $ M.toList a >>= f

  productWith f (MV a) (MV b) =
    MV $ M.filter ((> 1e-10).abs) $ M.fromListWith (+) $
    CM.join $ mulBlades f <$> M.toList a <*> M.toList b 

instance GeometricAlgebra MV where
  basis = e <$> [-3..3]
  dim = grade
  toPoint = undefined
  
instance Num MV where
  fromInteger = scalar . fromInteger
  MV a + MV b =
    MV $ M.filter ((> 1e-10).abs) $ M.unionWith (+) a b  
  (*) = geom
  negate (MV m) = MV $ negate <$> m
  abs = undefined
  signum = undefined

instance Fractional MV where
  fromRational = scalar . fromRational 
  recip  = reciprocal

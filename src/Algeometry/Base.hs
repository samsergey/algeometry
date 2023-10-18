{-# LANGUAGE DataKinds, LambdaCase, KindSignatures#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algeometry.Base
    ( LinSpace (..), GeometricAlgebra
    , e, e_, scalar
    , isScalar, isHomogeneous, isInvertible
    , scalarPart, grade, getGrade
    , norm, norm2, normalize
    , rev, inv, conj, dual
    , (∧), (∨), (⊢), (⊣), (∙), (•)
    , geom, outer, inner, rcontract, lcontract, reciprocal
    , meet, join, reflectAt, segmentMeet, projectOn
    , pseudoScalar, algebraElements
    , vec, pvec
    , MV, VGA, PGA (..)
    , point, line, toPoint
    , isPoint, isLine, isPlane
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad as CM
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.List (nub, maximumBy, sortOn, stripPrefix)
import Control.Monad hiding (join)
import GHC.TypeLits (Nat, KnownNat, natVal)

------------------------------------------------------------
-- LinSpace
------------------------------------------------------------

type Component = Int
type Blade = S.Set Component
type Term = (Blade, Double)

class (Eq a, Num a, Show a) => LinSpace a where
  zero :: a
  monom :: Foldable t => t Component -> a
  isMonom :: a -> Bool
  blades :: a -> [Blade]
  coefficients :: a -> [Double]
  coefficient :: Foldable t => t Component -> a -> Double
  terms :: a -> [a]
  scale :: Double -> a -> a
  mapTerms :: (Term -> [Term]) -> a -> a
  productWith :: ((Bool, Component) -> [Term]) -> a -> a -> a

------------------------------
-- constructors

e :: LinSpace a => Component -> a
e k = monom [k]

e_ :: LinSpace a => [Component] -> a
e_ = monom

scalar :: LinSpace a => Double -> a
scalar 0 = zero
scalar x = scale x $ monom []

------------------------------
-- predicates

isScalar :: LinSpace a => a -> Bool
isScalar x = grade x == 0

isHomogeneous :: LinSpace a => a -> Bool
isHomogeneous m = length (nub (grade <$> terms m)) == 1

------------------------------
-- parts

scalarPart :: LinSpace a => a -> Double
scalarPart = coefficient []

grade :: LinSpace a => a -> Int
grade m
  | null (terms m) = 0
  | otherwise = S.size $ maximumBy (comparing S.size) $ blades m

getGrade :: LinSpace a => Int -> a -> a  
getGrade k = mapTerms $
  \(b,c) -> guard (S.size b == k) >> [(b, c)]

------------------------------
-- norm

normalize :: LinSpace a => a -> a
normalize m = scale (1 / norm m) m

norm :: LinSpace a => a -> Double
norm m | isScalar m = scalarPart m
       | isMonom m = head (coefficients m)
       | otherwise = sqrt $ abs $ norm2 m 

norm2 :: LinSpace a => a -> Double
norm2 m
  | isScalar m = scalarPart m ** 2
  | isMonom m = head (coefficients m) ** 2
  | otherwise = scalarPart (rev m * m)

------------------------------
-- involutions and duality

rev :: LinSpace a => a -> a
rev = mapTerms $
  \(b, c) -> [(b, c * (-1)^(S.size b `div` 2))]

inv :: LinSpace a => a -> a  
inv = mapTerms $
  \(b, c) -> [(b, c * (-1)^S.size b)]

conj :: LinSpace a => a -> a
conj = inv . rev

------------------------------
-- products

infix 8 ⊣, ⊢, ∙
infixr 9 ∧
(∧),(⊢),(⊣),(∙) :: LinSpace a => a -> a -> a
(∧) = outer
(⊢) = lcontract
(⊣) = rcontract
(∙) = inner

mulBlades :: ((Bool, Component) -> [Term]) -> Term -> Term -> [Term]
mulBlades mul (a, sa) (b, sb) =
  foldM (insertWith mul) (b, sa*sb*(-1)^(S.size a `div` 2)) a

insertWith :: ((Bool, Component) -> [Term]) -> Term -> Component -> [Term]
insertWith mul (z, p) i = do
  let (l,c,r) = S.splitMember i z
  (k, s) <- mul (c, i)
  return (l <> k <> r, p*s*(-1)^S.size l)

geom :: LinSpace a => a -> a -> a
geom = productWith $ \case  
  (True,  0) -> []
  (True,  i) -> [(mempty, signum (fromIntegral i))]
  (False, i) -> [(S.singleton i, 1)]

outer :: LinSpace a => a -> a -> a
outer = productWith $ \case
  (True,  _) -> []
  (False, i) -> [(S.singleton i, 1)]

inner :: LinSpace a => a -> a -> a
inner a b = sum $ imul <$> terms a <*> terms b 
  where
    imul x y = getGrade (abs (grade x - grade y)) (x * y)

rcontract :: LinSpace a => a -> a -> a
rcontract a b = sum $ rmul <$> terms a <*> terms b 
  where
    rmul x y = getGrade (grade x - grade y) (x * y)

lcontract :: LinSpace a => a -> a -> a
lcontract a b = rev (rev b `rcontract` rev a)

infix 9 •
(•) :: LinSpace a => a -> a -> Double
a • b = scalarPart (a `inner` b)

------------------------------
-- reversing

reciprocal :: LinSpace a => a -> a
reciprocal m
    | not (isInvertible m) = error $ "Multivector is non-invertible: " <> show m
    | isScalar m = scalar $ recip $ scalarPart m
    | isHomogeneous m = scale (1 / norm2 m) $ rev m
    | otherwise = error $ "Don't know yet how to invert" <> show m

isInvertible :: LinSpace a => a -> Bool
isInvertible m
    | m == zero = False
    | isMonom m = m * m /= zero
    | otherwise = let m' = m * conj m
                  in grade m' <= 2 && isInvertible m'
   
------------------------------------------------------------
-- GeometricAlgebra
------------------------------------------------------------

class LinSpace a => GeometricAlgebra a where
  basis :: [a]
  dim :: a -> Int
  point :: [Double] -> a

  point = dual . vec

isPoint :: GeometricAlgebra a => a -> Bool
isPoint x = dim x == 0

isLine :: GeometricAlgebra a => a -> Bool
isLine x = dim x == 1

isPlane :: GeometricAlgebra a => a -> Bool
isPlane x = dim x == 2

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
-- geometry

infixr 9 ∨
(∨) :: GeometricAlgebra a => a -> a -> a
a ∨ b = dual (dual a ∧ dual b)

meet :: GeometricAlgebra a => a -> a -> a
meet a b = normalize $ a ∧ b

join :: GeometricAlgebra a => a -> a -> a
join a b = normalize $ a ∨ b

reflectAt :: GeometricAlgebra a => a -> a -> a
reflectAt a b = - b * a * reciprocal b

projectOn :: LinSpace a => a -> a -> a
projectOn p l = (p ⊣ l)*l

vec :: GeometricAlgebra a => [Double] -> a
vec xs = let es = filter ((== 1).grade) algebraElements
  in sum $ zipWith scale xs es

pvec :: GeometricAlgebra a => [Double] -> a
pvec = dual . vec

line :: GeometricAlgebra a => [Double] -> [Double] -> a
line a b = point a `join` point b

toPoint :: KnownNat n => PGA n -> [Double]
toPoint mv =
  if h == 0 then [] else [coefficient [k] mv' / h | k <- [1..n] ]
  where
    n = fromIntegral $ natVal mv
    mv' = dual mv
    h = coefficient [0] mv'

segmentMeet :: GeometricAlgebra a => a -> (a, a) -> Maybe a
segmentMeet x (a, b) = let
  p = (a ∨ b) ∧ x
  s = (p ∨ a)•(p ∨ b)
  in if s <= 0 then Just (normalize p) else Nothing


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
  basis = e <$> [-8..8]
  dim = grade

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

------------------------------------------------------------

newtype VGA (n :: Nat) = VGA MV
  deriving (LinSpace, Num, Eq, Fractional)

instance Show (VGA n) where
  show (VGA v) = show v

instance KnownNat n => GeometricAlgebra (VGA n) where
  basis = res
    where
      res = e <$> [1 .. fromIntegral n]
      n = natVal (head res)
  dim x = fromIntegral (natVal x) - grade x - 1
------------------------------------------------------------

newtype PGA (n :: Nat) = PGA MV
  deriving (LinSpace, Num, Eq, Fractional)

instance Show (PGA n) where
  show (PGA v) = show v

instance KnownNat n => GeometricAlgebra (PGA n) where
  basis = res
    where
      res = e <$> [0 .. fromIntegral n]
      n = natVal (head res)
  dim x = fromIntegral (natVal x) - grade x
  point x = dual $ vec (1:x)

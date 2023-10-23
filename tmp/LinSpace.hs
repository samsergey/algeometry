module Algeometry.LinSpace
    ( LinSpace (..), Component, Blade, Term
    , e, e_, scalar
    , isScalar, isHomogeneous
    , scalarPart, grade, getGrade, components
    ) where

import qualified Data.Set as S
import Data.Ord (comparing)
import Data.List (nub, maximumBy)
import Control.Monad

------------------------------------------------------------
-- LinSpace
------------------------------------------------------------

type Component = Int
type Blade = S.Set Component
type Term = (Blade, Double)

class (Show a, Num a) => LinSpace a where
  zero :: a
  isZero :: a -> Bool
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

components :: LinSpace a => a -> [a]
components mv = e <$> S.elems (S.unions (blades mv))


{-|
Module      : Algeometry.GeometricNum
Description : Definitions for exact geometric algebras.
Stability   : experimental
-}
{-# LANGUAGE UndecidableInstances
, FlexibleInstances
, FlexibleContexts
, StandaloneDeriving
, GeneralizedNewtypeDeriving
, FunctionalDependencies
, TypeFamilies
, DataKinds
, TypeOperators #-}

module Algeometry.GeometricNum
  ( GeometricNum (..)
  , expSeries ) where

import Algeometry.GeometricAlgebra
import Data.List
import Data.Maybe

{- | This newtype implements various classes for arbitrary Clifford algebra.
It may be used to derive numeric classes for exact Geometric algebra implementation using @deriving via@ mechanics.
-}
newtype GeometricNum a = GeometricNum a

type instance Basis (GeometricNum a) = Basis a
type instance Generator (GeometricNum a) = Generator a

deriving instance LinSpace a => LinSpace (GeometricNum a)

deriving instance CliffAlgebra a => CliffAlgebra (GeometricNum a)

instance CliffAlgebra a => Eq (GeometricNum a) where
  a == b = isZero (a - b)
  
instance CliffAlgebra a => Num (GeometricNum a) where
  -- | Integer is just a scalar.
  fromInteger 0 = zero
  fromInteger n = scalar (fromInteger n)
  (+) = add
  (*) = geom
  negate = scale (-1)
  abs = error "GeometricNum: abs is undefined!"
  signum 0 = 0
  signum x = scalar $ signum $ trace $ x*x

instance CliffAlgebra a => Fractional (GeometricNum a) where
  fromRational = scalar . fromRational 
  recip  = reciprocal

instance (Generator a ~ Int, CliffAlgebra a) => Show (GeometricNum a) where
  show m = if isZero m then "0" else strip $ sscal <> svecs
    where
      trms mv = sortOn (length . fst) $ assoc mv
      strip x = tail $ fromMaybe x $ stripPrefix " +" x
      scal = getGrade 0 m
      sscal = let c = trace scal
              in if c == 0 then "" else ssig c <> show (abs c)
      svecs = do (b, c) <- trms (nonScalar m)
                 ssig c <> snum c <> showBlade b
      ssig n = if n < 0 then " - " else " + "
      snum n = if abs n == 1 then "" else show (abs n)

      showBlade b = if null b then "1" else 'e':foldMap showIx b
        where
          showIx k | k < 0 = '₋' : index (-k)
                   | otherwise = index k
          index k = ["₀₁₂₃₄₅₆₇₈₉" !! k]

instance (Generator a ~ Int, CliffAlgebra a) => Floating (GeometricNum a) where
  pi = scalar pi

  exp x
    | isScalar x = scalar $ exp $ trace x
    | isHomogeneous v =
        scale (exp x0) $
        case trace (v*v) `compare` 0 of
          LT -> scalar (cos n) + scale (sin n / n) v
          EQ -> 1 + v
          GT -> scalar (cosh n) + scale (sinh n / n) v
    | otherwise = series expS x
    where
      (x0, v) = decompose x
      n = norm v
        
  log x
    | isScalar x = scalar $ log $ trace x
    | isHomogeneous x' = case trace (x'*x') `compare` 0 of
        LT -> scalar (log n) + scale (atan2 n' x0 / n') x'
        EQ -> error $ "log of vanishing multivector " ++ show x
        GT -> scalar (log n) + scale (atanh (n'/x0) / n') x'
    | otherwise = logSeries x
    where
      (x0, x') = decompose x
      n' = norm x'
      n = norm x

  sqrt x
    | isScalar x = scalar $ sqrt $ trace x
    | not (isSquare x) = error $ "sqrt is not defined for " ++ show x
    | isScalar x2 =
        if trace x2 < 0
        then scale (sqrt (n/2)) (1 + scale (1/n) x)
        else scale (sqrt n / 2) $ (i + scale (1/n) x) * (1 - i)
    | isHomogeneous x = sqrt a * (1 + b / (2*a))
    | otherwise = exp (0.5 * log x)
    where x2 = x*x
          n = norm x
          i = pseudoScalar
          a = weight a
          b = bulk a
  
  sin x
    | isScalar x = scalar $ sin $ trace x
    | otherwise = series sinS x
             
  cos x
    | isScalar x = scalar $ cos $ trace x
    | otherwise = series cosS x
    
  sinh x
    | isScalar x = scalar $ sinh $ trace x
    | isHomogeneous x = nonScalar (exp x)
    | otherwise = series sinhS x
    
  cosh x
    | isScalar x = scalar $ cosh $ trace x
    | isHomogeneous x = scalar $ trace (exp x)
    | otherwise = series coshS x
  
  asin = scalar . asin . trace
  acos = scalar . acos . trace
  atan = scalar . atan . trace
  asinh = scalar . asinh . trace
  acosh = scalar . acosh . trace
  atanh = scalar . atanh . trace

series :: (Eq a, Num a) => (t -> [a]) -> t -> a
series step x = converge $ scanl (+) 0 $ take 1000 $ step x

expSeries :: (Num a, LinSpace a, Eq a) => a -> a
expSeries = series expS

logSeries x
  | n < 1 = x' * foldr (\i r -> scalar ((-1)**(i-1)/i) + x'*r) 0 [1..200]
  | otherwise = - logSeries (1/x)
  where
    x' = x-1
    n = norm x

converge [] = error "converge: empty list"
converge xs = fromMaybe empty (convergeBy checkPeriodic Just xs) 
    where
      empty = error "converge: error in implementation"
      checkPeriodic (a:b:c:_)
          | a == b = Just a
          | a == c = Just a
      checkPeriodic _ = Nothing

convergeBy :: ([a] -> Maybe b) -> (a -> Maybe b) -> [a] -> Maybe b
convergeBy f end = listToMaybe . catMaybes . map f' . tails
    where 
        f' xs = case f xs of
            Nothing -> end' xs
            other   -> other
        end' [x] = end x
        end'  _  = Nothing

expS :: (LinSpace b, Num b) => b -> [b]
expS x = map snd $ iterate step (1,1)
  where
    step (n,b) = (n + 1, scale (1/n) (x*b))

coshS x = thinOut $ expS x
sinhS x = thinOut $ tail $ expS x
cosS x = zipWith (*) (cycle [1,-1]) $ coshS x
sinS x = zipWith (*) (cycle [1,-1]) $ sinhS x

thinOut (x:y:t) = x: thinOut t
thinOut x = x

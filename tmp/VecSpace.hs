module Algeometry.VecSpace where

import Linear.Vector
import qualified Data.Map as M
import qualified Data.Set as S


class VectorSpace a => GeometricAlgebra a where
  geom :: a -> a -> a
  outer :: a -> a -> a
  grade :: a -> Int

newtype MV = MV (M.Map (S.Set Int) Double)
  deriving (Show, Generic)

instance (Ord a, Num b) => AdditiveGroup (M.Map a b) where
  zeroV = mempty

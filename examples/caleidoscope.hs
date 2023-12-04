{-# LANGUAGE OverloadedStrings
, TypeFamilies, DataKinds #-}
module Main (main, caleidoscope) where

import Algeometry
import Algeometry.SVG
import Lucid.Svg
import Control.Monad.Random hiding (join)
import Data.Foldable

reflectFig m = mapFig (\x -> m*x/m)

reflections mirrors = iterate $ foldMap reflectFig mirrors

caleidoscope :: (Eq a, Foldable t, Fractional a)
             => Int -> t a -> Figure a b -> Figure a b
caleidoscope n mirrors = fold . take n . reflections mirrors

caleidoscope1 :: Figure PGA2 PGA2
caleidoscope1 = do
  -- горизонтальное зеркало
  m1 <- e2 @ []
  -- зеркало расположеноое под углом
  m2 <- rotateAt e12 (2*pi/5) m1 @ []
  -- шестикратные отражения точки в паре зеркал 
  caleidoscope 6 [m1,m2] (point [5,7] @ [])

caleidoscope2 :: Figure PGA2 PGA2
caleidoscope2 = do
  -- горизонтальное зеркало
  m1 <- line [0,-2] [1,-2] @ []
  -- два зеркала расположенные под углом к первому
  m2 <- rotateAt e12 (2*pi/3) m1 @ []
  m3 <- rotateAt e12 (2*pi/3) m2 @ []
  -- многократные отражения точки в трёх зеркалах 
  caleidoscope 10 [m1,m2,m3] (point [1,1] @ [])

-- случайная точка
randomPoint = point <$> (replicateM 2 $ getRandomR (-8,8))

-- случайный треугольник
randomPolygon = do
  col <- fromList [("red",1),("blue",1),("yellow",1)
                  ,("green",1),("magenta",1),("cyan",1)]
  pts <- replicateM 3 randomPoint
  return $
    polygon pts <@ [fill_ col, stroke_ "none", opacity_ "0.5"]

-- система зеркал
threeMirrors =
  take 3 $ iterate (rotateAt e12 (2*pi/3)) $ line [0,-4] [1,-4]

-- калейдоскоп из 15 треугольников
caleidoscope3 = do
  polygons <- fold <$> replicateM 15 randomPolygon
  return $ caleidoscope 5 threeMirrors polygons

randomRotor = do
  pt <- randomPoint
  w <- fromInteger <$> getRandomR (-4,4)
  return $ \f t -> mapFig (rotateAt pt (w*t)) f

animateCaleidoscope :: Int -> Int -> Rand StdGen (Animation ())
animateCaleidoscope m n = do
  polygons <- replicateM m randomPolygon
  rotors <- replicateM m randomRotor
  let fig = fold $ zipWith ($) rotors polygons
  return $ animate 96 (0,2*pi) (caleidoscope n threeMirrors . fig)

main :: IO ()
main = do
  print $ product $ fst $ sequence $ getFigure $ caleidoscope 20 mrs (put (point [1,1,2,3,4] :: PGA2))
--  cal <- evalRandIO (animateCaleidoscope 25 4)
--  runAnimation "caleidoscope.gif" cal

mrs :: GeomAlgebra a => [a]
mrs = let
  m1 = line [0,-2] [1,-2]
  m2 = rotateAt e12 (2*pi/3) m1
  m3 = rotateAt e12 (2*pi/3) m2
  in [m1, m2 ,m3]

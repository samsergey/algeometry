{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry

$(defineElements (basis :: [PGA2]))

desargues :: Figure PGA2 PGA2
desargues = mapFig (rotateAt e12 (pi/4) . rescale 10 . dual) $ do
  -- стороны треугольника, как 1-векторы
  a <- nvec 1 [-15,1,5]   @ [stroke_ "blue", id_"a"]
  b <- nvec 1 [-6,2,1]    @ [stroke_ "blue", id_"b"]
  c <- nvec 1 [20,3,-1]   @ [stroke_ "blue", id_"c"]
  polygon [a, b, c]      <@ [fill_ "wheat"]
  -- ось проекции, как 1-вектор
  p <- nvec 1 [1,1,-10]   @ [stroke_width_ "2", id_"p"]
  -- спроецированный треугольник
  let (x,y,z) = (2.5,1,-2)
  a' <- a + x*p           @ [stroke_ "green", id_"a'"]
  b' <- b + y*p           @ [stroke_ "green", id_"b'"]
  c' <- c + z*p           @ [stroke_ "green", id_"c'"]
  polygon [a', b', c']   <@ [fill_ "pink"]
  -- линии, соединяющие вершины двух треугольников
  y*a - x*b               @ [stroke_dasharray_ "8 4"]
  z*b - y*c               @ [stroke_dasharray_ "8 4"]
  x*c - z*a               @ [stroke_dasharray_ "8 4"]
  -- точка их пересечения
  z*a∧b + x*b∧c + y*c∧a @ [id_ "P"]

main = writeSVG "desargues.svg" desargues

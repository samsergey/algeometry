{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry
import Algeometry.SVG

-- лента Мёбиуса, полученная одновременным вращением отрезка
-- длины 7 вокруг нескольких осей:
moebius :: Figure (PGA3) ()
moebius = mapM_ (<@ [opacity_ "0.5", stroke_ "blue"]) $
  [ polyline $
    rotateAt e13 (2*α) .
    shiftAlong' e23 7 .
    rotateAt e12 α <$> [point [], point [7]] 
  | α <- [pi/300, pi/150 .. 2*pi]]

fig :: Animation ()
fig = animate 40 (0,2*pi) $ \a -> 
 -- проекция из указанной точки на плоскость
  viewPoint [0,0,50] $ do
  background "black"
  -- поворот ленты вокруг оси [1,1,0]
  mapFig (rotateAt (e23 + e13) a) moebius

main = runAnimation "moebius.gif" fig

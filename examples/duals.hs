{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry
import Algeometry.SVG
import Lucid.Svg
import Data.Text (pack)
import Text.Printf
import Data.List (inits)

duals :: Animation ()
duals = animate 40 (0,2*pi) frame
  where
    n = 2000
    col i = pack $
            printf "rgb(%.2f%%,50%%,%.2f%%)"
            (100*cos(i)**2) (100*sin(i)**2)
    frame x = do
      sequence [ dual p @ [opacity_ "0.1", stroke_ (col i)]
               | (p,i) <- zip pts [0,2*pi/n..] ]
      polyline pts <@ [ stroke_ "orange"
                      , opacity_ "1"
                      , stroke_width_ "2"]
        where
          pts = [point [rx*sin (2*a),ry*cos (2*a)]
                | a <- [0,2*pi/n..2*pi]
                , let rx = 1+0.75*sin(7*a+x)
                , let ry = 1+0.75*sin(7*a+2*x) ]


dualcurves = animateList $ mkFrame <$> tail (inits pts)
  where
    n = 100
    --curve t = [(1 - 0.99*cos (t))*sin (t-pi/2), (1 - 0.99*cos (t))*cos (t-pi/2)]
    curve t = [0.35 + 0.8*cos (t), 0.5*sin (t)]
    pts = [ point (curve (2*pi*t/n)) | t <- [0 .. n-1] ]
    mkFrame ps = mapFig (rescale 10) $ do
--      regularPoly 100 <@ [stroke_ "gray"]
      polyline ps <@ []
      polyline ((\x -> e12 ->| lcompl x) <$> ps) <@ [stroke_ "blue"]
      put $ last ps
      l <- lcompl (last ps) @ [stroke_ "blue", opacity_ "0.7"]
      segment e12 ((e12 `inner` l)*l) <@ [stroke_ "red"]

main = runAnimation "duals" duals

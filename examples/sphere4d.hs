{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry
import Algeometry.SVG
import Control.Monad.Random hiding (join)
import Data.Coerce


sphere r θ1 θ2 ϕ =
  point [ r * cos θ1
        , r * sin θ1 * cos θ2
        , r * sin θ1 * sin θ2 * cos ϕ
        , r * sin θ1 * sin θ2 * sin ϕ] :: PGA4

proj x = coerce (projectionOf x (e 4) * (e 4))

sphere4d v = animate 51 (-pi,pi) frame
  where
    frame a = let
      m = 15
      ls = [ polyline $
             [ rot $ sphere (17*cos a) (t) (m*t+2*pi/m*i) (5*t)
             | t <- [0,pi/300..pi]]
           | i <- [0..2*m-1]]
      rot = proj . rotateAt (nvect v) a
      in viewPoint [0,0,500] $ 
         mapM_ (<@ [stroke_ "orangered", opacity_ "0.35"]) ls

main :: IO ()
main = do
  mapM_ mkAn $ reverse $ drop 2 $ (++[1]) <$> replicateM 3 [0,1]
  print "Ok"
  where
    mkAn v = runAnimation (foldMap (show . round) v ++ ".gif")
             (sphere4d v)

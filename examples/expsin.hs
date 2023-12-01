{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry
import Algeometry.SVG
import Lucid.Svg

fig = mapFig (\x -> nonScalar (exp x)) grid

main = writeSVG "test2.svg" fig

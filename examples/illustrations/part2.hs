{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry

main = writeSVG "fig2.1.svg" $ do
  _A <- point [40, 100]   @ [id_ "A"]
  _B <- point [250, 50]   @ [id_ "B"]
  _C <- point [200, 220]  @ [id_ "C"]
  polygon [_A, _B, _C]  <@  [fill_ "wheat"]
  a <- _A ∨ (_B + _C)/2  @ [id_ "A∨(B+C)/2"]
  b <- _B ∨ (_C + _A)/2  @ [id_ "B∨(C+A)/2"]
  c <- _C ∨ (_A + _B)/2  @ [id_ "C∨(A+B)/2"]
  _A + _B + _C            @ [id_ "M"]

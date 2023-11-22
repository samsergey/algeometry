{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry
import Algeometry.SVG
import Lucid.Svg

menelaus :: Figure PGA2 PGA2
menelaus = do
  a <- point [-5, 1]      @ [id_ "A"]
  b <- point [7, 7]       @ [id_ "B"]
  c <- point [2, -7]      @ [id_ "C"]
  d <- line [1,-2] [0,-2] @ [id_ "d"]
  ab <- a ∨ b            @ [id_ "c"]
  ac <- a ∨ c            @ [id_ "b"]
  bc <- b ∨ c            @ [id_ "a"]
  a' <- bc ∧ d           @ [id_ "A'"]
  b' <- ac ∧ d           @ [id_ "B'"]
  c' <- ab ∧ d           @ [id_ "C'"]
  let r = (a∙b')/(b'∙c)*(c∙a')/(a'∙b)*(b∙c')/(c'∙a)
  display [10,-10] $ "Result: " <> show r

ceva :: Figure PGA2 PGA2
ceva = mapFig dual menelaus

main = do
  writeSVG "menelaus.svg" menelaus
  writeSVG "ceva.svg" ceva
  

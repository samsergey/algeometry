{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry.Base
import Algeometry.SVG
import Lucid.Svg

fig :: [Fig]
fig = figure $ do
  _A  <- point [40, 100]          @ [id_ "A"]
  _B  <- point [250, 50]          @ [id_ "B"]
  _C  <- point [150, 200]         @ [id_ "C"]
  polygon [_A, _B, _C]              [fill_ "wheat"]
  a   <- (_B ∨ _C) ∙ _A           @ [stroke_ "navy"]
  b   <- (_A ∨ _C) ∙ _B           @ [stroke_ "navy"]
  _O  <- a ∧ b                    @ [id_ "O"]
  a'  <- (_B ∨ _C) ∙ (_B + _C)/2  @ [stroke_ "gray"]
  b'  <- (_A ∨ _C) ∙ (_A + _C)/2  @ [stroke_ "gray"]
  _O' <- a' ∧ b'                  @ [id_ "O'"]
  _O ∨ _O'                        @ [stroke_ "red"]
  (_A + _B + _C)/3                @ [id_ "O''"]

main :: IO ()
main = print  $ svg fig

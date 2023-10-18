{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry.Base
import Algeometry.SVG
import Lucid.Svg

fig :: [Fig (PGA 2)]
fig = figure $ do
  _A  <- point [40, 100]          @ [id_ "A"]
  _B  <- point [250, 50]          @ [id_ "B"]
  _C  <- point [150, 200]         @ [id_ "C"]
  polygon [_A, _B, _C]              [fill_ "wheat"]
  a   <- (_B ∨ _C) ∙ _A           @ [id_ "a", stroke_ "navy"]
  b   <- (_A ∨ _C) ∙ _B           @ [id_ "b", stroke_ "navy"]
  _O  <- a ∧ b                    @ [id_ "O"]
  a'  <- (_B ∨ _C) ∙ (_B + _C)/2  @ [id_ "a'", stroke_ "gray"]
  b'  <- (_A ∨ _C) ∙ (_A + _C)/2  @ [id_ "b'", stroke_ "gray"]
  _O' <- a' ∧ b'                  @ [id_ "O'"]
  _   <- _O' ∨ _O                @ [id_ "e", stroke_ "red"]
  (_A + _B + _C)/3                @ [id_ "O''"]

fig3 :: ([Fig (PGA 3)], PGA 3)
fig3 = do
  _A  <- point [40, 100,10]          @ [id_ "A"]
  _B  <- point [250, 50,20]          @ [id_ "B"]
  _C  <- point [150, 200,50]         @ [id_ "C"]
  polygon [_A, _B, _C]              [fill_ "wheat"]
  a   <- (_B ∨ _C) ∙ _A           @ [id_ "a", stroke_ "navy"]
  b   <- (_A ∨ _C) ∙ _B           @ [id_ "b", stroke_ "navy"]
  _O  <- a ∧ b                    @ [id_ "O"]
  a'  <- (_B ∨ _C) ∙ (_B + _C)/2  @ [id_ "a'", stroke_ "gray"]
  b'  <- (_A ∨ _C) ∙ (_A + _C)/2  @ [id_ "b'", stroke_ "gray"]
  _O' <- a' ∧ b'                  @ [id_ "O'"]
  e'  <- _O' ∨ _O                @ [id_ "e'", stroke_ "red"]
  e   <- e' ∧ (_A ∨ _B ∨ _C)    @ [id_ "e", stroke_ "red"] 
  (_A + _B + _C)/3                @ [id_ "O''"]
  return e


main :: IO ()
main = print  $ svg fig

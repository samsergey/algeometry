{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry

fig1 = do
  _A  <- point [40, 100]          @ [id_ "A"]
  _B  <- point [250, 50]          @ [id_ "B"]
  _C  <- point [150, 200]         @ [id_ "C"]
  polygon [_A, _B, _C]            <@ [fill_ "wheat"]
  a   <- (_B ∨ _C) ∙ _A           @ [id_ "a", stroke_ "navy"]
  b   <- (_A ∨ _C) ∙ _B           @ [id_ "b", stroke_ "navy"]
  _O  <- a ∧ b                    @ [id_ "O"]
  a'  <- (_B ∨ _C) ∙ (_B + _C)/2  @ [id_ "a'", stroke_ "gray"]
  b'  <- (_A ∨ _C) ∙ (_A + _C)/2  @ [id_ "b'", stroke_ "gray"]
  _O' <- a' ∧ b'                  @ [id_ "O'"]
  _   <- _O' ∨ _O                 @ [id_ "e", stroke_ "red"]
  (_A + _B + _C)/3                 @ [id_ "O''"]


fig2 = animate 20 (0, 2*pi) $ \a ->
  viewPoint [0,0,500] $
  mapFig (rotateAt (line [150,150] [150,151]) a) $
  do
    _A   <- point [40, 100,100]       @ [id_ "A"]
    _B   <- point [250, 50,10]        @ [id_ "B"]
    _C   <- point [150, 200,130]      @ [id_ "C"]
    polygon [_A, _B, _C]             <@ [fill_ "wheat"]
    let
      _ABC = _A ∨ _B ∨ _C
      a    = (_B ∨ _C) ∙ _A
      b    = (_A ∨ _C) ∙ _B
      a'   = (_B ∨ _C) ∙ (_B + _C)/2
      b'   = (_A ∨ _C) ∙ (_A + _C)/2
    ab   <- a ∧ b                     @ [id_ "a∧b"]
    ab'  <- a' ∧ b'                   @ [id_ "a'∧b'"]
    _O   <- ab ∧ _ABC                 @ [id_ "O"]
    _O'  <- ab' ∧ _ABC                @ [id_ "O'"]
    e <- _O ∨ _O'                     @ [id_ "e", stroke_ "red"]
    (_A + _B + _C)/3                   @ [id_ "O''"]

main = do
  writeSVG "fig1.svg" fig1
  runAnimation "fig2.svg" fig2

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry.PGA
import Algeometry.SVG
import Lucid.Svg hiding (scale)

fig :: [Fig]
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
  _   <- _O' ∨ _O                 @ [id_ "e", stroke_ "red"]
  (_A + _B + _C)/3                 @ [id_ "O''"]

fig3 :: Double -> [Fig]
fig3 a = figureWith (viewPoint [0,0,500] . rotateAt ox a) (f, ())
  where
    (f, ox) = do
      _A   <- point [40, 100,100]       @ [id_ "A"]
      _B   <- point [250, 50,10]        @ [id_ "B"]
      _C   <- point [150, 200,130]      @ [id_ "C"]
      polygon [_A, _B, _C]              [fill_ "wheat"]
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
      return (line [150,150,0] [150,151,0])

test = rotateAbout o (viewPoint [0,0,2000]) (fig, ())
  where
    (fig, o) = do
      p <- point [120, 100,100]          @ [id_ "p"]
      l <- line [50,150,20] [200,200,50] @ [id_ "l"]
      plane p l (30, 10)                   [fill_ "wheat", stroke_ "none"]
      orthoPlane p l (30,10)               [fill_ "lightgreen", stroke_ "none"]
      return $ p `join` (projectionOf p `on` l)

--    o = line [150,150,0] [150,151,0]
  
main :: IO ()
main = do
  --writeSVG "test1.svg" (fig3 0)
  animate 40 (0, 2*pi) test "an.gif" 

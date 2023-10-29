{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Algeometry
import Lucid.Svg hiding (scale)

-- fig1 :: [Fig]
-- fig1 = figure $ do
--   _A  <- point [40, 100]          @ [id_ "A"]
--   _B  <- point [250, 50]          @ [id_ "B"]
--   _C  <- point [150, 200]         @ [id_ "C"]
--   polygon [_A, _B, _C]              [fill_ "wheat"]
--   a   <- (_B ∨ _C) ∙ _A           @ [id_ "a", stroke_ "navy"]
--   b   <- (_A ∨ _C) ∙ _B           @ [id_ "b", stroke_ "navy"]
--   _O  <- a ∧ b                    @ [id_ "O"]
--   a'  <- (_B ∨ _C) ∙ (_B + _C)/2  @ [id_ "a'", stroke_ "gray"]
--   b'  <- (_A ∨ _C) ∙ (_A + _C)/2  @ [id_ "b'", stroke_ "gray"]
--   _O' <- a' ∧ b'                  @ [id_ "O'"]
--   _   <- _O' ∨ _O                 @ [id_ "e", stroke_ "red"]
--   (_A + _B + _C)/3                 @ [id_ "O''"]

-- fig3 :: Double -> [Fig]
-- fig3 a = figureWith (viewPoint [0,0,500] . rotateAt ox a) (f, ())
--   where
--     (f, ox) = do
--       _A   <- point [40, 100,100]       @ [id_ "A"]
--       _B   <- point [250, 50,10]        @ [id_ "B"]
--       _C   <- point [150, 200,130]      @ [id_ "C"]
--       polygon [_A, _B, _C]              [fill_ "wheat"]
--       let
--         _ABC = _A ∨ _B ∨ _C
--         a    = (_B ∨ _C) ∙ _A
--         b    = (_A ∨ _C) ∙ _B
--         a'   = (_B ∨ _C) ∙ (_B + _C)/2
--         b'   = (_A ∨ _C) ∙ (_A + _C)/2
--       ab   <- a ∧ b                     @ [id_ "a∧b"]
--       ab'  <- a' ∧ b'                   @ [id_ "a'∧b'"]
--       _O   <- ab ∧ _ABC                 @ [id_ "O"]
--       _O'  <- ab' ∧ _ABC                @ [id_ "O'"]
--       e <- _O ∨ _O'                     @ [id_ "e", stroke_ "red"]
--       (_A + _B + _C)/3                   @ [id_ "O''"]
--       return (line [150,150,0] [150,151,0])

-- test = rotateAbout o (viewPoint [0,0,2000]) (fig, ())
--   where
--     (fig, o) = do
--       p <- point [120, 100,100]          @ [id_ "p"]
--       l <- line [50,150,20] [200,200,50] @ [id_ "l"]
--       plane p l (30, 10)                   [fill_ "wheat", stroke_ "none"]
--       orthoPlane p l (30,10)               [fill_ "lightgreen", stroke_ "none"]
--       return $ p `join` (projectionOf p `on` l)

-- --    o = line [150,150,0] [150,151,0]

-- --fig2 :: [Fig]
-- fig2 =
--   --figure $
--   do
--   _A <- (point [40, 100] :: PGA 2)      @ []
--   _B <- point [250, 50]        @ []
--   _C <- point [150, 200]      @ []
--   a  <- _B `join` _C   @ [id_ "a"]
--   b  <- _C `join` _A   @ [id_ "b"]
--   c  <- _A `join` _B   @ [id_ "c"]
--   a' <- c - b                    @ [id_ "a'"]
--   b' <- c - a                    @ [id_ "b'"]
--   c' <- a - b                    @ [id_ "c'"]
-- --  (c - b) ∧ (c - a)                     @ [id_ "O"]
--   --_O <- b∧a - c∧a - b∧c                      @ [id_ "O"]
--   b∧a                     @ [id_ "C"]
--   c∧a                     @ [id_ "B"]
--   b∧c                     @ [id_ "A"]
--   o <- (_C - _B - _A)      @ [id_ "O"]
--   return (o)

-- fig4 = figure $ do
--   _A <- point [40, 100]   @ [id_ "A"]
--   _B <- point [250, 50]   @ [id_ "B"]
--   _C <- point [200, 220]  @ [id_ "C"]
--   polygon [_A, _B, _C]      [fill_ "wheat"]
--   a <- _A ∨ (_B + _C)/2  @ [id_ "A∨(B+C)/2"]
--   b <- _B ∨ (_C + _A)/2  @ [id_ "B∨(C+A)/2"]
--   c <- _C ∨ (_A + _B)/2  @ [id_ "C∨(A+B)/2"]
--   _A + _B + _C            @ [id_ "M"]

-- fig5 an =
--   --figure $
--   figureWith (viewPoint [0,0,1000]. rotateAt (line [] [0,1,0]) an) $
--   do
--   axis
--   a <- (point []) @ []
--   b <- point [2,1,-1] @ [id_ "B"]
--   c <- point [1,-2,1] @ [id_ "C"]
--   plane a (b ∨ c) (1, 1)  [fill_ "wheat", stroke_ "none"]
--   blade (b ∨ c) [stroke_width_ "2", stroke_ "red"]
--   let p = a ∨ b ∨ c
--   (projectionOf (line [] [1]) p) @ []
--   (projectionOf (line [] [0,1]) p) @ []
  

-- blade b attr
--   | isPoint b = b @ attr
--   | isLine b = let p0 = projectionOf (point []) b
--                    p1 = shiftAlong b p0
--                in do segment p0 p1 attr
--                      return b
--   | isPlane b = return b 


-- поворот на угол θ вокруг p
rot θ p = scalar (cos (θ/2)) + scalar (sin (θ/2)) * p

-- смещение на вектор v
mov v = (pseudoScalar + scale (4/norm2 v) v)^2

-- лента Мёбиуса, полученная одновременным вращением отрезка
-- длины 7 вокруг нескольких осей:
moebius :: (Num a, Num e, GeomAlgebra e a) => [[a]]
moebius = [ rot (2*α) (e_[1,3]) $$
            mov (7*e_[2,3]) $$
            rot α (e_[1,2]) $$ vect [7,0,0]
          | α <- [pi/1000, pi/500 .. 2*pi]]

fig β =
  figure $
  mapM_ (`polygon` [opacity_ "0.5", stroke_ "blue"]) $
  -- проекция из указанной точки на плоскость
  viewFrom [0,0,50] $
  -- поворот ленты вокруг оси [1,1,0]
  [rot β (e_[2,3] + e_[1,3]) $$ s | s <- moebius]

infixr $$
($$) :: (Num a, GeomAlgebra e a) => a -> [a] -> [a]
t $$ xs = [t * x * rev t | x <- xs]
  
main :: IO ()
main = do
--  writeSVG "test1.svg" (fig3 0)
  animate 20 (0, 2*pi) fig "an.gif"
  print "Ok"

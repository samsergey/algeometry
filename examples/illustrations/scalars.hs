{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry

$(defineElements (basis :: [PGA3]))

innerFig1 = viewPoint [0,0,500] $ do
  let pt = point [1,2,-2]
      l = e123 `join` pt
      p = normalize $ e123 ∨ pt ∨ (point [1,1/2])
      l' = rotateAt ( e123 `inner` p) (pi/2) l
  plane2 pt l' (15,15) <@ [fill_ "wheat", id_ "p"]
  l @ [id_ "a"]
  l' @ [id_ "a∙p", stroke_ "blue"]
  label [9,7] "p" 


innerFig2 a = viewPoint [0,0,200] $
  mapFig (rotateAt e13 a . rotateAt e23 (-1.4)) $ do
  point [] @ []
  let l = nvect [1,1,2]
  p <- plane3 (point []) (point [1]) (point [0,1]) (12,12) <@ [fill_ "wheat"]
  let lp = l `inner` p
  pl <- plane2 (shiftAlong l (point [])) (lp `meet` p) (12,12) <@ [fill_ "pink"]
  l @ [id_ "a"]
  pl `meet` p @ []
  label [10,-5,0] "p"
  label [0,0,10] "a∙p"  

innerFig3 :: Figure PGA2 PGA2
innerFig3 = do
  p <- point [-1,-6] @ [id_ "A"]
  l <- nline [0,3] [-5,0] @ [id_ "a"]
  l `inner` p @ [id_ "A∙a", stroke_ "blue"]


innerFig4 a = viewPoint [0,0,200] $
  mapFig (rotateAt e13 a . rotateAt e23 (-1.2)) $ do
  p <- point [-1,-6] @ [id_ "A"]
  let l = nline [0,3] [-5,0]
  pl <- orthoPlane p l (15,15) <@ [fill_ "wheat"]
  l  @ [id_ "a"]
  l `meet` (l `inner` p) @ []
  label [-5,0,10] "A∙a"

innerFig5 a = viewPoint [0,0,100] $
  mapFig (rotateAt e12 a . rotateAt e23 (-1.2)) $ do
  p <- point [1,3,10] @ [id_ "A"]
  pl <- plane3 (point []) (point [1]) (point [0,1]) (12,12) <@ [fill_ "wheat"]
  l <- p `inner` pl @ []
  l `meet` pl @ []
  label [-5,0,10] "A∙a"


proj1 :: Figure PGA2 PGA2
proj1 = do
  p <- point [-3,5] @ [id_ "A"]
  l <- line [0,-5] [7,0] @ [id_ "a"]
  (p `inner` l) * l @ [fill_ "red"]
  label [4,-6] "(A∙a)A"
  (p `inner` l) * p @ [stroke_ "blue"]
  label [10,16] "(A∙a)A"

proj2 a = viewPoint [0,0,100] $
  mapFig (rotateAt e12 a . rotateAt e23 (-1.2)) $ do
  p <- point [1,3,7] @ [id_ "A"]
  pl <- plane3 (point [0,0,-10]) (point [1,0,-10]) (point [0,1,-10]) (12,12) <@ [fill_ "wheat"]
  label [-10,-10,-10] "p"
  l <- p `inner` pl @ []
  p' <- l * pl @ [id_ "(A∙p)p"]
  plane3 (point [0,0,7]) (point [1,0,7]) (point [0,1,7]) (12,12) <@ [fill_ "lightblue"]
  label [4,-6,-16] "(A∙p)A"

proj3 a = viewPoint [0,0,200] $
  mapFig (rotateAt e13 a . rotateAt e23 (-1.4)) $ do
  point [] @ []
  let l = nvect [-1,1,2]
  p <- plane3 (point []) (point [1]) (point [0,1]) (12,12) <@ [fill_ "wheat"]
  let lp = l `inner` p
  l @ [id_ "a"]
  l' <- lp*p @ [id_"(a∙p)p", stroke_ "blue"]
  let t x = l'*l*x*rev l*rev l'
  plane3 e123 (t $ point [1]) (t $ point [0,1]) (12,12) <@ [fill_ "lightblue"]
  label [10,-5,0] "p"
  label [0,0,10] "(a∙p)a"  

main = print "Ok"

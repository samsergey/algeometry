{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main (main) where

import Algeometry

$(defineElements (basis :: [PGA3]))

multan :: PGA2 -> Animation ()
multan el = do
  transform 20 $ fig (lerp 1 el)
  transform 20 $ fig (lerp el (el*el))
  where
  pts = [[-5,-5],[-5,7],[10,7]]
  getCoefs x = [coeff [] x, coeff (head (elems el)) x]
  fig f t = do
    let pt [x, y] = getCoefs $  (f t) * (scalar x + scalar y*el)
    axis
    sequence [ do segm (pt [x,-16]) (pt [x,16]) <@ [stroke_ "blue"]
                  segm (pt [-16,x]) (pt [16,x]) <@ [stroke_ "blue"]
             | x <- [-16,-14..16] ]
    polygon (fromXY . pt <$> pts) <@ [fill_ "wheat", stroke_ "red"]


multan2 :: Animation ()

multan2 = transform 30 $ fig (lerp 1 (b*a))
  where
    a = nvect [2,1]
    b = nvect [1,3]
    pts = [[-4,-2],[-4,4],[8,4]]
    fig f t = do
      axis
      grid
      a @ [id_ "a"]
      b @ [id_ "b"]
      sequence
        [ do mapFig (f t *) (segm [x,-16] [x,16]) <@ [stroke_ "blue"]
             mapFig (f t *) (segm [-16,x] [16,x]) <@ [stroke_ "blue"]
        | x <- [-16,-14..16] ]
      polygon [(f t)*point pt | pt <- pts] <@ [fill_ "wheat", stroke_ "red"]


multan3 = do
  transform 20 $ fig (0,pi/4)
  transform 20 $ fig (pi/4,pi/2)
  transform 20 $ fig (pi/2,3*pi/4-0.001)
  transform 20 $ fig (3*pi/4-0.001,pi)
  where
    pts = [[-4,-2],[-4,4],[8,4]]
    fig (a1, a2) t = do
      let a = rotateAt e12 (a1 + (a2 - a1)*t) $ vect [1]
      let f x = a * x / a
      axis
      grid
      polygon [(point pt) | pt <- pts] <@ [fill_ "wheat", stroke_ "red", opacity_ "1"]
      a @ [id_ "a"]
      sequence
        [ do mapFig f (segm [x,-16] [x,16]) <@ [stroke_ "blue"]
             mapFig f (segm [-16,x] [16,x]) <@ [stroke_ "blue"]
        | x <- [-16,-14..16] ]
      polygon [f (point pt) | pt <- pts] <@ [fill_ "wheat", stroke_ "red"]
-- exp' a x = scalar (cos a) + scale (sin a) x




projAn = do
  transform 20 $ proj $ \t -> (3+5*sin (2*t), 3+5*cos (2*t))
  transform 20 $ proj $ \t -> (3+5*sin (2-t), 3+5*(1-t)*cos 2)
  transform 30 $ proj $ \t -> (3+5*sin (1+3*t), 3+10*t*cos (3*t))
  transform 20 $ proj $ \t -> (lerp (3+5*sin (1+3)) 3 t, lerp (3+10*cos (3)) 8 t)
  where
    proj f t = viewPoint [0,0,50] $
      mapFig (rotateAt (vect [1]) (-1.2) ) $ do
      let (x,y) = f t
      pt <- point [x,y,0] @ [id_ "A"]
      p <- plane2 (point [0,0,0]) (line [1,0,0] [1,1,0]) (25,25) <@ [fill_ "wheat"]
      o <- point [0,0,-7] @ [id_ "O"]
      l <- o `join` pt  @ [stroke_ "blue"]
      pt' <- (l ∙ o) `meet` p @ [id_ "a"]
      plane2 o pt' (25,5) <@ [fill_ "blue", opacity_ "0.25"]
  
main  = print "Ok"

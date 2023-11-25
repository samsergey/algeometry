{-|
Module      : Algeometry.SVG
Description : Quick and dirty graphic backend for geometric algebra.
Stability   : experimental
-}
{-# LANGUAGE UndecidableInstances
, LambdaCase
, DerivingVia
, StandaloneDeriving
, DataKinds
, KindSignatures
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, FlexibleInstances
, OverloadedStrings
, FunctionalDependencies
, BangPatterns #-}

module Algeometry.SVG
  ( -- * Types
    Fig (..)
  , Figure
  , figure
  , getFigure
  , getResult
  , mapFig
  , Animation

  -- * SVG generation and output
  , put
  , (@)
  , (<@)
  , svg
  , writeSVG
  , writePNG

  -- * Simple animation
  , writeFrame
  , runAnimation
  , animate
  , transform
  , animateList

  -- * Constructors
  , origin
  , point
  , line
  , nline
  , vect
  , nvect
  , segm
  , polygon
  , polyline
  , regularPoly
  , segment
  , plane
  , plane2
  , plane3
  , orthoPlane
  , label
  , display
  
  -- * Miscellaneous tools
  , viewPoint
  , axis
  , grid
  , background
  ) where

import Algeometry
import Control.Monad hiding (join)
import Data.List (sort, nub, union)
import Data.Maybe
import Data.Text (pack)
import Lucid.Base (HtmlT)
import Lucid.Svg hiding (scale, Term)
import System.IO
import System.Process
import Data.Coerce

------------------------------------------------------------

instance With [Attribute] where
  with as bs = foldl add as bs
    where
      add res att@(Attribute a v) = case res of
        Attribute b _ : r | a == b -> att : r
        t -> att : t

------------------------------------------------------------

type XY = (Double, Double)

-- | Represents graphic primitives
data Fig = Point   [Attribute] XY
         | Line    [Attribute] XY XY
         | Polygon [Attribute] [XY]
  deriving (Show, Eq)

instance Ord Fig where
  compare (Point _ _) _ = GT
  compare _ (Point _ _) = LT
  compare _ _ = EQ

------------------------------------------------------------

clip :: PGA2 -> [PGA2]
clip mv = let
  vs = point <$> [[-20,-20],[-20,20],[20,20],[20,-20],[-20,-20]]
  frame = zip vs (tail vs)
  in mapMaybe (segmentMeet mv) frame

------------------------------------------------------------

-- | Representation of a figure with many graphic objects with attributes.
newtype Figure a b = Figure ([([a], [Attribute])], b)
  deriving (Show, Functor, Applicative, Monad)

instance Eq a => Semigroup (Figure a b) where
  Figure (!f1, b) <> Figure (!f2, _) = Figure (f1 `union` f2, b)

instance Eq a => Monoid (Figure a b) where
  mempty = Figure (mempty, undefined)

instance With (Figure a b) where
  with (Figure !([(a, attr)], b)) as = 
        Figure ([(a, attr `with` as)], b)
    
-- | Returns list of graphic objects with attributes.
getFigure :: Figure a b -> [([a], [Attribute])]
getFigure (Figure p) = fst p

-- | Returns value, stored in Figure type.
getResult :: Figure a b -> b
getResult (Figure p) = snd p

-- | Applies transformation to objects, stored in Figure type.
mapFig :: (a1 -> a2) -> Figure a1 b -> Figure a2 b
mapFig f (Figure !(s, r)) = Figure (map (\(x, a) -> (map f x, a)) s, r)

-- | Adds multivector as graphic object to Figure.
put :: a -> Figure a a
put x = Figure ([([x], [])], x)

-- | Adds multivectors as graphic object to Figure.
puts :: [a] -> Figure a [a]
puts x = Figure ([(x, [])], x)

infix 1 @
-- | Adds multivector as graphic object with given attributes.
(@) :: a -> [Attribute] -> Figure a a
a @ attr = put a `with` attr

infix 1 <@
-- | Appends attributes to attributed object.
(<@) :: Figure a b -> [Attribute] -> Figure a b
(<@) = with

------------------------------
-- geometric objects

-- | Returns multivector, which represents the origin.
origin :: GeomAlgebra b a => a
origin = point []

-- | Returns a label with given value.
display :: (Show s,  GeomAlgebra b a) => [Double] -> s -> Figure a a
display p s = label p $ show s

-- | Returns a label at given position.
label :: (GeomAlgebra b a) => [Double] -> String -> Figure a a
label p s = point p @ [ id_ (pack s), class_ "label"]

-- | Returns multivector, which represents a normalized line.
nline :: GeomAlgebra b a => [Double] -> [Double] -> a
nline a b = point a `join` point b

-- | Returns multivector, which represents a line.
line :: GeomAlgebra b a => [Double] -> [Double] -> a
line a b = point a ∨ point b

-- | Returns multivector, which represents a vector (line passing through the origin).
vect :: GeomAlgebra b a => [Double] -> a
vect = line []

-- | Returns multivector, which represents a normalized vector (line passing through the origin).
nvect :: GeomAlgebra b a => [Double] -> a
nvect = nline []

-- | Returns a graphic object, which represents a polygon.
polygon :: GeomAlgebra e a => [a] -> Figure a [a]
polygon pts = res `with` [class_ "polygon"]
  where
    res = case pts of
      h:_:_ | isPoint h -> puts pts
            | isLine h ->
              let pts' = zipWith outer pts (tail pts ++ [h])
              in const pts <$> puts pts'
      _ -> puts pts

-- | Returns a graphic object, which represents a polyline.
polyline :: GeomAlgebra e a => [a] -> Figure a [a]
polyline pts = polygon pts `with` [class_ "polyline"]

-- | Returns a graphic object, which represents a regular polygon.
regularPoly :: GeomAlgebra e a => Double -> Figure a [a]
regularPoly n = polygon [ point ([cos (2*pi*t), sin (2*pi*t)])
                        | t <- [0, 1/n .. n-1/n] ]

-- | Returns a graphic object, which represents a segment? given by two points.
segment :: GeomAlgebra e a => a -> a -> Figure a [a]
segment p1 p2 = polyline [p1, p2]

-- | Returns a graphic object, which represents a segment given by coordinates.
segm
  :: GeomAlgebra b a
  => [Double] -> [Double] -> Figure a [a]
segm a b = segment (point a) (point b)

-- | Returns multivector, which represents a plane passing through three points.
plane :: GeomAlgebra b a
      => [Double] -> [Double] -> [Double] -> a
plane a b c = point a `join` point b `join` point c

-- | Returns graphic object, which represents a plane passing through a point and a line.
plane2
  :: (Fractional a, GeomAlgebra e a)
  => a -> a -> (Double, Double)
  -> Figure a a
plane2 p l (a, b) = do
  polygon [ shiftAlong' l' (b) $ shiftAlong' l (a) p
          , shiftAlong' l' (b) $ shiftAlong' l (-a) p 
          , shiftAlong' l' (-b) $ shiftAlong' l (-a) p'
          , shiftAlong' l' (-b) $ shiftAlong' l (a) p' ] 
  return (p ∨ l)
  where
    p' = p ->| l 
    l' = p `join` p'

-- | Returns graphic object, which represents a plane passing through a point, orthogonal to a line.
orthoPlane
  :: (Fractional a, GeomAlgebra e a)
  => a -> a -> (Double, Double) 
  -> Figure a a
orthoPlane p o = plane2 p l
  where
    p' = p ->| o 
    l = p' `inner` (p `join` o)

-- | Returns graphic object, which represents a plane passing through three points.
plane3
  :: (Fractional a, GeomAlgebra e a)
  => a -> a -> a -> (Double, Double)
  -> Figure a a
plane3 p1 p2 p3 = plane2 p1 (p2 `join` p3)

-- rplane
--   :: (Fractional a, GeomAlgebra e a)
--   => a -> Double -> String -> String
--   -> Figure a a
-- rplane p r col lab = do
--   polygon pts attr'
--   sequence [ segment p0 pt attr' | pt <- pts ]
--   if not (null lab)
--     then p2 @ [stroke_width_ "0", fill_ "none", id_ (pack lab)]
--     else return p
--   return p
--   where
--     p0 = point [] ->| p
--     p1 = point [log 2, sqrt 2, sin 2] ->| p
--     p2 = shiftAlong' (p0 `join` p1) r p0
--     n = point [] |- p
--     pts = [ rotateAt n a p2 | a <- [0,pi/20..2*pi] ]
--     attr' = [fill_ (pack col), stroke_ (pack col), opacity_ "0.25"]

-- | Adds 2D axes to a Figure
axis :: Figure PGA2 PGA2
axis = do
  line [-20,0] [20,0] @ [stroke_width_ "0.25"]
  line [0,-20] [0,20] @ [stroke_width_ "0.25"]

-- | Adds 2D unit grid to a Figure
grid :: Figure PGA2 ()
grid = do
  line [] [0,1] @ attr
  line [] [1] @ attr
  mapM_ mk [1..19]
  where
    mk x = do
      line [x] [x,1] @ attr
      line [0,x] [1,x] @ attr
      line [-x] [-x,1] @ attr
      line [0,-x] [1,-x] @ attr
    attr = [stroke_ "lightgray", stroke_width_ "0.2"]

-- | Adds 2D background rectangle to a Figure
background :: GeomAlgebra e a => String -> Figure a [a]
background col = polygon [] `with` [ class_ "bg", fill_ (pack col)] 
    
------------------------------------------------------------
-- | Returns a figure as a list of primitives.
figure :: Figure PGA2 b -> [Fig]
figure = foldMap draw . getFigure
  where
    draw (mvs, attr) = case mvs of
      [mv] | isPoint mv -> [Point attr $ xy mv]
           | isLine mv -> case nub (xy <$> clip mv) of
               [] -> []
               [p] -> [Point attr p]
               p1:p2:_ -> [Line attr p1 p2]
           | otherwise -> []
      _ | all isPoint mvs -> [Polygon attr $ xy <$> mvs]
        | otherwise -> mvs >>= \v -> draw ([v], attr)
      
    xy mv = case coord mv of
      [] -> (0,0)
      [x] -> (x,0)
      x:y:_ -> (x,y)

-- | Projects 3D points to a 2D plane, using given viewpoint.
viewPoint :: [Double] -> Figure PGA3 b -> Figure PGA2 b
viewPoint vp = mapFig (coerce . project)
  where
    p0 = dual (point vp)
    project x
      | isPoint x = -((x `join` point vp) `meet` p0 ) * p0
      | isLine x = -((x `join` point vp) `meet` p0 ) * p0
      | otherwise =  -((x `join` point vp) `meet` p0 ) * p0

------------------------------------------------------------

rescaleTo (w,h) f = case f of
  Point attr p -> Point attr (sc p)
  Line attr a b -> Line attr (sc a) (sc b)
  Polygon attr ps -> Polygon attr (sc <$> ps)
  where
    sc (x,y) = (w/2 + x/20*w/2, h/2 - y/20*h/2)

renderFig :: Monad m => Fig -> HtmlT m ()
renderFig f = case f of
  Point attr (x, y) -> do
    case getClass attr of
      ["label"] -> mempty
      _ ->
        circle_ $ [ cx_ (pack (show x))
                  , cy_ (pack (show y))
                  , r_ "2.25"
                  , fill_ "black"
                  , stroke_ "white"] `with` attr
    case getId attr of
      [s] -> mkLabel s (x+6, y-6)
      _ -> mempty
      
  Line attr a b -> do
    case getClass attr of
      ["label"] -> mempty
      _ ->
        polyline_ $ [ points_ (pack $ foldMap toStr [a, b])] `with` attr
    case getId attr of
      [s] -> do
        let (x1, y1) = a
        let (x2, y2) = b
        let n = sqrt((x1-x2)**2 + (y1-y2)**2)
        mkLabel s (x2 + 20/n*(x1 - x2), y2 + 20/n*(y1 - y2))
      _ -> mempty
 
  Polygon attr pts ->
    case getClass attr of
      ["polygon"]
        -> polygon_ $ [ points_ (pack $ foldMap toStr pts)] `with` attr
      ["polyline"]
        -> polyline_ $ [ points_ (pack $ foldMap toStr pts)] `with` attr
      ["bg"]
        -> rect_ $ [ width_ "100%", height_ "100%"
                   , stroke_ "none", fill_opacity_ "1"] `with` attr
      _ -> mempty

  where
    toStr (x,y) = show x <> "," <> show y <> " "
    
    getId = mapMaybe $ \case
      Attribute "id" x -> Just x
      _ -> Nothing

    getClass = mapMaybe $ \case
      Attribute "class" x -> Just x
      _ -> Nothing

mkLabel :: (ToHtml a, Monad m) => a -> XY -> HtmlT m ()
mkLabel s (x, y) = do
  text_ [ x_ (toText x)
        , y_ (toText y)
        , stroke_ "white", fill_ "white"
        , stroke_width_ "5"
        , filter_ "url(#blur1)"
        , text_anchor_ "middle"
        , opacity_ "1" ] $ toHtml s
  text_ [ x_ (toText x)
        , y_ (toText y)
        , stroke_ "none", fill_ "black"
        , text_anchor_ "middle"
        , opacity_ "1" ] $ toHtml s

-- | Returns raw SVG for a figure.
svg :: Monad m => [Fig] -> HtmlT m ()
svg fig = do
  doctype_
  with (svg11_ content)
    [ width_ "400" , height_ "400"
    , stroke_ "black", fill_ "none" , fill_opacity_ "0.75"
    , font_size_ "14", font_style_ "italic", font_family_ "sans-serif" ]
  where
    content = do
      filter_ [id_ "blur1"] $ feGaussianBlur_ [stdDeviation_ "0.5"]
      rect_ [ width_ "100%", height_ "100%"
            , fill_ "white", stroke_ "none", fill_opacity_ "1"]
      foldMap (renderFig . rescaleTo (400,400)) $ sort fig

-- | Outputs figure to SVG file.
writeSVG :: FilePath -> Figure PGA2 b -> IO ()
writeSVG fname figs = do
  h <- openFile fname WriteMode
  hPrint h (svg (figure figs))
  hClose h

-- | Outputs figure to PNG file.
writePNG :: String -> Figure PGA2 b -> IO ()
writePNG fname figs = do
  let svgFile = fname <> ".svg"
      pngFile = fname <> ".png"
  writeSVG svgFile figs
  let opts = ["-density", "150", svgFile, pngFile]
  (_, Just hout, _, _) <- createProcess (proc "convert" opts){ std_out = CreatePipe }
  out <- hGetContents hout
  guard (null out)
  print pngFile

------------------------------------------------------------

-- | Type representing animationa frames. 
newtype Animation a = Animation ([[Fig]], a)
  deriving (Show, Functor, Applicative, Monad)

-- | Outputs single frame of animation, given by a frame number. 
writeFrame n fname (Animation (fs,_)) =
  do h <- openFile fname WriteMode
     hPrint h (svg (fs !! n))
     hClose h
     print fname

-- | Outputs  animation as GIF file, using `convert` utility.
runAnimation :: String -> Animation a -> IO ()
runAnimation fname (Animation (fs,_)) = do
  print "producing SVG.."
  mapM_ mkSVG $ zip fs [0..]
  print "converting to GIF.."
  let opts = ["-delay", "0", "-loop", "0", "figs/an/*.svg", fname]
  (_, Just hout1, _, _) <- createProcess (proc "convert" opts){ std_out = CreatePipe }
  out <- hGetContents hout1
  guard (null out)
  print fname
  return ()
  where
    mkSVG (fr, i) = let
      si = show i
      fnum = replicate (3-length si) '0' <> si
      fname = "figs/an/p" <> fnum <> ".svg"
      in do h <- openFile fname WriteMode
            hPrint h (svg fr)
            hClose h
            print fname

-- | Creates animation from a list of figures.
animateList :: [Figure PGA2 a] -> Animation ()
animateList frs = Animation (figure <$> frs, ())

-- | Creates animation from a parameterized  function and given nuber of frames.
animate :: Int -> (Double, Double) -> (Double -> Figure PGA2 a) -> Animation ()
animate n (a, b) mkFrame = mapM_ frame ts
  where
    ts = [ a + fromIntegral i * (b-a)/fromIntegral (n)
         | i <- [0..n-1] ]
    frame t = Animation ([figure (mkFrame t)], ())

-- | Creates animation with nonlinear paremeterization.
transform :: Int -> (Double -> Figure PGA2 a) -> Animation ()
transform n f = animate n (0,1) go
  where
    go t = f $ 1/(1+exp(6/tan(pi*t)))

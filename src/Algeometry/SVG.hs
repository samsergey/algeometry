{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase
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
  (  module Lucid.Svg
  , Fig (..), Figure, svg , writeSVG, writePNG, (@), (<@)
  , figure, viewPoint
  , axis, grid, background, display, label
  , point, line, nline, vect, nvect, plane
  , angle
  , polygon, polyline, regularPoly, segment, segm, put
  , plane2, plane3, orthoPlane
  , clipPoly
--  , rplane
  , getFigure, getResult, mapFig
  , Animation, writeFrame, runAnimation, animate, transform, animateList
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

data Fig = Point   [Attribute] XY
         | Line    [Attribute] XY XY
         | Polygon [Attribute] [XY]
  deriving (Show, Eq)

instance Ord Fig where
  compare (Point _ _) _ = GT
  compare _ (Point _ _) = LT
  compare _ _ = EQ

------------------------------------------------------------

clipPoly :: [PGA2] -> PGA2 -> [PGA2]
clipPoly pts mv = let
  frame = zip pts (tail pts)
  in mapMaybe (segmentMeet mv) frame

clip :: PGA2 -> [PGA2]
clip mv = let
  vs = fromXY <$> [[-20,-20],[-20,20],[20,20],[20,-20],[-20,-20]]
  frame = zip vs (tail vs)
  in mapMaybe (segmentMeet mv) frame

------------------------------------------------------------

newtype Figure a b = Figure ([([a], [Attribute])], b)
  deriving (Show, Functor, Applicative, Monad)

instance Eq a => Semigroup (Figure a b) where
  Figure (!f1, b) <> Figure (!f2, _) = Figure (f1 `union` f2, b)

instance Eq a => Monoid (Figure a b) where
  mempty = Figure (mempty, undefined)

instance With (Figure a b) where
  with (Figure !([(a, attr)], b)) as = 
        Figure ([(a, attr `with` as)], b)
    
getFigure :: Figure a b -> [([a], [Attribute])]
getFigure (Figure p) = fst p

getResult :: Figure a b -> b
getResult (Figure p) = snd p

mapFig :: (a1 -> a2) -> Figure a1 b -> Figure a2 b
mapFig f (Figure !(s, r)) = Figure (map (\(x, a) -> (map f x, a)) s, r)

put :: a -> Figure a a
put x = Figure ([([x], [])], x)

puts :: [a] -> Figure a [a]
puts x = Figure ([(x, [])], x)

infix 1 @
(@) :: a -> [Attribute] -> Figure a a
a @ attr = put a `with` attr

infix 1 <@
(<@) :: Figure a b -> [Attribute] -> Figure a b
(<@) = with

------------------------------
-- geometric objects

point :: GeomAlgebra b a => [Double] -> a
point = fromXY

display :: (Show s,  GeomAlgebra b a) => [Double] -> s -> Figure a a
display p s = label p $ show s

label :: (GeomAlgebra b a) => [Double] -> String -> Figure a a
label p s = point p @ [ id_ (pack s), class_ "label"]

nline :: GeomAlgebra b a => [Double] -> [Double] -> a
nline a b = fromXY a `join` fromXY b

line :: GeomAlgebra b a => [Double] -> [Double] -> a
line a b = fromXY a ∨ fromXY b

vect :: GeomAlgebra b a => [Double] -> a
vect = line []

nvect :: GeomAlgebra b a => [Double] -> a
nvect = nline []

axis :: Figure PGA2 PGA2
axis = do
  line [-20,0] [20,0] @ [stroke_width_ "0.25"]
  line [0,-20] [0,20] @ [stroke_width_ "0.25"]

plane :: GeomAlgebra b a
      => [Double] -> [Double] -> [Double] -> Figure a a
plane a b c = put $ fromXY a `join` fromXY b `join` fromXY c

polygon :: GeomAlgebra e a => [a] -> Figure a [a]
polygon pts = res `with` [class_ "polygon"]
  where
    res = case pts of
      h:_:_ | isPoint h -> puts pts
            | isLine h ->
              let pts' = zipWith outer pts (tail pts ++ [h])
              in const pts <$> puts pts'
      _ -> puts pts

polyline :: GeomAlgebra e a => [a] -> Figure a [a]
polyline pts = polygon pts `with` [class_ "polyline"]

regularPoly n = polygon [ point ([cos (2*pi*t), sin (2*pi*t)])
                        | t <- [0, 1/n .. n-1/n] ]

segment :: GeomAlgebra e a => a -> a -> Figure a [a]
segment p1 p2 = polyline [p1, p2]

segm
  :: GeomAlgebra b a
  => [Double] -> [Double] -> Figure a [a]
segm a b = segment (fromXY a) (fromXY b)

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
    p' = projectionOf p `on` l 
    l' = p `join` p'

orthoPlane
  :: (Fractional a, GeomAlgebra e a)
  => a -> a -> (Double, Double) 
  -> Figure a a
orthoPlane p o = plane2 p l
  where
    p' = projectionOf p `on` o 
    l = p' `inner` (p `join` o)

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

background :: GeomAlgebra e a => String -> Figure a [a]
background col = polygon [] `with` [ class_ "bg", fill_ (pack col)] 
    
------------------------------------------------------------

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
      
    xy mv = case toXY mv of
      [] -> (0,0)
      [x] -> (x,0)
      x:y:_ -> (x,y)


viewPoint :: [Double] -> Figure PGA3 b -> Figure PGA2 b
viewPoint vp = mapFig (coerce . project)
  where
    p0 = dual (fromXY vp)
    project x
      | isPoint x = -((x `join` fromXY vp) `meet` p0 ) * p0
      | isLine x = -((x `join` fromXY vp) `meet` p0 ) * p0
      | otherwise =  -((x `join` fromXY vp) `meet` p0 ) * p0

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

writeSVG :: FilePath -> Figure PGA2 b -> IO ()
writeSVG fname figs = do
  h <- openFile fname WriteMode
  hPrint h (svg (figure figs))
  hClose h

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

newtype Animation a = Animation ([[Fig]], a)
  deriving (Show, Functor, Applicative, Monad)

writeFrame n fname (Animation (fs,_)) =
  do h <- openFile fname WriteMode
     hPrint h (svg (fs !! n))
     hClose h
     print fname

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

animateList :: [Figure PGA2 a] -> Animation ()
animateList frs = Animation (figure <$> frs, ())

animate :: Int -> (Double, Double) -> (Double -> Figure PGA2 a) -> Animation ()
animate n (a, b) mkFrame = mapM_ frame ts
  where
    ts = [ a + fromIntegral i * (b-a)/fromIntegral (n)
         | i <- [0..n-1] ]
    frame t = Animation ([figure (mkFrame t)], ())

transform :: Int -> (Double -> Figure PGA2 a) -> Animation ()
transform n f = animate n (0,1) go
  where
    go t = f $ 1/(1+exp(6/tan(pi*t)))

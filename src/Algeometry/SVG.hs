{-# LANGUAGE LambdaCase
, DerivingVia
, StandaloneDeriving
, DataKinds
, KindSignatures
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, FlexibleInstances
, OverloadedStrings #-}

module Algeometry.SVG
  ( Fig (..), Figure, svg , writeSVG, writePNG,animate, (@)
  , figure, viewPoint
  , axis, vect, display
  , polygon, polyline, segment, put, plane, plane3, orthoPlane
  , getFigure, getResult, mapFig
  ) where

import Algeometry.GeometricAlgebra
import Algeometry.Types
import Control.Monad hiding (join)
import Data.List (sort, nub)
import Data.Maybe
import Data.Text (pack)
import Lucid.Base (HtmlT)
import Lucid.Svg hiding (scale, Term)
import System.IO
import System.Process
import Data.Coerce

------------------------------------------------------------

type XY = (Double, Double)

toXY :: PGA2 -> (Double, Double)
toXY mv = case toPoint mv of
  [] -> (0,0)
  [x] -> (x,0)
  x:y:_ -> (x,y)

------------------------------------------------------------

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

newtype Figure a b = Figure ([([a], [Attribute])], b)
  deriving (Show, Functor, Applicative, Monad)

getFigure :: Figure a b -> [([a], [Attribute])]
getFigure (Figure p) = fst p

getResult :: Figure a b -> b
getResult (Figure p) = snd p

mapFig :: (a1 -> a2) -> Figure a1 b -> Figure a2 b
mapFig f (Figure (s, r)) = Figure (map (\(x, a) -> (map f x, a)) s, r)

axis :: Figure PGA2 PGA2
axis = do
  line [-20,0] [20,0] @ [stroke_width_ "0.25"]
  line [0,-20] [0,20] @ [stroke_width_ "0.25"]

put :: a -> [Attribute] -> Figure a ()
put x attr = Figure ([([x], attr)], ())

display :: (Show s, GeomAlgebra e a)
        => [Double] -> s -> Figure a a
display p s = point p @ [ id_ (pack (show s))
                        , fill_ "none"
                        , stroke_width_ "0"
                        , class_ "label"]

polygon :: [a] -> [Attribute] -> Figure a [a]
polygon pts attr = Figure ([(pts, attr')], pts)
  where
    attr' = [class_ "polygon"] <> attr

polyline :: [a] -> [Attribute] -> Figure a [a]
polyline pts attr = Figure ([(pts, attr')], pts)
  where
    attr' = [class_ "polyline"] <> attr

vect :: GeomAlgebra b a => [Double] -> a
vect p = point [] ∨ point p

segment :: a -> a -> [Attribute] -> Figure a [a]
segment p1 p2 = polyline [p1, p2]

plane
  :: PGA3 -> PGA3 -> (Double, Double) -> [Attribute]
  -> Figure (PGA3) (PGA3)
plane p l (a, b) attr = do
  polygon [ shiftAlong' l' (b) $ shiftAlong' l (a) p
          , shiftAlong' l' (b) $ shiftAlong' l (-a) p 
          , shiftAlong' l' (-b) $ shiftAlong' l (-a) p'
          , shiftAlong' l' (-b) $ shiftAlong' l (a) p' ] attr
  return (p ∨ l)
  where
    p' = projectionOf p `on` l 
    l' = p `join` p'

orthoPlane
  :: PGA3 -> PGA3 -> (Double, Double)
  -> [Attribute] -> Figure (PGA3) (PGA3)
orthoPlane p o = plane p l
  where
    p' = projectionOf p `on` o 
    l = p' `inner` (p `join` o)

plane3
  :: PGA3 -> PGA3 -> PGA3 -> (Double, Double)
  -> [Attribute]  -> Figure (PGA3) (PGA3)
plane3 p1 p2 p3 = plane p1 (p2 `join` p3)

------------------------------------------------------------

infix 1 @
(@) :: a -> [Attribute] -> Figure a a
mv @ attr = const mv <$> put mv attr

figure :: Figure PGA2 b -> [Fig]
figure = foldMap draw . getFigure
  where
    draw (mvs, attr) = case mvs of
      [mv] | isPoint mv -> [Point attr $ toXY mv]
           | isLine mv -> case nub (toXY <$> clip mv) of
               [] -> []
               [p] -> [Point attr p]
               p1:p2:_ -> [Line attr p1 p2]
           | otherwise -> []
      _ | all isPoint mvs -> [Polygon attr $ toXY <$> mvs]
        | otherwise -> mvs >>= \v -> draw ([v], attr)

viewPoint :: [Double] -> Figure PGA3 b -> Figure PGA2 b
viewPoint vp = mapFig (coerce . project)
  where
    p0 = dual (point vp)
    project x
      | isPoint x = -((x `join` point vp) `meet` p0 ) * p0
      | isLine x = -((x `join` point vp) `meet` p0 ) * p0
      | otherwise = undefined

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
        circle_ $ [ cx_ (pack (show x)) , cy_ (pack (show y))
                  , r_ "2.25", fill_ "black" , stroke_ "white"] <> attr
    case getId attr of
      [s] -> label s (x+6, y-6)
      _ -> mempty
      
  Line attr a b -> do
    case getClass attr of
      ["label"] -> mempty
      _ ->
        polyline_ $ [ points_ (pack $ foldMap toStr [a, b])] <> attr
    case getId attr of
      [s] -> do
        let (x1, y1) = a
        let (x2, y2) = b
        let n = sqrt((x1-x2)**2 + (y1-y2)**2)
        label s (x2 + 20/n*(x1 - x2), y2 + 20/n*(y1 - y2))
      _ -> mempty
 
  Polygon attr pts ->
    case getClass attr of
      ["polygon"]
        -> polygon_ $ [ points_ (pack $ foldMap toStr pts)] <> attr
      ["polyline"]
        -> polyline_ $ [ points_ (pack $ foldMap toStr pts)] <> attr
      _ -> mempty

  where
    toStr (x,y) = show x <> "," <> show y <> " "
    
    getId = mapMaybe $ \case
      Attribute "id" x -> Just x
      _ -> Nothing

    getClass = mapMaybe $ \case
      Attribute "class" x -> Just x
      _ -> Nothing

label :: (ToHtml a, Monad m) => a -> XY -> HtmlT m ()
label s (x, y) = do
  text_ [ x_ (pack $ show $ x)
        , y_ (pack $ show $ y)
        , stroke_ "white", fill_ "white"
        , stroke_width_ "5"
        , filter_ "url(#blur1)"
        , text_anchor_ "middle"
        , opacity_ "1" ] $ toHtml s
  text_ [ x_ (pack $ show $ x)
        , y_ (pack $ show $ y)
        , stroke_ "none", fill_ "black"
        , text_anchor_ "middle"
        , opacity_ "1" ] $ toHtml s

svg :: Monad m => [Fig] -> HtmlT m ()
svg fig = do
  doctype_
  with (svg11_ content)
    [width_ "400" , height_ "400"
    , stroke_ "black", fill_ "none" , fill_opacity_ "0.75"
    , font_style_ "italic", font_family_ "CMU Serif, sans-serif" ]
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

animate
  :: Fractional a => Int -> (a, a) -> (a -> Figure PGA2 b) -> String -> IO ()
animate n (a, b) mkFrame fname = do
  print "producing SVG.."
  forM_ [0..n-1] frame
  print "converting to GIF.."
  let opts = ["-delay", "0", "-loop", "0", "figs/an/*.svg", fname]
  (_, Just hout1, _, _) <- createProcess (proc "convert" opts){ std_out = CreatePipe }
  out <- hGetContents hout1
  guard (null out)
  print fname
  return ()
  where
    frame i = let
      x = a + fromIntegral i * (b-a)/fromIntegral (n)
      si = show i
      fnum = replicate (3-length si) '0' <> si
      fname = "figs/an/p" <> fnum <> ".svg"
      in do writeSVG fname (mkFrame x)
            print fname
  
--rotateAbout ::
--  PGA3 -> (PGA3 -> PGA2) -> Figure (PGA3) a -> Double -> [Fig]
--rotateAbout o f fig a = figureWith (f . rotateAt o a) fig

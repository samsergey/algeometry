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
  ( Fig (..), svg , writeSVG, writePNG,animate, (@)
  , figure, figureWith, viewPoint, rotateAbout
  , axis, vect, viewFrom
  , polygon, segment, put, plane, plane3, orthoPlane
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

type Record a b = ([([a], [Attribute])], b)

axis :: Record PGA2 PGA2
axis = do
  line [-20,0] [20,0] @ [stroke_width_ "0.25"]
  line [0,-20] [0,20] @ [stroke_width_ "0.25"]

put :: a -> [Attribute] -> Record a ()
put x attr = ([([x], attr)], ())

polygon :: [a] -> [Attribute] -> Record a [a]
polygon pts attr = ([(pts, attr)], pts)

vect p = [point [], point p]

segment :: a -> a -> [Attribute] -> Record a [a]
segment p1 p2 = polygon [p1, p2]

plane
  :: PGA3 -> PGA3 -> (Double, Double) -> [Attribute]
  -> Record (PGA3) (PGA3)
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
  -> [Attribute] -> Record (PGA3) (PGA3)
orthoPlane p o = plane p l
  where
    p' = projectionOf p `on` o 
    l = p' `inner` (p `join` o)

plane3
  :: PGA3 -> PGA3 -> PGA3 -> (Double, Double)
  -> [Attribute]  -> Record (PGA3) (PGA3)
plane3 p1 p2 p3 = plane p1 (p2 `join` p3)

------------------------------------------------------------

infix 1 @
(@) :: a -> [Attribute] -> Record a a
mv @ attr = const mv <$> put mv attr

figure :: Record (PGA2) b -> [Fig]
figure = foldMap draw . fst
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

figureWith :: (a -> PGA2) -> Record a b -> [Fig]
figureWith f = figure . mapFst (mapFst f)
  where
    mapFst g (a,b) = (map g a, b)

viewPoint :: [Double] -> PGA3 -> PGA2
viewPoint vp = coerce . project
  where
    p0 = dual (point vp)
    project x
      | isPoint x = -((x `join` point vp) `meet` p0 ) * p0
      | isLine x = -((x `join` point vp) `meet` p0 ) * p0
      | otherwise = undefined

viewFrom :: [Double] -> [[PGA3]] -> [[PGA2]]
viewFrom p = map (map (viewPoint p))

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
    circle_ $ [ cx_ (pack (show x)) , cy_ (pack (show y))
              , r_ "2.25", fill_ "black" , stroke_ "white"] <> attr
    case getId attr of
      [s] -> label s (x+6, y-6)
      _ -> mempty
      
  Line attr a b -> do
    polygon_ $ [ points_ (pack $ foldMap toStr [a, b])] <> attr
    case getId attr of
      [s] -> do
        let (x1, y1) = a
        let (x2, y2) = b
        let n = sqrt((x1-x2)**2 + (y1-y2)**2)
        label s (x2 + 20/n*(x1 - x2), y2 + 20/n*(y1 - y2))
      _ -> mempty
 
  Polygon attr pts ->
    polygon_ $ [ points_ (pack $ foldMap toStr pts)] <> attr

  where
    toStr (x,y) = show x <> "," <> show y <> " "
    
    getId = mapMaybe $ \case
      Attribute "id" x -> Just x
      _ -> Nothing

label :: (ToHtml a, Monad m) => a -> XY -> HtmlT m ()
label s (x, y) = do
  circle_ [ cx_ (pack $ show $ x)
          , cy_ (pack $ show $ y - 5)
          , r_ "7" , fill_ "white", filter_ "url(#blur)"
          , stroke_ "none", fill_opacity_ "1"]
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
      filter_ [id_ "blur"] $ feGaussianBlur_ [stdDeviation_ "2"]
      rect_ [ width_ "100%", height_ "100%"
            , fill_ "white", stroke_ "none", fill_opacity_ "1"]
      foldMap (renderFig . rescaleTo (400,400)) $ sort fig

writeSVG :: FilePath -> [Fig] -> IO ()
writeSVG fname figs = do
  h <- openFile fname WriteMode
  hPrint h (svg figs)
  hClose h

writePNG :: String -> [Fig] -> IO ()
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
  :: Fractional a => Int -> (a, a) -> (a -> [Fig]) -> String -> IO ()
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
  
rotateAbout ::
  PGA3 -> (PGA3 -> PGA2) -> Record (PGA3) a -> Double -> [Fig]
rotateAbout o f fig a = figureWith (f . rotateAt o a) fig

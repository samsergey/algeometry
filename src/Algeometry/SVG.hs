{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Algeometry.SVG
  ( Fig (..), svg , writeSVG, writePNG,animate, (@)
  , figure, figureWith, viewPoint
  , polygon, put, plane, plane3, orthoPlane
  ) where

import Algeometry.PGA
import Control.Monad hiding (join)
import Data.List (sort, nub)
import Data.Maybe
import Data.Text (pack)
import Lucid.Base (HtmlT)
import Lucid.Svg hiding (scale, Term)
import GHC.TypeNats (KnownNat)
import System.IO
import System.Process
import Data.Coerce

------------------------------------------------------------

type XY = (Double, Double)

toXY :: KnownNat n => PGA n -> (Double, Double)
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

clip :: GeometricAlgebra b => b -> [b]
clip mv = let
  vs = point <$> [[0,0],[0,300],[300,300],[300,0],[0,0]]
  frame = zip vs (tail vs)
  in mapMaybe (segmentMeet mv) frame

------------------------------------------------------------

type Record a b = ([([a], [Attribute])], b)

put :: a -> [Attribute] -> Record a ()
put x attr = ([([x], attr)], ())

polygon :: [a] -> [Attribute] -> Record a ()
polygon pts attr = ([(pts, attr)], ())

plane
  :: PGA 3 -> PGA 3 -> (Double, Double)
  -> [Attribute] -> Record (PGA 3) ()
plane p l (a, b) =
  polygon [ shiftAlong l' (-b) $ shiftAlong l (a) p
          , shiftAlong l' (-b) $ shiftAlong l (-a) p 
          , shiftAlong l' (b) $ shiftAlong l (-a) p'
          , shiftAlong l' (b) $ shiftAlong l (a) p' ]
  where
    p' = (l `inner` p)*l
    l' = p `join` p'

orthoPlane
  :: PGA 3 -> PGA 3 -> (Double, Double)
  -> [Attribute] -> Record (PGA 3) ()
orthoPlane p o = plane p l
  where
    p' = (o `inner` p)*o
    l = p' `inner` (p `join` o)

plane3
  :: PGA 3 -> PGA 3 -> PGA 3 -> (Double, Double)
  -> [Attribute]  -> Record (PGA 3) ()
plane3 p1 p2 p3 = plane p1 (p2 `join` p3)

------------------------------------------------------------

infix 1 @
(@) :: a -> [Attribute] -> Record a a
mv @ attr = const mv <$> put mv attr

figure :: Record (PGA 2) b -> [Fig]
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

figureWith :: (a -> PGA 2) -> Record a b -> [Fig]
figureWith f = figure . mapFst (mapFst f)
  where
    mapFst g (a,b) = (map g a, b)

viewPoint :: [Double] -> PGA 3 -> PGA 2
viewPoint vp = coerce . project
  where
    project x
      | isPoint x = -((x `join` point vp) `meet` e 3 ) * e 3
      | isLine x = -((x `join` point vp) `meet` e 3 ) * e 3
      | otherwise = zero

------------------------------------------------------------

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
        , opacity_ "1"
        , style_ "fontfamily : CMU Serif;"] $ toHtml s

svg :: Monad m => [Fig] -> HtmlT m ()
svg fig = do
  doctype_
  with (svg11_ content)
    [width_ "300" , height_ "300"
    , stroke_ "black", fill_ "none" , fill_opacity_ "0.75"]
  where
    content = do
      filter_ [id_ "blur"] $ feGaussianBlur_ [stdDeviation_ "2"]
      rect_ [ width_ "100%", height_ "100%"
            , fill_ "white", stroke_ "none", fill_opacity_ "1"]
      foldMap renderFig $ sort fig

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
  let opts = ["-format", "png", svgFile, pngFile]
  (_, Just hout, _, _) <- createProcess (proc "mogrify" opts){ std_out = CreatePipe }
  out <- hGetContents hout
  guard (null out)
  print pngFile

animate
  :: Fractional a => Int -> (a, a) -> (a -> [Fig]) -> String -> IO ()
animate n (a, b) mkFrame fname = do
  forM_ [0..n-1] frame
  let opts = ["-delay", "0", "-loop", "0", "figs/an/*.png", fname]
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
      fname = "figs/an/p" <> fnum
      in writePNG fname (mkFrame x)
  

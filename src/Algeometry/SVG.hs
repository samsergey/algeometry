{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Algeometry.SVG
  ( Fig (..), svg , writeSVG, writePNG, (@)
  , figure, polygon, put
  ) where

import Algeometry.Base
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

data Fig a = Point   [Attribute] XY
           | Line    [Attribute] XY XY
           | Polygon [Attribute] [XY]
  deriving (Show, Eq)

instance Ord (Fig a) where
  compare (Point _ _) _ = GT
  compare _ (Point _ _) = LT
  compare _ _ = EQ

------------------------------------------------------------

class GeometricAlgebra a => Figure a where
  draw :: a -> [Attribute] -> [Fig a]
  drawMany :: [a] -> [Attribute] -> [Fig a]

instance Figure (PGA 2) where
  draw mv attr 
    | isPoint mv = [Point attr $ toXY mv]
    | isLine mv = case nub (toXY <$> clip mv) of
        [] -> []
        [p] -> [Point attr p]
        p1:p2:_ -> [Line attr p1 p2]
    | otherwise = []

  drawMany mvs attr =
    if length mvs > 1 && all isPoint mvs
    then [Polygon attr $ toXY <$> mvs]
    else mvs >>= \v -> draw v attr

clip :: GeometricAlgebra b => b -> [b]
clip mv = let
  vs = point <$> [[0,0],[0,300],[300,300],[300,0],[0,0]]
  frame = zip vs (tail vs)
  in mapMaybe (segmentMeet mv) frame

projectPGA :: PGA 3 -> PGA 2
projectPGA (PGA mv) = case grade mv of
  3 -> PGA $ projectOn mv (e 3) * (-e 3)
  2 -> PGA $ ((mv ∨ dual (e 3)) ∧ e 3) * (-e 3)
  _ -> zero

instance Figure (PGA 3) where
  draw = coerce . draw . projectPGA
  drawMany = coerce . drawMany . map projectPGA

------------------------------------------------------------

put :: Figure a => a -> [Attribute] -> ([Fig a], ())
put x attr = (draw x attr, ())

polygon :: Figure a => [a] -> [Attribute] -> ([Fig a], ())
polygon pts attr = (drawMany pts attr, ())

infix 1 @
(@) :: Figure a => a -> [Attribute] -> ([Fig a], a)
mv @ attr = const mv <$> put mv attr

figure :: ([Fig a], a) -> [Fig a]
figure = fst

------------------------------------------------------------

renderFig :: Monad m => Fig a -> HtmlT m ()
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

svg :: Monad m => [Fig a] -> HtmlT m ()
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

writeSVG :: FilePath -> [Fig a] -> IO ()
writeSVG fname figs = do
  h <- openFile fname WriteMode
  hPrint h (svg figs)
  hClose h

writePNG :: String -> [Fig a] -> IO ()
writePNG fname figs = do
  writeSVG "tmp.svg" figs
  let opts = ["-density", "150", "tmp.svg", fname]
  (_, Just hout, _, _) <- createProcess (proc "convert" opts){ std_out = CreatePipe }
  out <- hGetContents hout
  guard (null out)
  return ()

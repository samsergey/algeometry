{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Algeometry.SVG
  ( Fig (..), svg , writeSVG, writePNG, (@)
  , figure, polygon, put
  ) where

import Algeometry.Base
import Control.Monad
import Data.List (sort, nub)
import Data.Maybe
import Data.Text (pack)
import Lucid.Base (HtmlT)
import Lucid.Svg hiding (scale, Term)
import System.IO
import System.Process

data Fig = Point [Attribute] [Double]
         | Polygon [Attribute] [[Double]]
  deriving (Show, Eq)

instance Ord Fig where
  compare (Point _ _) _ = GT
  compare _ (Point _ _) = LT
  compare _ _ = EQ

class Figure a where
  toFig :: a -> [Attribute] -> [Fig]

instance Figure (PGA 2) where
  toFig mv attr
    | grade mv == 1 = case nub (toPoint <$> clip mv) of
        [] -> mempty
        [p] -> [Point attr p]
        p1:p2:_ -> [Polygon attr [p1, p2]]
    | grade mv == 2 = [Point attr $ toPoint mv]
    | otherwise = mempty

clip :: GeometricAlgebra b => b -> [b]
clip mv = let
  vs = point <$> [[0,0],[0,300],[300,300],[300,0],[0,0]]
  frame = zip vs (tail vs)
  in mapMaybe (segmentMeet mv) frame

------------------------------------------------------------

put :: PGA 2 -> [Attribute] -> ([Fig], ())
put x attr = (toFig x attr, ())


polygon :: [PGA 2] -> [Attribute] -> ([Fig], ())
polygon pts attr = ([res], ())
  where
    res = Polygon attr $ toPoint <$> filter (\x -> grade x == 2) pts

infix 1 @
(@) :: PGA 2 -> [Attribute] -> ([Fig], PGA 2)
mv @ attr = const mv <$> put mv attr

figure :: ([Fig], a) -> [Fig]
figure = fst

------------------------------------------------------------

renderFig :: Monad m => Fig -> HtmlT m ()
renderFig f = case f of
  Point attr [x,y] -> do 
    circle_ $ [ cx_ (pack (show x)) , cy_ (pack (show y))
              , r_ "2.25", fill_ "black" , stroke_ "white"] <> attr

    case getId attr of
      [s] -> do
        circle_ [ cx_ (pack $ show $ x + 6)
                , cy_ (pack $ show $ y - 11)
                , r_ "7" , fill_ "white", filter_ "url(#blur)"
                , stroke_ "none", fill_opacity_ "1"]
        text_ [ x_ (pack $ show $ x + 6)
              , y_ (pack $ show $ y - 6)
              , stroke_ "none", fill_ "black"
              , text_anchor_ "middle"
              , opacity_ "1"] $ toHtml s
      _ -> mempty
  Polygon attr pts ->
    polygon_ $ [ points_ (pack $ foldMap toStr pts)] <> attr
  _ -> mempty
  where
    toStr [x,y] = show x <> "," <> show y <> " "
    toStr _ = mempty
    
    getId = mapMaybe $ \case
      Attribute "id" x -> Just x
      _ -> Nothing

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
            , fill_ "white", stroke_ "none"]
      foldMap renderFig $ sort fig

writeSVG :: FilePath -> [Fig] -> IO ()
writeSVG fname figs = do
  h <- openFile fname WriteMode
  hPrint h (svg figs)
  hClose h

writePNG :: String -> [Fig] -> IO ()
writePNG fname figs = do
  writeSVG "tmp.svg" figs
  let opts = ["-density", "150", "tmp.svg", fname]
  (_, Just hout, _, _) <- createProcess (proc "convert" opts){ std_out = CreatePipe }
  out <- hGetContents hout
  guard (null out)
  return ()

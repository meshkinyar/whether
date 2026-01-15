{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Whether.Frame where

import Formatting
import GHC.Generics
import Whether.Weather
import Whether.Display
import qualified Data.Text as S ( Text, pack, unpack, show, intercalate, replicate, length )

newtype Element = Border Border

data Border = Top
            | Divider
            | Bottom

data FrameProperties = FrameProperties
  { lineStyle    :: LineStyle
  , dtStyle      :: DTStyle
  , contentStyle :: ContentStyle
  , colWidth     :: Int
  , mode         :: DisplayStyle
  }

data Frame = Frame
  { properties :: FrameProperties
  , order      :: [Element]
  }

data Row = Row S.Text S.Text S.Text

newtype Cell = Cell [S.Text]

data LineStyle = Rounded | Angular
  deriving (Eq, Read, Show, Generic)

concatWrap :: [S.Text] -> S.Text
concatWrap t = S.pack $ concat $ wrapLine (map S.unpack t)
  where
    wrapLine []   = []
    wrapLine [x]  = [x]
    wrapLine (x:xs) = x : "│" : wrapLine xs

-- Framers

makeRows :: Cell -> [Row]
makeRows (Cell ts) = [Row "│" (concatWrap ts) "│"]

borderRow :: LineStyle -> Border -> Int -> Int -> Row
borderRow Rounded btype w n = border btype
  where
    border Top     = Row "╭" (body "┬") "╮"
    border Divider = Row "├" (body "┼") "┤"
    border Bottom  = Row "╰" (body "┴") "╯"
    body = borderBody w n "─"
borderRow Angular btype w n = border btype
  where
    border Top     = Row "┌" (body "┬") "┐"
    border Divider = Row "├" (body "┼") "┤"
    border Bottom  = Row "└" (body "┴") "┘"
    body = borderBody w n "─"

borderBody :: Int -> Int -> S.Text -> S.Text -> S.Text
borderBody w n line sep = S.intercalate sep $ replicate n $ S.replicate w line

-- contentRow :: (a -> S.Text) -> [a] -> Row
-- contentRow formatter li = Row "│" body "│"
--   where
--   body = concatWrap $ map formatter li

unwrapRow :: Row -> S.Text
unwrapRow (Row x y z) = x <> y <> z
  
compactFormat :: FrameProperties -> S.Text -> S.Text -> S.Text
compactFormat fp icoL t = " " <> icoL <> t <> padR
  where
    padR = S.replicate lenR " "
    lenR = colWidth fp - S.length t - S.length icoL - 1

expandedFormat :: FrameProperties -> S.Text -> S.Text -> S.Text
expandedFormat fp x y = " " <> x <> " " <> y <> padR
  where
    padR = S.replicate lenR " "
    lenR = colWidth fp - S.length y - S.length x - 2

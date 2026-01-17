{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Whether.Frame where

import Data.List               ( intersperse )
import Formatting
import GHC.Generics
import Whether.Weather
import Whether.Display
import qualified Data.Text as S ( Text, pack, concat, show, intercalate, replicate, length )

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

data Row = 
  Row
    {
      cells     :: [Cell]
    , sepChar   :: S.Text
    , leftChar  :: S.Text
    , rightChar :: S.Text
    }

newtype Cell = Cell S.Text

data LineStyle = Rounded | Angular | ASCII
  deriving (Eq, Read, Show, Generic)

wrapRowContent :: [S.Text] -> S.Text
wrapRowContent t = S.concat $ wrapContent t
  where
    wrapContent []   = []
    wrapContent [x]  = [x]
    wrapContent (x:xs) = x : "│" : wrapContent xs

-- Framers

-- makeRows :: [Cell] -> [Row]
-- makeRows [] = []
-- makeRows [Cell t] = [Row t "│" "│" "│"]
-- makeRows ts = map (\t -> Row  "│" "│" "│")

mkBorderRow :: LineStyle -> Border -> Int -> Int -> Row
mkBorderRow Rounded btype w n = border btype
  where
    border Top     = row "┬" "╭" "╮"
    border Divider = row "┼" "├" "┤"
    border Bottom  = row "┴" "╰" "╯"
    row sep = Row (mkBorderCells w n "─" sep) sep
mkBorderRow Angular btype w n = border btype
  where
    border Top     = row "┬" "┌" "┐"
    border Divider = row "┼" "├" "┤"
    border Bottom  = row "┴" "└" "┘"
    row sep = Row (mkBorderCells w n "─" sep) sep
mkBorderRow ASCII btype w n = border btype
  where
    border Top     = row "+" "/" "\\"
    border Divider = row "+" "+" "+"
    border Bottom  = row "+" "\\" "/"
    row sep = Row (mkBorderCells w n "-" sep) sep
 
mkBorderCells :: Int -> Int -> S.Text -> S.Text -> [Cell]
mkBorderCells w n line sep = intersperse (Cell sep) . replicate n . Cell . S.replicate w $ line

-- contentRow :: (a -> S.Text) -> [a] -> Row
-- contentRow formatter li = Row "│" body "│"
--   where
--   body = concatWrap $ map formatter li

-- unwrapRow :: Row -> S.Text
-- unwrapRow (Row x y z) = x <> y <> z
  
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

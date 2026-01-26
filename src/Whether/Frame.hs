{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Whether.Frame where

import Data.Int
import Formatting
import GHC.Generics
import Whether.Weather
import Whether.Display
import Whether.Formatters
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text as S ( Text, pack, concat, show, intercalate, replicate, length )

newtype Element = Border Border

type Component r = FrameProperties -> Forecast -> Format r r

data Border = Top
            | Divider
            | Bottom

data FrameProperties = FrameProperties
  { lineStyle    :: LineStyle
  , dtStyle      :: DTStyle
  , contentStyle :: ContentStyle
  , displayMode  :: DisplayMode
  , cellWidth    :: Int
  , cellCount    :: Int
  }

data Frame r = Frame
  { properties :: FrameProperties
  , rows       :: [Row r]
  }

data Row r = 
  Row
    { mkCell    :: Component r
    , sepChar   :: T.Text
    , leftChar  :: Format r r
    , rightChar :: Format r r
    }

data LineStyle = Rounded | Angular | ASCII
  deriving (Eq, Read, Show, Generic)

-- formatFrame :: Frame T.Text -> [Forecast] -> T.Text
-- formatFrame (Frame fp rs) = format rowList
--   where
--     rowList = foldl (\f s -> f >% "\n" <> row fp s)
--               (later $ const "")
--               rs

row :: FrameProperties -> [Forecast] -> Row r -> Format r r
row fp forecasts (Row mk s l r) = l % foldl (\b f -> b % mk fp f) "" forecasts % r

contentRow :: LineStyle -> Component r -> Row r
contentRow style f = Row f b b' b'
  where
    b  = boundary style :: T.Text
    b' = boundary style :: Format r r
    boundary ASCII = "|"
    boundary _     = "│"

borderRow :: LineStyle -> Border -> Row r
borderRow Rounded Top     = mkBorderRow Rounded "┬" "╭" "╮"
borderRow Rounded Divider = mkBorderRow Rounded "┼" "├" "┤"
borderRow Rounded Bottom  = mkBorderRow Rounded "┴" "╰" "╯"
borderRow Angular Top     = mkBorderRow Angular "┬" "┌" "┐"
borderRow Angular Divider = mkBorderRow Angular "┼" "├" "┤"
borderRow Angular Bottom  = mkBorderRow Angular "┴" "└" "┘"
borderRow ASCII   Top     = mkBorderRow ASCII   "+" "/" "\\"
borderRow ASCII   Divider = mkBorderRow ASCII   "+" "+" "+"
borderRow ASCII   Bottom  = mkBorderRow ASCII   "+" "\\" "/"

mkBorderRow :: LineStyle -> T.Text -> Format r r -> Format r r -> Row r
mkBorderRow ASCII = Row $ spanRow "-"
mkBorderRow _     = Row $ spanRow "─"

spanRow :: T.Text -> Component r
spanRow t fp _ = now . T.fromLazyText . T.replicate (fromIntegral $ cellWidth fp) $ t

compactFormat :: FrameProperties -> T.Text -> T.Text -> T.Text
compactFormat fp icoL t = " " <> icoL <> t <> padR
  where
    padR = T.replicate lenR " "
    lenR = fromIntegral $ fromIntegral (cellWidth fp) - T.length t - T.length icoL - 1

expandedFormat :: FrameProperties -> T.Text -> T.Text -> T.Text
expandedFormat fp x y = " " <> x <> " " <> y <> padR
  where
    padR = T.replicate lenR " "
    lenR = fromIntegral $ fromIntegral (cellWidth fp) - T.length y - T.length x - 2

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

-- | A function used to compose a row by formatting individual cells.
type Component r = FrameProperties -> Format r (Forecast -> r)

-- | A type representing a border within a frame.
data Border = Top
            | Divider
            | Bottom

-- | A record of properties used to format elements within a frame.
data FrameProperties = FrameProperties
  { lineStyle    :: LineStyle
  , dtStyle      :: DTStyle
  , contentStyle :: ContentStyle
  , displayMode  :: DisplayMode
  , cellWidth    :: Int
  , cellCount    :: Int
  }

-- | A record containing all information used to print a "frame", or table of
-- formatters displaying weather data.
data Frame r = Frame
  { properties :: FrameProperties
  , rows       :: [Row r]
  }

-- | A record containing a component used to determine cell characters,
-- as well as left, right, and separator border characters.
data Row r = Row
  { composeCell :: Component r
  , sepChar     :: T.Text
  , leftChar    :: Format r r
  , rightChar   :: Format r r
  }

-- | Determines the style of Unicode character used by borders and line rows.
data LineStyle = Rounded | Angular | ASCII
  deriving (Eq, Read, Show, Generic)

-- formatFrame :: Frame T.Text -> [Forecast] -> T.Text
-- formatFrame (Frame fp rs) = format rowList
--   where
--     rowList = foldl (\f s -> f >% "\n" <> row fp s)
--               (later $ const "")
--               rs

-- row :: FrameProperties -> [Forecast] -> Row r -> Format r r
-- row fp forecasts (Row mk s l r) = l % foldl (\b f -> b % mk fp f) "" forecasts % r

-- | Creates a row based on the provided @(LineStyle) and @(Component).
-- Content rows use the same character for left, right, and separator chars.
contentRow :: LineStyle -> Component r -> Row r
contentRow style f = Row f b b' b'
  where
    b  = boundary style :: T.Text
    b' = boundary style :: Format r r
    boundary ASCII = "|"
    boundary _     = "│"

-- | Creates a row consisting of border characters.
-- These are used to form the boundaries of the frame.
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

-- | Common helper function for @(borderRow) that adds a component consisting
-- entirely of horizontal line cells.
mkBorderRow :: LineStyle -> T.Text -> Format r r -> Format r r -> Row r
mkBorderRow ASCII = Row $ spanCell '-'
mkBorderRow _     = Row $ spanCell '─'

-- | Component that takes a single character and outputs a cell that repeats
-- that character to fill the cell width specified in the @(FrameProperties).
spanCell :: Char -> Component r
spanCell c fp = later . (\t _ -> T.fromLazyText t) . T.pack . replicate (fromIntegral $ cellWidth fp) $ c

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

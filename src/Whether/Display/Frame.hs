{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Whether.Display.Frame where

import Data.Int
import Formatting
import GHC.Generics
import Whether.Weather
import Whether.Display
import Whether.Display.Formatters
import Whether.Units

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text as S ( Text, pack, concat, show, intercalate, replicate, length )

-- | A function used to compose a row by formatting individual cells.
type Component a = FrameProperties -> Format T.Builder (a -> T.Builder)

-- | Represents a type of border within a @Frame@.
data Border = Top
            | Divider
            | Bottom

-- | A record of properties used to format elements within a frame.
data FrameProperties = FrameProperties
  { lineStyle   :: LineStyle
  , dtStyle     :: DTStyle
  , glyphStyle  :: GlyphStyle
  , displayMode :: DisplayMode
  , cellWidth   :: Int
  , cellCount   :: Int
  }
  deriving Generic

-- | A record containing all information used to print a "frame", or table of
-- formatters displaying weather data.
data Frame a = Frame
  { properties :: FrameProperties
  , rows       :: [Row a]
  }

-- | A record containing a component used to determine cell characters,
-- as well as left, right, and separator border characters.
data Row a = Row
  { composeCell :: Component a
  , separator   :: T.Text
  , leftBound   :: Format T.Builder T.Builder
  , rightBound  :: Format T.Builder T.Builder
  }

-- | Determines the style of Unicode character used by borders and line rows.
data LineStyle = Rounded | Angular | ASCII
  deriving (Eq, Read, Show, Generic)

-- | Create a forecast builder from a frame and a list of weather data.
formatFrame :: Frame a -> [a] -> T.Builder
formatFrame (Frame fp rs) list = foldMap (\r -> bformat (composeRow r fp) list <> "\n") rs

-- | Compose a row formatter given @FrameProperties@.
composeRow :: Row a -> FrameProperties -> Format T.Builder ([a] -> T.Builder)
composeRow (Row compose sep l r) fp = l %> intercalated sep (compose fp) >% r

-- | Creates a row based on the provided @LineStyle@ and @Component@.
-- Content rows use the same character for left, right, and separator chars.
contentRow :: LineStyle -> Component a -> Row a
contentRow style f = Row f b bl br
  where
    (bl, b, br) = boundaries style
    boundaries ASCII = ("| ", " | ", " |")
    boundaries _     = ("│ ", " │ ", " │")

-- | Creates a row consisting of border characters.
-- These are used to form the boundaries of the frame.
borderRow :: LineStyle -> Border -> Row a
borderRow Rounded Top     = mkBorderRow Rounded "─┬─" "╭─" "─╮"
borderRow Rounded Divider = mkBorderRow Rounded "─┼─" "├─" "─┤"
borderRow Rounded Bottom  = mkBorderRow Rounded "─┴─" "╰─" "─╯"
borderRow Angular Top     = mkBorderRow Angular "─┬─" "┌─" "─┐"
borderRow Angular Divider = mkBorderRow Angular "─┼─" "├─" "─┤"
borderRow Angular Bottom  = mkBorderRow Angular "─┴─" "└─" "─┘"
borderRow ASCII   Top     = mkBorderRow ASCII   "-+-" "/-" "-\\"
borderRow ASCII   Divider = mkBorderRow ASCII   "-+-" "+-" "-+"
borderRow ASCII   Bottom  = mkBorderRow ASCII   "-+-" "\\-" "-/"

-- | Common helper function for @borderRow@ that adds a component consisting
-- entirely of horizontal lines.
mkBorderRow :: LineStyle -> T.Text -> Format T.Builder T.Builder -> Format T.Builder T.Builder -> Row a
mkBorderRow ASCII = Row $ spanCell '-'
mkBorderRow _     = Row $ spanCell '─'

-- | Component that takes a single character and outputs a cell that repeats
-- that character to fill the cell width specified in the @FrameProperties@.
spanCell :: Char -> Component a
spanCell c fp = later . (\t _ -> T.fromString t) . replicate (fromIntegral $ cellWidth fp) $ c


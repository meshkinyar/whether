{-# LANGUAGE OverloadedStrings #-}

module Whether.Components where

import Data.Int
import Formatting

import Whether.Weather
import Whether.Frame
import Whether.Display
import Whether.Formatters

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T

dcomponent :: (FrameProperties -> Format r (Forecast -> r)) -> Component r
dcomponent f = Dynamic $ \fp -> f fp

-- formatCenterLeft :: Int -> Format T.Builder (a -> T.Builder) -> a -> T.Builder
-- formatCenterLeft cw = formatPadded leftLength rightLength
--   where
--     leftLength  = cw `div` 2
--     rightLength = leftLength + (cw `mod` 2)
-- 
-- formatCenterRight :: Int -> Format T.Builder (a -> T.Builder) -> a -> T.Builder
-- formatCenterRight cw = formatPadded leftLength rightLength
--   where
--     leftLength  = leftLength + (cw `mod` 2)
--     rightLength = cw `div` 2
-- 
-- formatPadded :: Int -> Int -> Format T.Builder (a -> T.Builder) -> a -> T.Builder
-- formatPadded l r content record = padBuilder l <> bformat content record <> padBuilder r
--   where
--     padBuilder w = T.fromLazyText $ T.replicate (fromIntegral l) " "

status :: Forecast -> T.Text
status = format (wc Symbolic <> temp >+% rH Symbolic <+> humid <+> mp Symbolic)

dayComponent :: Component r 
dayComponent fp = cpadded (fromIntegral $ cellWidth fp) ' '
  $ datetime (dtStyle fp)

uviComponent :: Component r
uviComponent fp = rightPadded fp
  $ uv (contentStyle fp) % colPad fp %> uvi (displayMode fp)

tempComponent :: Component r
tempComponent fp = rightPadded fp
  $ te (contentStyle fp) % colPad fp %> temp

tempsComponent :: Component r
tempsComponent fp = rightPadded fp
  $ te (contentStyle fp) % colPad fp %> tempL >+% "-" <+> tempH

humidityComponent :: Component r
humidityComponent fp = rightPadded fp
  $ rH (contentStyle fp) % colPad fp %> humid

conditionComponent :: Component r
conditionComponent fp = rightPadded fp
  $ wc (contentStyle fp) <> colPad fp %> condition (displayMode fp)

windComponent :: Component r
windComponent fp = rightPadded fp
  $ wd (contentStyle fp) <> colPad fp %> wind (displayMode fp)

colPad :: FrameProperties -> Format r r
colPad fp = pad (displayMode fp)
  where
    pad Compact  = " "
    pad Expanded = "  "

rightPadded :: FrameProperties -> Format r (a -> r) -> Format r (a -> r)
rightPadded fp = rpadded (fromIntegral $ cellWidth fp) ' '



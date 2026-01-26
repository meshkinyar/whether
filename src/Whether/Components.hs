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

status :: Forecast -> T.Text
status = format (wc Symbolic <> temp >+% rH Symbolic <+> humid <+> mp Symbolic)

uviComponent :: FrameProperties -> Format r (Forecast -> r)
uviComponent fp = uv' % "  " %> uvi'
  where
    uv'  = uv (contentStyle fp)
    uvi' = uvi (displayMode fp)

dayHeader :: FrameProperties -> Forecast -> T.Builder
dayHeader fp = formatCenterLeft (cellWidth fp) (datetime (dtStyle fp))
--  where
--    dtLength = case dayStyle $ dtStyle fp of
--      DayAbbr -> 3
--      _       -> 5
--    leftPadding = T.replicate  $ fromIntegral (cellWidth fp / 2) ' '
--    rightPadding = T.replicate $ fromIntegral (leftPadding + (cellWidth fp `mod` 2))

formatCenterLeft :: Int -> Format T.Builder (a -> T.Builder) -> a -> T.Builder
formatCenterLeft cw content forecast = pad leftLength <> bformat content forecast <> pad rightLength
  where
    pad l = T.fromLazyText $ T.replicate (fromIntegral l) " "
    leftLength = cw `div` 2
    rightLength = leftLength + (cw `mod` 2)

{-# LANGUAGE OverloadedStrings #-}

module Whether.Components where

import Formatting

import Whether.Weather
import Whether.Display
import Whether.Formatters

import qualified Data.Text as S

status :: Forecast -> S.Text
status = sformat (wc Symbolic <> temp >+% rH Symbolic <+> humid <+> mp Symbolic)

uviComponent :: Forecast -> S.Text
uviComponent = sformat (uv Symbolic % "  " %> uvI Expanded)

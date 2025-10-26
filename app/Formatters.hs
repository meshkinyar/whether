{-# LANGUAGE OverloadedStrings #-}

module Formatters where

import Types
import Display
import Formatting.Internal
import qualified Types as DF (DailyForecast(condition, temperature, humidity, uvi))
import qualified Data.Text.Lazy.Builder as T

sy :: Symbol a => Format r (a -> r)
sy = later (T.fromText . fSymbol)

wc :: ContentStyle -> Format r (DailyForecast -> r)
wc style = later $ T.fromText . fmt . DF.condition
  where
    fmt = case style of
      Textual -> display
      Symbolic -> fSymbol

tempL :: Format r (DailyForecast -> r)
tempL = later $ T.fromText . display . fst . DF.temperature

tempH :: Format r (DailyForecast -> r)
tempH = later $ T.fromText . display . snd . DF.temperature

rH :: Format r (DailyForecast -> r)
rH = later $ T.fromText . display . DF.humidity

uvi :: Format r (DailyForecast -> r)
uvi = later $ T.fromText . display . DF.uvi



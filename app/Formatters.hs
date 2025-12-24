{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Formatters where

import Types
import Display
import Formatting.Internal
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text as S

(<+>) :: Format r (a -> r) -> Format r (a -> r) -> Format r (a -> r)
f <+> g = Format (\k a -> runFormat f (\b1 -> runFormat g (\b2 -> k (b1 <> T.singleton ' ' <> b2)) a) a)

wc :: ContentStyle -> Format r (Forecast -> r)
wc style = later $ T.fromText . fmt . weatherCondition
  where
    fmt = case style of
      Textual  -> const "WC"
      Symbolic -> fSymbol

uv :: ContentStyle -> Format r (Forecast -> r)
uv style = later $ T.fromText . fmt . uvIndex
  where
    fmt = case style of
      Textual  -> const "UV"
      Symbolic -> (\_ -> fSymbol SunFlat)

wd :: ContentStyle -> Format r (Forecast -> r)
wd style = later $ T.fromText . fmt . getCD . windVelocity
  where
    fmt = case style of
      Textual  -> display Basic
      Symbolic -> fSymbol
    getCD (Just (WindVelocity cd _)) = Just cd
    getCD Nothing = Nothing

rH :: ContentStyle -> Format r (Forecast -> r)
rH style = later $ T.fromText . fmt . h
  where
    fmt = case style of
      Textual  -> const "rH"
      Symbolic -> \_ -> fSymbol DropletWide
    h DailyForecast{} = id
    h CurrentWeather{} = id 

te :: ContentStyle -> Format r (Forecast -> r)
te style = later $ T.fromText . fmt . temperature
  where
    fmt = case style of
      Textual  -> const "T "
      Symbolic -> \_ -> fSymbol Thermometer

mp :: ContentStyle -> Format r (Forecast -> r)
mp style = later $ T.fromText . fmt . moon
  where
    fmt = case style of
      Textual -> const "MP"
      Symbolic -> fSymbol

condition :: DisplayStyle -> Format r (Forecast -> r)
condition style = later $ T.fromText . display style . weatherCondition

temp :: Format r (Forecast -> r)
temp = later $ T.fromText . display Basic . t
  where
    t DailyForecast{temperatureRange} = fst temperatureRange
    t CurrentWeather{temperature} = temperature

tempL :: Format r (Forecast -> r)
tempL = later $ T.fromText . display Basic . t
  where
    t DailyForecast{temperatureRange} = fst temperatureRange
    t CurrentWeather{} = undefined

tempH :: Format r (Forecast -> r)
tempH = later $ T.fromText . display Basic . t
  where
    t DailyForecast{temperatureRange} = snd temperatureRange
    t CurrentWeather{} = undefined


humid :: Format r (Forecast -> r)
humid = later $ T.fromText . display Basic . h
  where
    h DailyForecast{humidity} = humidity
    h CurrentWeather{humidity} = humidity

uvi :: Format r (Forecast -> r)
uvi = later $ T.fromText . display Basic . u
  where
    u DailyForecast{uvIndex} = uvIndex
    u CurrentWeather{} = undefined

wind :: Format r (Forecast -> r)
wind = later $ T.fromText . display Basic . w
  where
    w DailyForecast{windVelocity} = windVelocity
    w CurrentWeather{} = undefined

cwtr :: Forecast
cwtr = CurrentWeather (Just PartlyCloudy) (Fahrenheit 54) (RelativeHumidity 55) (Just WaningGibbous)

status :: Forecast -> S.Text
status = sformat (wc Symbolic <> temp <+> rH Symbolic <+> humid <+> mp Symbolic)


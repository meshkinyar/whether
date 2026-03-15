{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Whether.Config where

import Optics
import GHC.Generics             ( Generic                      )
import Data.Aeson               ( ToJSON, FromJSON             )
import Data.Maybe               ( fromMaybe                    )
import Text.Read                ( readMaybe                    )
import Toml.Schema
import Toml.Schema.FromValue    ( typeError                    )
import Data.Text.Encoding       ( encodeUtf8                   )
import Data.Time.Zones
import Data.Time.Zones.All
import qualified Data.Text as S ( Text, show, unpack           )

import Whether.Display          ( GlyphStyle(Symbolic)         )
import Whether.Display.Frame    ( LineStyle                    )
import Whether.Units            ( UnitSystem
                                , TimeNotation(TwentyFourHour)
                                , CurrentStyle(HourMinute)
                                , DayStyle(DayAbbr)
                                )

-- | Represents configuration values used by this package.
-- Conversion from @ConfigFile@ assigns defaults to any Nothing value.
data Config = 
  Config
    { location       :: S.Text
    , unitSystem     :: UnitSystem
    , timeNotation   :: TimeNotation
    , lineStyle      :: LineStyle
    , glyphStyle     :: GlyphStyle
    , dayStyle       :: DayStyle
    , currentStyle   :: CurrentStyle
    , timezone       :: TZ
    , openWeatherMap :: Maybe OWMConfig
    }
  deriving (Eq, Show, Generic)
 
-- | Represents the config file on disk, with some fields being optional.
data ConfigFile = 
  ConfigFile
    { location       :: S.Text
    , unitSystem     :: UnitSystem
    , timeNotation   :: TimeNotation
    , lineStyle      :: LineStyle
    , glyphStyle     :: Maybe GlyphStyle
    , dayStyle       :: Maybe DayStyle
    , currentStyle   :: Maybe CurrentStyle
    , timezone       :: Maybe TZ
    , openWeatherMap :: Maybe OWMConfig
    }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable ConfigFile

newtype OWMConfig = OWMConfig { apiKey :: S.Text }
  deriving (Eq, Show, Generic)
  deriving (ToValue, FromValue) via GenericTomlTable OWMConfig

instance FromValue GlyphStyle where
  fromValue = fromValueGround "Symbolic | Textual"
instance ToValue GlyphStyle where
  toValue = Text . S.show

instance FromValue DayStyle where
  fromValue = fromValueGround "DayAbbr | DayMonth | MonthDay"
instance ToValue DayStyle where
  toValue = Text . S.show

instance FromValue CurrentStyle where
  fromValue = fromValueGround "HourMinute | DayHourMinute | MonthDayHourMinute | YearMonthDayHourMinute"
instance ToValue CurrentStyle where
  toValue = Text . S.show

instance FromValue UnitSystem where
  fromValue = fromValueGround "Metric | Imperial | Standard"
instance ToValue UnitSystem where
  toValue = Text . S.show

instance FromValue TimeNotation where
  fromValue = fromValueGround "TwelveHour | TwentyFourHour"
instance ToValue TimeNotation where
  toValue = Text . S.show

instance FromValue LineStyle where
  fromValue = fromValueGround "Rounded | Angular"
instance ToValue LineStyle where
  toValue = Text . S.show

instance FromValue TZ where
  fromValue = parseTZ 
    where
      parseTZ v@(Text' _ name) = ensureTZ v $ tzByLabel <$> fromTZName (encodeUtf8 name)
      ensureTZ _ (Just tz) = pure tz
      ensureTZ v@(Text' _ name) Nothing = typeError ("Specified timezone" <> S.unpack name <> "is not recognized as a valid tz identifier.") v
instance ToValue TZ where
  toValue = Text . S.show

-- | Converts a @ConfigFile@ to a @Config@, adding default values
-- where the user has not provided a value.
toConfig :: TZ -> ConfigFile -> Config
toConfig tz cf =
  Config
    { location       = cf ^. #location
    , unitSystem     = cf ^. #unitSystem
    , timeNotation   = cf ^. #timeNotation
    , lineStyle      = cf ^. #lineStyle
    , glyphStyle     = fromMaybe Symbolic (cf ^. #glyphStyle)
    , dayStyle       = fromMaybe DayAbbr (cf ^. #dayStyle)
    , currentStyle   = fromMaybe HourMinute (cf ^. #currentStyle)
    , timezone       = fromMaybe tz (cf ^. #timezone)
    , openWeatherMap = cf ^. #openWeatherMap
    }

fromValueGround :: (FromValue a, Read a) => String -> Value' l -> Matcher l a
fromValueGround err v@(Text' _ t) = fromValue' . readMaybe . S.unpack $ t
  where
    fromValue' (Just t) = pure t
    fromValue' Nothing  = typeError err v

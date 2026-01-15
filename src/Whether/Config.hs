{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Whether.Config where

import GHC.Generics             ( Generic            )
import Data.Aeson               ( ToJSON, FromJSON   )
import Text.Read                ( readMaybe          )
import Toml.Schema
import Toml.Schema.FromValue    ( typeError          )
import qualified Data.Text as S ( Text, show, unpack )

import Whether.Frame            ( LineStyle          )
import Whether.Units            ( UnitSystem         )

data Config = 
  Config
    { location       :: S.Text
    , unitSystem     :: UnitSystem
    , lineStyle      :: LineStyle
    , openWeatherMap :: Maybe OWMConfig
    }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

newtype OWMConfig = OWMConfig { apiKey :: S.Text }
  deriving (Eq, Show, Generic)
  deriving (ToValue, FromValue) via GenericTomlTable OWMConfig

instance FromValue UnitSystem where
  fromValue = fromValueGround "Metric | Imperial | Standard"
instance ToValue UnitSystem where
  toValue = Text . S.show

instance FromValue LineStyle where
  fromValue = fromValueGround "Rounded | Angular"
instance ToValue LineStyle where
  toValue = Text . S.show

fromValueGround :: (FromValue a, Read a) => String -> Value' l -> Matcher l a
fromValueGround err v@(Text' _ t) = fromValue' $ readMaybe $ S.unpack t
  where
    fromValue' (Just t) = pure t
    fromValue' Nothing  = typeError err v

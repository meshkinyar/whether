{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}

module Whether.Display.Forecasts where

import Optics
import GHC.Generics
import Data.Maybe
import Whether.Weather
import Whether.Units
import Whether.Display
import Whether.Display.Components
import Whether.Display.Frame as Frame
import Whether.Config as Config

-- | A frame used to format a detailed weather forecast.
detailFrame :: Config -> Frame Forecast
detailFrame config = Frame fp rows
  where
    fp = FrameProperties 
      { lineStyle   = config ^. #lineStyle
      , dtStyle     = DTStyle
        { timeNotation = config ^. #timeNotation
        , dayStyle     = config ^. #dayStyle
        , currentStyle = config ^. #currentStyle
        }
      , glyphStyle  = config ^. #glyphStyle
      , displayMode = Expanded
      , cellWidth   = 17
      , cellCount   = 3
      }
    rows = 
      [ border  Top
      , content dayComponent
      , border  Divider
      , content tempsComponent
      , content humidityComponent
      , content conditionComponent
      , content windComponent
      , content uviComponent
      , border  Bottom
      ]

    border  = borderRow (fp ^. #lineStyle)
    content = contentRow (fp ^. #lineStyle)

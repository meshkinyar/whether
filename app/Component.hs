{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Component where

import qualified Data.Text as S

class Dynamic a where
    resolve :: ComponentOptions -> a -> S.Text

data Frame = Frame FrameParameters [Component]

data FrameParameters = FrameParameters
    {
      columnN     :: Int
    , columnWidth :: Int
    , divIx       :: Int
    , style       :: FrameStyle
    }

data ComponentOptions = ComponentOptions
    { 
      displayType :: DisplayType
    }

data DisplayType = Textual | Symbolic

data FrameStyle = Rounded

data Segment = Static S.Text
             | forall a. (Dynamic a) => Dynamic a

data Component = Component Int [Segment]

-- glyph :: Text -> S.Text



-- wcComponent = glyph "WC" <> wcText <> 

-- tempComponent = glyph "T" <> tempH <> txt "-" <> tempL

{- completeForecast = do
 - wcComponent
 - tempComponent
 -
 -}


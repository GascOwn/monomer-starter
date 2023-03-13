{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Monomer.Lens as L
import Monomer.Core.Lens (HasFontSize(fontSize))

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = hstack [vstack [
      label "Hello world",
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease
      ],
      spacer,
      vstack_ [childSpacing_ 20] [
        label "this is a line",
        label "this is a another line"
      ]
    ] `styleBasic` [padding 10],
    widgetIf (model ^. clickCount >= 5) (label "STOP FUCKING CLICKING!") `styleBasic` styles model]

handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Home Management",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0


moreThanTen :: AppModel -> StyleState
moreThanTen model = styleIf (model ^. clickCount >= 10) (textColor red)

moreThanFifteen :: AppModel -> StyleState
moreThanFifteen model = styleIf (model ^. clickCount >= 15) (textSize 30)

styles :: AppModel -> [StyleState]
styles x = [moreThanTen x, moreThanFifteen x]
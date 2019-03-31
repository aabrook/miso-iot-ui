{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Login
  ( Action
  , Model
  , update
  , view
  , initModel
  ) where

import qualified Data.ByteString.Lazy
import qualified Data.JSString

import qualified Miso
import Miso
  ( App(App)
  , Effect
  , View
  , button_
  , defaultEvents
  , div_
  , noEff
  , onClick
  , onChange
  , startApp
  , text
  , input_
  , value_
  , (<#)
  )
import Miso.String (MisoString
  , ms
  , fromMisoString
  )

import Pings (query)

data Action
  = Clear
  | Update MisoString
  | Login
  | ShowPings
  | SetPings (Maybe String)
  | NoOp
  deriving (Show, Eq)

data Model
  = Model
  { bearer :: MisoString
  , placeholder :: MisoString
  , pingResult :: String
  }
  deriving (Show, Eq)

initModel :: Model
initModel = Model { bearer = "", placeholder = "", pingResult = mempty }

update :: Action -> Model -> Effect Action Model
update Clear model = noEff $ model { bearer = "" }
update (Update b) model = noEff $ model { bearer = b }
update Login model = noEff $ model { placeholder = bearer model }
update NoOp model = noEff model
update ShowPings m = m <# do
  SetPings <$> query (fromMisoString $ bearer m)
update (SetPings p) m = noEff $ m { pingResult = res p }
  where
    res (Just v) = v
    res Nothing = pingResult m

view :: Model -> View Action
view model = div_
  []
  [ input_ [ onChange Update, value_ $ bearer model ]
  , button_ [ onClick Login ] [ text "Login" ]
  , button_ [ onClick Clear ] [ text "Clear" ]
  , text (placeholder model)
  , button_ [ onClick ShowPings ] [ text "Show Pings" ]
  , text . ms $ pingResult model
  ]

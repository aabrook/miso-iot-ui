{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Lens
  ( makeLenses
  , over
  , set
  , (^.)
  , (%=)
  )


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
  { _bearer :: MisoString
  , _placeholder :: MisoString
  , _pingResult :: String
  }
  deriving (Show, Eq)

makeLenses ''Model

initModel :: Model
initModel = Model { _bearer = mempty, _placeholder = mempty, _pingResult = mempty }

update :: Action -> Model -> Effect Action Model
update Clear model        = noEff $ set bearer "" model
update (Update b) model   = noEff $ set bearer b model
update Login model        = noEff $ set placeholder (model ^. bearer) model
update NoOp model         = noEff model
update ShowPings m        = m <# do
                              SetPings <$> query (fromMisoString $ _bearer m)
update (SetPings p) m     = noEff $ m { _pingResult = res p }
  where
    res (Just v)  = v
    res Nothing   = m ^. pingResult

view :: Model -> View Action
view model = div_
  []
  [ input_ [ onChange Update, value_ $ _bearer model ]
  , button_ [ onClick Login ] [ text "Login" ]
  , button_ [ onClick Clear ] [ text "Clear" ]
  , text (_placeholder model)
  , button_ [ onClick ShowPings ] [ text "Show Pings" ]
  , text . ms $ _pingResult model
  ]

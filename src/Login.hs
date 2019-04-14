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

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy
import qualified Data.JSString
import Data.Bifunctor (bimap)
import Control.Monad.Reader
import Control.Lens
  ( makeLenses
  , over
  , set
  , (^.)
  , (%=)
  )
import Data.Map
  ( Map
  , fromList
  , lookup
  , singleton
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
  , style_
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

styles = fromList
  [ ("margin", "15px auto")
  , ("width", "50%")
  , ("border-radius", "15px")
  , ("box-shadow", "0 -3px 31px 0 rgba(0, 0, 0, 0.05), 0 6px 20px 0 rgba(0, 0, 0, 0.02)")
  , ("background-color", "#f9f9f9")
  , ("padding", "15px")
  ]

buttonStyles = fromList
  [ ("margin", "auto")
  , ("width", "80px")
  ]

inputStyles = fromList [("width", "180px")] <> buttonStyles

btn :: Map String (Map MisoString MisoString) -> Map MisoString MisoString
btn mp = maybe mempty id $ lookup ("btn") mp

baseView :: Map String (Map MisoString MisoString) -> Model -> View Action
baseView styling model = div_
    [ style_ styles ]
    [ div_ [style_ inputStyles] [input_ [ onChange Update, value_ $ _bearer model ]]
    , div_ [ style_ buttonStyles ] [button_ [ onClick Login ] [ text "Login" ]]
    , div_ [ style_ buttonStyles ] [button_ [ onClick Clear ] [ text "Clear" ]]
    , text (_placeholder model)
    , button_ [ onClick ShowPings ] [ text "Show Pings" ]
    , text . ms $ _pingResult model
    ]
view :: Reader (Map String (Map MisoString MisoString)) (Model -> View Action)
view = do
  styling <- ask
  return $ baseView styling

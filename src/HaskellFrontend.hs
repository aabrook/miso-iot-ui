{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HaskellFrontend
  ( main
  ) where

import Data.Bifunctor (bimap)
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
  , startApp
  , text
  )
import Miso.String (ms)

import Login as L
  ( Model
  , Action
  , update
  , view
  , initModel
  )

import Pings

data Model_ = Model_
  { counter :: Int
  , login :: L.Model
  }
  deriving (Show, Eq)

data Action_
  = AddOne
  | SubtractOne
  | NoOp
  | LoginActions L.Action
  deriving (Show, Eq)

main :: IO ()
main =
  startApp App
    { Miso.initialAction = NoOp
    , Miso.model         = Model_ { counter = 0, login = L.initModel }
    , Miso.update        = HaskellFrontend.update
    , Miso.view          = HaskellFrontend.view
    , Miso.events        = defaultEvents
    , Miso.subs          = []
    , Miso.mountPoint    = Nothing
    }

update :: Action_ -> Model_ -> Effect Action_ Model_
update AddOne model = noEff $ model { counter = ((counter model) + 1) }
update SubtractOne model = noEff $ model { counter = ((counter model) - 1) }
update NoOp m = noEff m
update (LoginActions actions) m = updated
  where
    result = L.update actions (login m)
    updated = bimap LoginActions (\e -> m { login = e }) result

view :: Model_ -> View Action_
view x = div_
  []
  [ button_ [onClick AddOne] [text "+"]
  , text (ms $ counter x)
  , button_ [onClick SubtractOne] [text "-"]
  , LoginActions <$> (L.view $ login x)
  ]

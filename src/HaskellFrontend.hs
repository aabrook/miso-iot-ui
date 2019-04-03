{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module HaskellFrontend
  ( main
  ) where

import Data.Bifunctor (bimap)
import Control.Lens as Lens
  ( set
  , view
  , makeLenses
  , (^.)
  , (.=)
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
  { _login :: L.Model
  }
  deriving (Show, Eq)

makeLenses ''Model_

data Action_
  = NoOp
  | LoginActions L.Action
  deriving (Show, Eq)

main :: IO ()
main =
  startApp App
    { Miso.initialAction = NoOp
    , Miso.model         = Model_ { _login = L.initModel }
    , Miso.update        = HaskellFrontend.update
    , Miso.view          = HaskellFrontend.view
    , Miso.events        = defaultEvents
    , Miso.subs          = []
    , Miso.mountPoint    = Nothing
    }

update :: Action_ -> Model_ -> Effect Action_ Model_
update NoOp m                   = noEff m
update (LoginActions actions) m = loginUpdate m
  where
    setLogin e    = Lens.set login e m
    runUpdate m   = L.update actions (m ^. login)
    loginUpdate m = bimap LoginActions setLogin (runUpdate m)

view :: Model_ -> View Action_
view x = div_
  []
  [ LoginActions <$> (L.view $ Lens.view login x)
  ]

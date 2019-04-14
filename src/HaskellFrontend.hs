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
import Control.Monad.Reader
import Data.Map
  ( Map
  , fromList
  )
import qualified Miso
import Miso
  ( App(App)
  , Effect
  , View
  , button_
  , defaultEvents
  , div_
  , style_
  , noEff
  , onClick
  , startApp
  , text
  )
import Miso.String (MisoString, ms)

import Login as L
  ( Model
  , Action
  , update
  , view
  , initModel
  )

import Pings
import Styles.Reference

data Model_ = Model_
  { _login :: L.Model
  }
  deriving (Show, Eq)

-- newtype AvailableStyles = Reader (Map String (Map String String))

makeLenses ''Model_

data Action_
  = NoOp
  | LoginActions L.Action
  deriving (Show, Eq)

buttonStyles = fromList $ toMsStyle <$>
  [ MarginLeft Auto
  , MarginRight Auto
  , Width (Px 80)
  ]

inputStyles = (fromList $ (toMsStyle <$> [Width (Px 180)])) <> buttonStyles

baseStyles :: Map String (Map MisoString MisoString)
baseStyles = fromList [
    ("btn", fromList
      $ toMsStyle <$> [
        MarginLeft Auto
      , MarginRight Auto
      , Width (Px 80)
      , BackgroundColor (Raw "gray")
    ])
    , ("input", inputStyles)]

boxStyle = toMsStyle <$> [
  BackgroundColor (Hex "505458")
  , MinHeight (Px 130)
  , Padding (Px 15) Blank Blank Blank
  ]

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
  [ style_ $ fromList boxStyle ]
  [ div_ [] $ [ LoginActions <$> ((runReader (L.view >>= \f -> return $ f $ x ^. login)) baseStyles) ]
  ]

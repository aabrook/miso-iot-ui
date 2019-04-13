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
import Miso.String (ms)

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

styles = fromList
  [ ("margin", "15px auto")
  , ("width", "50%")
  , ("border-radius", "15px")
  , ("box-shadow", "0 -3px 31px 0 rgba(0, 0, 0, 0.05), 0 6px 20px 0 rgba(0, 0, 0, 0.02)")
  , ("background-color", "#f9f9f9")
  , ("padding", "15px")
  ]

buttonStyles = fromList $ toStyle <$>
  [ MarginLeft Auto
  , MarginRight Auto
  , Width (Px 80)
  ]

inputStyles = (fromList $ (toStyle <$> [Width (Px 180)])) <> buttonStyles

baseStyles :: Map String (Map String String)
baseStyles = fromList [
    ("btn", fromList
      $ toStyle <$> [
        MarginLeft Auto
      , MarginRight Auto
      , Width (Px 80)
      , BackgroundColor (Raw "gray")
    ])
    , ("input", inputStyles)]

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
  [ style_ $ fromList [("background-color", "#505458"), ("min-height", "130px"), ("padding", "15px")]]
  [ div_ [] $ [ LoginActions <$> ((runReader (L.view >>= \f -> return $ f $ x ^. login)) baseStyles) ]
  ]

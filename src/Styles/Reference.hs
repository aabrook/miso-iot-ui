module Styles.Reference where

data Length =
  Auto
  | None
  | Px Int
  | Pt Int
  | Percentage Int
  | Cm Float
  | Mm Int
  | In Float
  | Pc Int
  | Em Float
  | Ex Float
  | Ch Float
  | Rem Float
  | Vw Float
  | Vh Float
  | Vmin Float
  | Vmax Float

instance Show Length where
  show Auto = "auto"
  show None = "none"
  show (Px n) = (show n) <> "px"
  show (Pt n) = (show n) <> "pt"
  show (Percentage n) = (show n) <> "%"
  show (Cm n) = (show n) <> "cm"
  show (Mm n) = (show n) <> "mm"
  show (In n) = (show n) <> "in"
  show (Pc n) = (show n) <> "pc"
  show (Em n) = (show n) <> "em"
  show (Ex n) = (show n) <> "ex"
  show (Ch n) = (show n) <> "ch"
  show (Rem n) = (show n) <> "rem"
  show (Vw n) = (show n) <> "vw"
  show (Vh n) = (show n) <> "vh"
  show (Vmin n) = (show n) <> "vmin"
  show (Vmax n) = (show n) <> "vmax"

data Color =
  Hex String
  | Rgb (String, String, String)
  | Rgba (String, String, String, String)
  | Hsl (String, String, String)
  | Hsla (String, String, String, String)
  | Raw String

instance Show Color where
  show (Hex s) = "#" <> s
  show (Rgb s) = "rgb" <> (show s)
  show (Rgba s) = "rgba" <> (show s)
  show (Hsl s) = "hsl" <> (show s)
  show (Hsla s) = "hsla" <> (show s)
  show (Raw s) = s

data Reference = MarginLeft Length
  | MarginRight Length
  | MarginTop Length
  | MarginBottom Length
  | Width Length
  | BackgroundColor Color

toStyle :: Reference -> (String, String)
toStyle (MarginLeft l)      = ("margin-left", show l)
toStyle (MarginRight l)     = ("margin-right", show l)
toStyle (MarginTop l)       = ("margin-top", show l)
toStyle (MarginBottom l)    = ("margin-bottom", show l)
toStyle (BackgroundColor c) = ("background-color", show c)

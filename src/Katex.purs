module Katex
  ( Accent(..)
  , Settings
  , Macros
  , Operator(..)
  , Expr(..)
  , Delim(..)
  , Delimiter(..)
  , Side(..)
  , _renderToStringNullable
  , defaultOptions
  , toggleDisplay
  , render
  , inline
  , display
  , parse
  )
  where

import Prelude
import Effect (Effect)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign
import Web.DOM (Element)
import Data.Map (Map, empty)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Attribute ((!:=))

data Operator
  = Sum
  | Product
  | Integral
  | IInt
  | IIInt
  | OInt
  | OIInt
  | OIIInt
  | Bigotimes
  | Bigoplus
  | Bigodot
  | Biguplus
  | Bigvee
  | Bigwedge
  | Bigcap
  | Bigcup
  | Bigsqcup

instance showOperator :: Show Operator where
  show Sum = "\\sum"
  show Product = "\\prod"
  show Integral = "\\int"
  show IInt = "\\iint"
  show IIInt = "\\iiint"
  show OInt = "\\oint"
  show OIInt = "\\oiint"
  show OIIInt = "\\oiiint"
  show Bigotimes = "\\bigotimes"
  show Bigoplus = "\\bigoplus"
  show Bigodot = "\\bigodot"
  show Biguplus = "\\biguplus"
  show Bigvee = "\\bigvee"
  show Bigwedge = "\\bigwedge"
  show Bigcap = "\\bigcap"
  show Bigcup = "\\bigcup"
  show Bigsqcup = "\\bigsqcup"

data Accent
  = Prime
  | DoublePrime
  | Acute
  | Bar
  | Dot
  | DDot
  | Grave
  | Hat
  | WideHat
  | Tilde
  | WideTilde
  | UTilde
  | Vec
  | OverLeftArrow
  | UnderLeftArrow
  | OverRightArrow
  | UnderRightArrow
  | OverLeftHarpoon
  | OverRightHarpoon
  | OverRightArrowBig
  | Overline
  | Underline
  | WideCheck
  | Mathring
  | Overgroup
  | Undergroup
  | Overbrace
  | Underbrace
  | Overlinesegment
  | Underlinesegment
  | Underbar

data Limits
  = UpperLimit String
  | LowerLimit String
  | Open String
  | Boundary String
  | Interval String String

instance showLimits :: Show Limits where
  show (UpperLimit s) = "^{" <> s <> "}"
  show (LowerLimit s) = "_{" <> s <> "}"
  show (Open s) = s
  show (Boundary s) = "_{\\partial " <> s <> "}"
  show (Interval a b) = "[" <> a <> ", " <> b <> "]"

data Side = L | R

data Delim
  = Paren
  | Ceil
  | Arrow
  | Brack
  | Floor
  | Brace
  | Moustache
  | Angle
  | Group
  | UCorner
  | LCorner
  | Vert
  | VVert
  | Ang
  | Bracket
  | BBrace

derive instance Ord Delim
derive instance Eq Delim

instance Show Delim where
  show Paren = "paren"
  show Ceil = "ceil"
  show Arrow = "Arr"
  show Brack = "brack"
  show Floor = "floor"
  show Brace = "brace"
  show Moustache = "moustache"
  show Angle = "angle"
  show Group = "group"
  show UCorner = "corner"
  show LCorner = "corner"
  show Vert = "vert"
  show VVert = "Vert"
  show Ang = "ang"
  show Bracket = "bracket"
  show BBrace = "Brace"

data Delimiter = Delimiter Side Delim

instance Show Delimiter where
  show (Delimiter R  UCorner) = "\\ulcorner"
  show (Delimiter L  UCorner) = "\\urcorner"
  show (Delimiter R  LCorner) = "\\llcorner"
  show (Delimiter L  LCorner) = "\\lrcorner"
  show (Delimiter L  Bracket) = "\\llbracket"
  show (Delimiter R  Bracket) = "\\rrbracket"
  show (Delimiter R  Arrow) = "\\uparrow"
  show (Delimiter L  Arrow) = "\\downarrow"
  show (Delimiter R  delim) = "\\r" <> show delim
  show (Delimiter L  delim) = "\\l" <> show delim

data Expr 
  = Acc Accent Expr
  | Str String

instance Show Expr where
  show (Acc a exp) = show a <> "{" <> show exp <> "}"
  show (Str s) = s


instance showAccent :: Show Accent where
  show Prime = "'"
  show DoublePrime = "''"
  show Acute = "\\acute"
  show Bar = "\\bar"
  show Dot = "\\dot"
  show DDot = "\\ddot"
  show Grave = "\\grave"
  show Hat = "\\hat"
  show WideHat = "\\widehat"
  show Tilde = "\\tilde"
  show WideTilde = "\\widetilde"
  show UTilde = "\\utilde"
  show Vec = "\\vec"
  show OverLeftArrow = "\\overleftarrow"
  show UnderLeftArrow = "\\underleftarrow"
  show OverRightArrow = "\\overrightarrow"
  show UnderRightArrow = "\\underrightarrow"
  show OverLeftHarpoon = "\\overleftharpoon"
  show OverRightHarpoon = "\\overrightharpoon"
  show OverRightArrowBig = "\\Overrightarrow"
  show Overline = "\\overline"
  show Underline = "\\underline"
  show WideCheck = "\\widecheck"
  show Mathring = "\\mathring"
  show Overgroup = "\\overgroup"
  show Undergroup = "\\undergroup"
  show Overbrace = "\\overbrace"
  show Underbrace = "\\underbrace"
  show Overlinesegment = "\\overlinesegment"
  show Underlinesegment = "\\underlinesegment"
  show Underbar = "\\underbar"

type Settings =
  { displayMode :: Boolean
  , output :: String
  , leqno :: Boolean
  , fleqn :: Boolean
  , throwOnError :: Boolean
  , errorColor :: String
  , minRuleThickens :: Number
  , colorIsTextColor :: Boolean
  , macros :: Map String String
  --, maxSize :: Number
  --, maxExpand :: Number
  , strict :: Boolean
  , trust :: Boolean
  }

defaultOptions :: Settings
defaultOptions =
  { displayMode: false
  , output: "htmlAndMathMl"
  , leqno: false
  , fleqn: false
  , throwOnError: true
  , errorColor: "#cc0000"
  , minRuleThickens: 0.04
  , colorIsTextColor: true
  , macros: empty
  --, maxSize : Number
  -- , maxExpand : Number
  , strict: false
  , trust: false
  }

toggleDisplay :: Settings -> Settings
toggleDisplay ops = case ops.displayMode of
  true -> (ops { displayMode = false })
  false -> (ops { displayMode = true })

data Macros = Object

render
  :: { config :: Settings
     , katex :: String
     }
  -> Nut
render { config, katex } =
      D.span
        [ D.Self !:= \elt -> do
            renderImpl katex elt config
        ]
        []

display :: String -> Nut
display s =
  D.span
    [ D.Self !:= \elt -> do
        renderImpl s elt (defaultOptions {displayMode = true})
    ]
    []

inline :: String -> Nut
inline s =
  D.span
    [ D.Self !:= \elt -> do
        renderImpl s elt (defaultOptions {displayMode = false})
    ]
    []


foreign import renderImpl :: String -> Element -> Settings -> Effect Unit
foreign import _renderToStringNullable :: String -> { displayMode :: Boolean } -> Effect Unit
foreign import parse :: String -> String

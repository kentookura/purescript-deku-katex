module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=), (!:=))
import Deku.Control (text_, (<#~>))
import Deku.Core (Nut)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Katex as Katex
import Katex (Operator(..), Accent(..), Expr(..), Delim(..), Delimiter(..), Side(..))
import FRP.Event (Event)

main :: Effect Unit
main = runInBody
  Deku.do
    D.span
      [ D.Self !:= \e -> do
          Katex.render "c = \\pm\\sqrt{a^2 + b^2}" e Katex.defaultOptions]
      [operators, accents, delimiters]

render
  :: { config :: Katex.Settings
     , katex :: String
     }
  -> Nut
render { config, katex } =
      D.span
        [ D.Self !:= \elt -> do
            Katex.render katex elt config
        ]
        []

operators :: Nut
operators = 
  D.div_ $
    [ Sum 
    , Product 
    , Integral
    , IInt 
    , IIInt
    , OInt 
    , OIInt
    , OIIInt
    , Bigotimes
    , Bigoplus 
    , Bigodot 
    , Biguplus
    , Bigvee 
    , Bigwedge
    , Bigcap
    , Bigcup
    , Bigsqcup 
    ] # 
    map (\op -> Deku.do
      render {katex: (show op), config: Katex.defaultOptions}
    )

accents :: Nut
accents = 
  D.div_ $
    [ Acc Prime (Str "A")
    , Acc DoublePrime (Str "A")
    , Acc Acute (Str "A")
    , Acc Bar (Str "A")
    , Acc Dot (Str "A")
    , Acc DDot (Str "A")
    , Acc Grave (Str "A")
    , Acc Hat (Str "A")
    , Acc WideHat (Str "A")
    , Acc Tilde (Str "A")
    , Acc WideTilde (Str "A")
    , Acc UTilde (Str "A")
    , Acc Vec (Str "A")
    , Acc OverLeftArrow (Str "A")
    , Acc UnderLeftArrow (Str "A")
    , Acc OverRightArrow (Str "A")
    , Acc UnderRightArrow (Str "A")
    , Acc OverLeftHarpoon (Str "A")
    , Acc OverRightHarpoon (Str "A")
    , Acc OverRightArrowBig (Str "A")
    , Acc Overline (Str "A")
    , Acc Underline (Str "A")
    , Acc WideCheck (Str "A")
    , Acc Mathring (Str "A")
    , Acc Overgroup (Str "A")
    , Acc Undergroup (Str "A")
    , Acc Overbrace (Str "A")
    , Acc Underbrace (Str "A")
    , Acc Overlinesegment (Str "A")
    , Acc Underlinesegment (Str "A")
    , Acc Underbar (Str "A")
    ] # 
    map (\expr -> Deku.do
      render {katex: (show expr), config: Katex.defaultOptions}
    )


delimiters :: Nut
delimiters = 
  D.div_ $
    [ Paren
    , Ceil
    , Arrow
    , Brack
    , Floor
    , Brace
    , Moustache
    , Angle
    , Group
    , UCorner
    , LCorner
    , Vert
    , VVert
    , Ang
    , Bracket
    , BBrace
    ] # (map \delim -> (Delimiter L delim /\ Delimiter R delim))
      # map (\(r /\ l) -> 
      Deku.do
        D.div_ 
          [ render {katex: (show r), config: Katex.defaultOptions}
          , render {katex: (show l), config: Katex.defaultOptions}
          ]
    )

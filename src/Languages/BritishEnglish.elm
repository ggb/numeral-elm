module Languages.BritishEnglish exposing (lang)

{-| British English language configuration.

@docs lang
-}

import Language exposing (..)


englishOrdinal : Ordinal
englishOrdinal number =
    let
        number1 =
            floor number

        b =
            modBy 10 number1
    in
        if floor (toFloat (modBy 100 number1) / 10) == 1 then
            "th"
        else if b == 1 then
            "st"
        else if b == 2 then
            "nd"
        else if b == 3 then
            "rd"
        else
            "th"


{-| Configuration data.

    lang =
      { delimiters=
        { thousands=","
        , decimal="."
        }
      , abbreviations=
        { thousand="k"
        , million="m"
        , billion="b"
        , trillion="t"
        }
      , ordinal=englishOrdinal
      , currency=
        { symbol="£"
        }
      }
-}
lang : Language
lang =
    { delimiters =
        { thousands = ","
        , decimal = "."
        }
    , abbreviations =
        { thousand = "k"
        , million = "m"
        , billion = "b"
        , trillion = "t"
        }
    , ordinal = englishOrdinal
    , currency =
        { symbol = "£"
        }
    }

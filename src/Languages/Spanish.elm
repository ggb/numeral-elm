module Languages.Spanish exposing (lang)

{-| Spanish language configuration.

@docs lang

-}

import Language exposing (..)


spanishOrdinal : Ordinal
spanishOrdinal number =
    let
        number1 =
            floor number

        b =
            modBy 10 number1
    in
    if b == 1 || b == 3 then
        "er"
    else if b == 2 then
        "do"
    else if b == 7 || b == 0 then
        "mo"
    else if b == 8 then
        "vo"
    else if b == 9 then
        "no"
    else
        "to"


{-| Configuration data.

    lang =
        { delimiters =
            { thousands = "."
            , decimal = ","
            }
        , abbreviations =
            { thousand = "k"
            , million = "mm"
            , billion = "b"
            , trillion = "t"
            }
        , ordinal = spanishOrdinal
        , currency =
            { symbol = "€"
            }
        }

-}
lang : Language
lang =
    { delimiters =
        { thousands = "."
        , decimal = ","
        }
    , abbreviations =
        { thousand = "k"
        , million = "mm"
        , billion = "b"
        , trillion = "t"
        }
    , ordinal = spanishOrdinal
    , currency =
        { symbol = "€"
        }
    }

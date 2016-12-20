module Languages.Russian exposing (lang)

{-| Russian language configuration.

@docs lang
-}

import Language exposing (..)


russianOrdinal : Ordinal
russianOrdinal number =
    "."


{-| Configuration data.

    lang =
      { delimiters =
        { thousands = " "
        , decimal = ","
        }
      , abbreviations =
        { thousand = "тыс."
        , million = "млн"
        , billion = "b"
        , trillion = "трлн"
        }
      , ordinal = russianOrdinal
      , currency =
        { symbol = "руб."
        }
    }
-}
lang : Language
lang =
    { delimiters =
        { thousands = " "
        , decimal = ","
        }
    , abbreviations =
        { thousand = "тыс."
        , million = "млн"
        , billion = "b"
        , trillion = "трлн"
        }
    , ordinal = russianOrdinal
    , currency =
        { symbol = "руб."
        }
    }

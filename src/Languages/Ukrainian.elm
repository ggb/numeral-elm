module Languages.Ukrainian exposing (lang)

{-| Ukrainian language configuration.

@docs lang
-}

import Language exposing (..)


ukrainianOrdinal : Ordinal
ukrainianOrdinal number =
  ""


{-| Configuration data.

    lang =
      { delimiters =
        { thousands = " "
        , decimal = ","
        }
      , abbreviations =
        { thousand = "тис."
        , million = "млн"
        , billion = "млрд"
        , trillion = "блн"
        }
      , ordinal = ukrainianOrdinal
      , currency =
        { symbol = "₴"
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
        { thousand = "тис."
        , million = "млн"
        , billion = "млрд"
        , trillion = "блн"
        }
    , ordinal = ukrainianOrdinal
    , currency =
        { symbol = "₴"
        }
    }

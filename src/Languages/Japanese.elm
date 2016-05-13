module Languages.Japanese exposing(lang)

{-| Japanese language configuration.

@docs lang
-}

import Language exposing (..)


japaneseOrdinal : Ordinal
japaneseOrdinal number =
  "."


{-| Configuration data.

    lang =
      { delimiters=
        { thousands=","
        , decimal="."
        }
      , abbreviations=
        { thousand="千"
        , million="百万"
        , billion="十億"
        , trillion="兆"
        }
      , ordinal=japaneseOrdinal
      , currency=
        { symbol="¥"
        }
      }
-}
lang : Language
lang =
  { delimiters=
    { thousands=","
    , decimal="."
    }
  , abbreviations=
    { thousand="千"
    , million="百万"
    , billion="十億"
    , trillion="兆"
    }
  , ordinal=japaneseOrdinal
  , currency=
    { symbol="¥"
    }
  }

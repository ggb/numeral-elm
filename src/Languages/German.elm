module Languages.German exposing(lang)

{-| German language configuration.

@docs lang
-}

import Language exposing (..)


germanOrdinal : Ordinal
germanOrdinal number =
  "."


{-| Configuration data.

    lang =
      { delimiters=
        { thousands=" "
        , decimal=","
        }
      , abbreviations=
        { thousand="k"
        , million="m"
        , billion="b"
        , trillion="t"
        }
      , ordinal=germanOrdinal
      , currency=
        { symbol="€"
        }
      }
-}
lang : Language
lang =
  { delimiters=
    { thousands=" "
    , decimal=","
    }
  , abbreviations=
    { thousand="k"
    , million="m"
    , billion="b"
    , trillion="t"
    }
  , ordinal=germanOrdinal
  , currency=
    { symbol="€"
    }
  }

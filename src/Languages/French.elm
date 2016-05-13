module Languages.French exposing(lang)

{-| French language configuration.

@docs lang
-}

import Language exposing (..)


frenchOrdinal : Ordinal
frenchOrdinal number =
  if (floor number) == 1 then
    "er"
  else
    "e"


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
      , ordinal=frenchOrdinal
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
  , ordinal=frenchOrdinal
  , currency=
    { symbol="€"
    }
  }

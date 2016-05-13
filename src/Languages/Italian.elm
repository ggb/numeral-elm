module Languages.Italian exposing(lang)

{-| Italian language configuration.

@docs lang
-}

import Language exposing (..)


italianOrdinal : Ordinal
italianOrdinal number =
  "°"


{-| Configuration data.

    lang =
      { delimiters=
        { thousands="."
        , decimal=","
        }
      , abbreviations=
        { thousand="mila"
        , million="mln"
        , billion="mld"
        , trillion="bil"
        }
      , ordinal=italianOrdinal
      , currency=
        { symbol="€"
        }
      }
-}
lang : Language
lang =
  { delimiters=
    { thousands="."
    , decimal=","
    }
  , abbreviations=
    { thousand="mila"
    , million="mln"
    , billion="mld"
    , trillion="bil"
    }
  , ordinal=italianOrdinal
  , currency=
    { symbol="€"
    }
  }

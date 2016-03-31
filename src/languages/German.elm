module Languages.German where

import Language exposing (..)


germanOrdinal : Ordinal
germanOrdinal number =
  "."

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
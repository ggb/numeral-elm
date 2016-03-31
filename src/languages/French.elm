module Languages.French where

import Language exposing (..)


frenchOrdinal : Ordinal
frenchOrdinal number =
  if (floor number) == 1 then
    "er"
  else
    "e"


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
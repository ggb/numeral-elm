module Languages.Japanese where

import Language exposing (..)


japaneseOrdinal : Ordinal
japaneseOrdinal number =
  "."

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
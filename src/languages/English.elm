module Languages.English where

import Language exposing (..)


englishOrdinal : Ordinal
englishOrdinal number =
  let
    number' = floor number
    b = number' % 10
  in
    if floor (toFloat (number' % 100) / 10) == 1 then
      "th"
    else if b == 1 then
      "st"
    else if b == 2 then
      "nd"
    else if b == 3 then
      "rd"
    else
      "th"

lang : Language
lang =
  { delimiters=
    { thousands=","
    , decimal="."
    }
  , abbreviations=
    { thousand="k"
    , million="m"
    , billion="b"
    , trillion="t"
    }
  , ordinal=englishOrdinal
  , currency=
    { symbol="$"
    }
  }
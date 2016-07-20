module Numeral exposing(format, formatWithLanguage)

{-| Elm module for (advanced) number formatting. It is a direct port of [Numeral.js](http://numeraljs.com/) and it is possible to use the same format strings. Manipulation and unformatting of numbers is not yet supported.

If you create a new language-file, please let me know or send a pull request.

# Formatting

@docs format, formatWithLanguage

-}

import String
import Array exposing (Array)
import Regex exposing (HowMany(All), regex)
import Language exposing (..)
import Languages.English as English


type alias NumberTypeFormatter =
  Language -> String -> Float -> String -> String


suffixes : Array String
suffixes =
  ["B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"]
  |> Array.fromList


indexOf : String -> String -> Int
indexOf part word  =
  String.indexes part word
  |> List.head
  |> Maybe.withDefault -1


emptyReplace : String -> String -> String
emptyReplace str =
  Regex.replace All (regex str) (\_ -> "")


formatWithoutCurrency : String -> (String, String)
formatWithoutCurrency format =
  if String.contains " $" format then
    (" ", emptyReplace " \\$" format)
  else if String.contains "$ " format then
    (" ", emptyReplace "\\$ " format)
  else
    ("", emptyReplace "\\$" format)


formatCurrency : NumberTypeFormatter
formatCurrency lang format value strValue =
  let
    symbolIndex = indexOf "$" format
    openParenIndex = indexOf "(" format
    minusSignIndex = indexOf "-" format
    (space, format') = formatWithoutCurrency format
    formatted = formatNumber lang format' value strValue
    currencySymbol = lang.currency.symbol
  in
    if symbolIndex <= 1 then
      if String.contains "(" formatted || String.contains "-" formatted then
        if symbolIndex < openParenIndex || symbolIndex < minusSignIndex then
          [ currencySymbol
          , space
          , if String.contains "-" formatted then "-" else ""
          , if String.contains "(" formatted then "(" else ""
          , String.slice 1 (String.length formatted) formatted
          ] |> String.join ""
        else
          [ if String.contains "-" formatted then "-" else ""
          , if String.contains "(" formatted then "(" else ""
          , currencySymbol
          , space
          , String.slice 1 (String.length formatted) formatted
          ] |> String.join ""
      else
        currencySymbol ++ space ++ formatted
    else
      if String.contains ")" formatted then
        [ String.slice 0 (String.length formatted - 1) formatted
        , space
        , currencySymbol
        , ")"
        ] |> String.join ""
      else
        formatted ++ space ++ currencySymbol


formatWithoutPercent : String -> (String, String)
formatWithoutPercent format =
  if String.contains " %" format then
    (" ", emptyReplace " %" format)
  else
    ("", emptyReplace "%" format)


formatPercentage : NumberTypeFormatter
formatPercentage lang format value strValue =
  let
    value' = value * 100
    (space, format') = formatWithoutPercent format
    formatted = formatNumber lang format' value' (toString value')
  in
    if String.contains ")" formatted then
      [ String.slice 0 (String.length formatted - 1) formatted
      , space
      , "%"
      , ")"
      ] |> String.join ""
    else
      formatted ++ space ++ "%"


formatTime : NumberTypeFormatter
formatTime lang format value strValue =
  let
    hasOneDigit val =
      if String.length val < 2 then
        "0" ++ val
      else
        val
    hours =
      value / 60 / 60
      |> floor
      |> toFloat
    minutes =
      (value - (hours * 60 * 60))/60
      |> floor
      |> toFloat
    seconds =
      (value - (hours * 60 * 60) - (minutes * 60))
      |> round
  in
    [ (hours |> toString)
    , (minutes |> toString |> hasOneDigit)
    , (seconds |> toString |> hasOneDigit)
    ] |> String.join ":"


checkParensAndSign : String -> (String, Bool, Bool)
checkParensAndSign format =
  if String.contains "(" format then
    (String.slice 1 -1 format, True, False)
  else if String.contains "+" format then
    (emptyReplace "\\+" format, False, True)
  else
    (format, False, False)


checkAbbreviation : Language -> String -> Float -> (String, String, Float)
checkAbbreviation lang format value =
  let
    abbrK = String.contains "aK" format
    abbrM = String.contains "aM" format
    abbrB = String.contains "aB" format
    abbrT = String.contains "aT" format
    abbrForce = abbrK || abbrM || abbrB || abbrT |> not
    absValue = abs value
    (abbr, format') =
      if String.contains " a" format then
        (" ", emptyReplace " a" format)
      else
        ("", emptyReplace "a" format)
  in
    if not <| String.contains "a" format then
      (format, "", value)
    else if absValue >= 10^12 && abbrForce || abbrT then
      (format', abbr ++ lang.abbreviations.trillion, value / 10^12)
    else if absValue < 10^12 && absValue >= 10^9 && abbrForce || abbrB then
      (format', abbr ++ lang.abbreviations.billion, value / 10^9)
    else if absValue < 10^9 && absValue >= 10^6 && abbrForce || abbrM then
      (format', abbr ++ lang.abbreviations.million, value / 10^6)
    else if absValue < 10^6 && absValue >= 10^3 && abbrForce || abbrK then
      (format', abbr ++ lang.abbreviations.thousand, value / 10^3)
    else
      (format', abbr, value)


checkByte : String -> Float -> (String, Float, String)
checkByte format value =
  let
    (bytes, format') =
      if String.contains " b" format then
        (" ", emptyReplace " b" format)
      else
        ("", emptyReplace "b" format)

    suffixIndex' power =
      let
        minValue = 1024^power
        maxValue = 1024^(power + 1)
      in
        if value >= minValue && value < maxValue then
          if minValue > 0 then
            (power, value / minValue)
          else
            (power, value)
        else if power < 10 then
          suffixIndex' (power + 1)
        else
            (-1, value)
    (suffixIndex, value') = suffixIndex' 0
    suffix =
      Array.get suffixIndex suffixes
      |> Maybe.withDefault ""
  in
    if String.contains "b" format then
      (format', value', bytes ++ suffix)
    else
      (format, value, "")


checkOrdinal : Language -> String -> Float -> (String, String)
checkOrdinal lang format value =
  let
    (ord, format') =
      if String.contains " o" format then
        (" ", emptyReplace " o" format)
      else
        ("", emptyReplace "o" format)
  in
    if String.contains "o" format then
      (format', ord ++ (value |> lang.ordinal))
    else
      (format, "")


checkOptionalDec : String -> (String, Bool)
checkOptionalDec format =
  if String.contains "[.]" format then
    (Regex.replace All ("[.]" |> Regex.escape |> regex) (\_ -> ".") format, True)
  else
    (format, False)


toFixed : Int -> Float -> String
toFixed precision value =
  let
    power = toFloat 10^(toFloat precision)
    pad num =
      case num of
        [x, y] ->
          [x, String.padRight precision '0' y]
        [val] ->
          [val, String.padRight precision '0' ""]
        val ->
          val
  in
    (round (value * power + 0.01) |> toFloat) / power
    |> toString
    |> String.split "."
    |> pad
    |> String.join "."


toFixedWithOptional : List Int -> Float -> String
toFixedWithOptional prs value =
  case prs of
    [x, y] ->
      toFixed (x + y) value
      |> emptyReplace ("0{1," ++ toString y ++ "}$")
    [x] ->
      toFixed x value
    _ ->
      toString value


processPrecision : Language -> String -> Float -> String -> (String, String)
processPrecision lang format value precision =
  let
    fst =
      if String.contains "[" precision then
        emptyReplace "]" precision
        |> String.split "["
        |> List.map String.length
        |> List.take 2
        |> (flip toFixedWithOptional) value
      else
        toFixed (String.length precision) value
    snd =
      case String.split "." fst of
        [x, y] ->
          if String.length y > 0 then
            lang.delimiters.decimal ++ y
          else
            ""
        _ ->
          ""
    w =
      String.split "." fst
      |> List.head
      |> Maybe.withDefault ""
  in
    if precision == "" then
      (w, "")
    else
      (w, snd)


addThousandsDelimiter : Language -> String -> String
addThousandsDelimiter lang word =
  Regex.replace
    All
    (regex "(\\d)(?=(\\d{3})+(?!\\d))")
    (\{match} -> match ++ lang.delimiters.thousands)
    word


formatNumber : NumberTypeFormatter
formatNumber lang format value strValue =
  let
    (format', negP, signed) = checkParensAndSign format
    (format'', abbr, value') = checkAbbreviation lang format' value
    (format''', value'', bytes) = checkByte format'' value'
    -- this is a stupid mess...
    (format'''', ord) = checkOrdinal lang format''' value''
    (finalFormat, optDec) = checkOptionalDec format''''
    strValue' = toString value''
    w =
      String.split "." strValue'
      |> List.head
      |> Maybe.withDefault ""
    precision =
      String.split "." finalFormat
      |> List.drop 1
      |> List.head
      |> Maybe.withDefault ""
    (w', d) = processPrecision lang format value'' precision
    d' =
      let
        result =
          String.slice 1 (String.length d) d
          |> String.toInt
          |> Result.toMaybe
          |> Maybe.withDefault 1
      in
        if optDec && result == 0 then
          ""
        else
          d
    w'' =
      if String.contains "," finalFormat then
        addThousandsDelimiter lang w'
      else
        w'
    (w''', neg) =
      if String.contains "-" w'' then
        (String.slice 1 (String.length w'') w'', True)
      else
        (w'', False)
    finalWord =
      if indexOf "." finalFormat == 0 then
        ""
      else
        w'''
    parens =
      if negP && neg then
        ("(", ")")
      else
        ("", "")
    minus =
      if (not negP) && neg then
        "-"
      else
        ""
    plus =
      if (not neg) && signed then
        "+"
      else
        ""
  in
    [ fst parens
    , minus
    , plus
    , finalWord
    , d'
    , ord
    , abbr
    , bytes
    , snd parens
    ] |> String.join ""


{-| Format a number with a given language.

    import Language.Japanese as Japanese

    myFormat = formatWithLanguage Japanese.lang "0.0a"

    -- map myFormat [10123.12, 235798239.3242] == ["10.1千","235.8百万"]
-}
formatWithLanguage : Language -> String -> Float -> String
formatWithLanguage lang format value =
  let
    numberType =
      if String.contains "$" format then
        formatCurrency
      else if String.contains "%" format then
        formatPercentage
      else if String.contains ":" format then
        formatTime
      else
        formatNumber
  in
    numberType lang format value (toString value)


{-| Same as formatWithLanguage, but English is set as default language.

    format "$0,0.00" 1000.234 == "$1,000.23"
-}
format : String -> Float -> String
format =
  formatWithLanguage English.lang

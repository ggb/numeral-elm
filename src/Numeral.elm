module Numeral exposing(format, formatWithLanguage)

{-| Elm module for (advanced) number formatting. It is a direct port of [Numeral.js](http://numeraljs.com/) and it is possible to use the same format strings. Manipulation and unformatting of numbers is not yet supported.

If you create a new language-file, please let me know or send a pull request.

# Formatting

@docs format, formatWithLanguage

-}

import Array exposing (Array)
import Language exposing (..)
import Languages.English as English
import Regex exposing (..)
import String


type alias NumberTypeFormatter =
  Language -> String -> Float -> String -> String


type alias Numeral = 
  { language: Language
  , format_value: String
  , value: Float
  , word: String
  , strValue: String
  , signed: Bool
  , neg: Bool
  , negP: Bool
  , customSuffix: String
  , abbreviation: String
  , bytes: String
  , ordinal: String
  , decimal: String
  , optionalDecimal: Bool
  , parens: (String, String)
  , precision: String
  , leadingZeros: Int
  , minus: String
  , plus: String
  }


empty : Language -> String -> Float -> Numeral
empty lang format_val value = 
  { language=lang
  , format_value=format_val
  , value=value
  , word=""
  , strValue=String.fromFloat value
  , signed=False
  , neg=False
  , negP=False
  , customSuffix=""
  , abbreviation=""
  , bytes=""
  , ordinal=""
  , decimal=""
  , optionalDecimal=False
  , parens=("","")
  , precision=""
  , leadingZeros=0
  , minus=""
  , plus=""
  }


regex : String -> Regex
regex str =
  Maybe.withDefault Regex.never <|
    Regex.fromString str 
    
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
  Regex.replace (regex str) (\_ -> "")


formatWithoutCurrency : String -> (String, String)
formatWithoutCurrency lformat =
  if String.contains " $" lformat then
    (" ", emptyReplace " \\$" lformat)
  else if String.contains "$ " lformat then
    (" ", emptyReplace "\\$ " lformat)
  else
    ("", emptyReplace "\\$" lformat)


formatCurrency : NumberTypeFormatter
formatCurrency lang lformat value strValue =
  let
    symbolIndex = indexOf "$" lformat
    openParenIndex = indexOf "(" lformat
    minusSignIndex = indexOf "-" lformat
    (space, format1) = formatWithoutCurrency lformat
    formatted = formatNumber (empty lang format1 value)
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
formatWithoutPercent lformat =
  if String.contains " %" lformat then
    (" ", emptyReplace " %" lformat)
  else
    ("", emptyReplace "%" lformat)


formatPercentage : Language -> String -> Float -> String -> String
formatPercentage lang lformat value strValue =
  let
    value1 = value * 100
    (space, format1) = formatWithoutPercent lformat
    formatted = formatNumber (empty lang format1 value1)
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
formatTime lang lformat value strValue =
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
    [ (hours |> String.fromFloat)
    , (minutes |> String.fromFloat |> hasOneDigit)
    , (seconds |> String.fromInt |> hasOneDigit)
    ] |> String.join ":"


checkParensAndSign : Numeral -> Numeral
checkParensAndSign numeral =
  if String.contains "(" numeral.format_value then
    {numeral | format_value = String.slice 1 -1 numeral.format_value, negP=True, signed=False}
  else if String.contains "+" numeral.format_value then
    {numeral | format_value = emptyReplace "\\+" numeral.format_value, negP=False, signed=True}
  else
    numeral


checkAbbreviation : Numeral -> Numeral
checkAbbreviation numeral =
  let
    {language, format_value, value} = numeral
    abbrK = String.contains "aK" format_value
    abbrM = String.contains "aM" format_value
    abbrB = String.contains "aB" format_value
    abbrT = String.contains "aT" format_value
    abbrForce = abbrK || abbrM || abbrB || abbrT |> not
    absValue = abs value
    (abbr, format1) =
      if String.contains " a" format_value then
        (" ", emptyReplace " a" format_value)
      else
        ("", emptyReplace "a" format_value)
  in
    if not <| String.contains "a" format_value then
      numeral
    else if absValue >= 10^12 && abbrForce || abbrT then
      {numeral | format_value=format1, abbreviation=abbr ++ language.abbreviations.trillion, value=value / 10^12}
    else if absValue < 10^12 && absValue >= 10^9 && abbrForce || abbrB then
      {numeral | format_value=format1, abbreviation=abbr ++ language.abbreviations.billion, value=value / 10^9}
    else if absValue < 10^9 && absValue >= 10^6 && abbrForce || abbrM then
      {numeral | format_value=format1, abbreviation=abbr ++ language.abbreviations.million, value=value / 10^6}
    else if absValue < 10^6 && absValue >= 10^3 && abbrForce || abbrK then
      {numeral | format_value=format1, abbreviation=abbr ++ language.abbreviations.thousand, value=value / 10^3}
    else
      {numeral | format_value=format1, abbreviation=abbr}


checkByte : Numeral -> Numeral
checkByte numeral =
  let
    {format_value, value} = numeral
    (bytes, format1) =
      if String.contains " b" format_value then
        (" ", emptyReplace " b" format_value)
      else
        ("", emptyReplace "b" format_value)

    suffixIndex1 power =
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
          suffixIndex1 (power + 1)
        else
            (-1, value)
    (suffixIndex, value1) = suffixIndex1 0
    suffix =
      Array.get (round suffixIndex) suffixes
      |> Maybe.withDefault ""
  in
    if String.contains "b" format_value then
      {numeral | format_value=format1, value=value1, bytes=bytes ++ suffix}
    else
      numeral


checkOrdinal : Numeral -> Numeral
checkOrdinal numeral =
  let
    {language, format_value, value} = numeral
    (ord, format1) =
      if String.contains " o" format_value then
        (" ", emptyReplace " o" format_value)
      else
        ("", emptyReplace "o" format_value)
  in
    if String.contains "o" format_value then
      {numeral | format_value=format1, ordinal=ord ++ (value |> numeral.language.ordinal)}
    else
      numeral


checkOptionalDec : Numeral -> Numeral
checkOptionalDec numeral =
  if String.contains "[.]" numeral.format_value then
    {numeral 
      | format_value=Regex.replace (regex "[.]") (\_ -> ".") numeral.format_value -- potential error source
      , optionalDecimal=True}
  else
    numeral


checkForCustomSuffix : Numeral -> Numeral
checkForCustomSuffix numeral =
  let
    hasSuffix = 
      Regex.find (regex "\\[\\D+\\]$") numeral.format_value
      |> List.head
  in
    case hasSuffix of
      Nothing ->
        numeral
      Just {match} ->
        { numeral 
          | format_value=(Regex.replace (regex match) (\_ -> "") numeral.format_value)
          , customSuffix=(Regex.replace (regex "\\[|\\]") (\_ -> "") match) }


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
    |> String.fromFloat
    |> String.split "."
    |> pad
    |> String.join "."


toFixedWithOptional : List Int -> Float -> String
toFixedWithOptional prs value =
  case prs of
    [x, y] ->
      toFixed (x + y) value
      |> emptyReplace ("0{1," ++ (String.fromInt y) ++ "}$")
    [x] ->
      toFixed x value
    _ ->
      String.fromFloat value

flip : (b -> a -> c) -> a -> b -> c
flip f x y = f y x

processPrecision : Numeral -> Numeral
processPrecision numeral =
  let
    {language, format_value, value, precision} = numeral
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
            language.delimiters.decimal ++ y
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
      {numeral | word=w, decimal=""}
    else
      {numeral | word=w, decimal=snd}


addThousandsDelimiter : Language -> String -> String
addThousandsDelimiter lang word =
  Regex.replace
    (regex "(\\d)(?=(\\d{3})+(?!\\d))")
    (\{match} -> match ++ lang.delimiters.thousands)
    word


updateStringValue : Numeral -> Numeral
updateStringValue numeral =
  {numeral | strValue=String.fromFloat numeral.value}


processWord : Numeral -> Numeral
processWord numeral =
  let
    w =
      String.split "." numeral.strValue
      |> List.head
      |> Maybe.withDefault ""
  in
    {numeral | word=w}


getPrecision : Numeral -> Numeral
getPrecision numeral =
  let
    splitted =
      String.split "." numeral.format_value
    leadingZeros = 
      splitted
      |> List.head
      |> Maybe.withDefault ""
      |> String.length
    leadingZeros_ =
      if String.contains "," numeral.format_value then
        0
      else
        leadingZeros 
    precision =
      splitted
      |> List.drop 1
      |> List.head
      |> Maybe.withDefault ""
  in
    {numeral | precision=precision, leadingZeros=leadingZeros_}


processLeadingZeros : Numeral -> Numeral
processLeadingZeros numeral =
  let
    w = 
      numeral.word
      |> String.split "."
      |> List.head
      |> Maybe.withDefault ""
      |> String.length
    n =
      numeral.leadingZeros - w
    prefix =
      String.repeat n "0"
  in
    {numeral | word=prefix++numeral.word}


processDecimal : Numeral -> Numeral
processDecimal numeral =
  let
    d = numeral.decimal
    result =
      String.slice 1 (String.length d) d
      |> String.toInt
      |> Maybe.withDefault 1
  in
    if numeral.optionalDecimal && result == 0 then
      {numeral | decimal=""}
    else
      {numeral | decimal=d}


checkThousandsDelimiter : Numeral -> Numeral
checkThousandsDelimiter numeral =
  if String.contains "," numeral.format_value then
    {numeral | word=addThousandsDelimiter numeral.language numeral.word}
  else
    numeral


checkIfNegative : Numeral -> Numeral
checkIfNegative numeral =
  if String.contains "-" numeral.word then
    {numeral 
      | word=String.slice 1 (String.length numeral.word) numeral.word
      , neg=True}
  else
    numeral


createFinalWord : Numeral -> Numeral
createFinalWord numeral =
  if indexOf "." numeral.format_value == 0 then
    {numeral | word=""}
  else
    numeral


createParens : Numeral -> Numeral
createParens numeral =
  if numeral.negP && numeral.neg then
    {numeral | parens=("(", ")")}
  else
    numeral


hasMinus : Numeral -> Numeral
hasMinus numeral =
  if (not numeral.negP) && numeral.neg then
    {numeral | minus="-"}
  else
    numeral


hasPlus : Numeral -> Numeral
hasPlus numeral =
  if (not numeral.neg) && numeral.signed then
    {numeral | plus="+"}
  else
    numeral


createFinalString : Numeral -> String
createFinalString {parens,minus,plus,word,decimal,ordinal,abbreviation,bytes,customSuffix} =
  [ Tuple.first parens
  , minus
  , plus
  , word
  , decimal
  , ordinal
  , abbreviation
  , bytes
  , customSuffix
  , Tuple.second parens
  ] |> String.join ""


formatNumber : Numeral -> String
formatNumber numeral =
  numeral
  |> checkParensAndSign
  |> checkForCustomSuffix
  |> checkAbbreviation
  |> checkByte
  |> checkOrdinal
  |> checkOptionalDec
  |> updateStringValue
  |> processWord
  |> getPrecision
  |> processPrecision
  |> processLeadingZeros
  |> processDecimal
  |> checkThousandsDelimiter
  |> checkIfNegative
  |> createFinalWord
  |> createParens
  |> hasMinus
  |> hasPlus
  |> createFinalString


{-| Format a number with a given language.

    import Language.Japanese as Japanese

    myFormat = formatWithLanguage Japanese.lang "0.0a"

    -- map myFormat [10123.12, 235798239.3242] == ["10.1千","235.8百万"]
-}
formatWithLanguage : Language -> String -> Float -> String
formatWithLanguage lang lformat value =
  if String.contains "$" lformat then
    formatCurrency lang lformat value (String.fromFloat value)
  else if String.contains "%" lformat then
    formatPercentage lang lformat value (String.fromFloat value)
  else if String.contains ":" lformat then
    formatTime lang lformat value (String.fromFloat value)
  else
    formatNumber (empty lang lformat value)


{-| Same as formatWithLanguage, but English is set as default language.

    format "$0,0.00" 1000.234 == "$1,000.23"
-}
format : String -> Float -> String
format =
  formatWithLanguage English.lang

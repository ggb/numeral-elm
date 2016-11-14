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


type alias Numeral = 
  { language: Language
  , format: String
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
  , minus: String
  , plus: String
  }


empty : Language -> String -> Float -> Numeral
empty lang format value = 
  { language=lang
  , format=format
  , value=value
  , word=""
  , strValue=toString value
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
  , minus=""
  , plus=""
  }


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
    (space, format1) = formatWithoutCurrency format
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
formatWithoutPercent format =
  if String.contains " %" format then
    (" ", emptyReplace " %" format)
  else
    ("", emptyReplace "%" format)


formatPercentage : Language -> String -> Float -> String -> String
formatPercentage lang format value strValue =
  let
    value1 = value * 100
    (space, format1) = formatWithoutPercent format
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


checkParensAndSign : Numeral -> Numeral
checkParensAndSign numeral =
  if String.contains "(" numeral.format then
    {numeral | format = String.slice 1 -1 numeral.format, negP=True, signed=False}
  else if String.contains "+" numeral.format then
    {numeral | format = emptyReplace "\\+" numeral.format, negP=False, signed=True}
  else
    numeral


checkAbbreviation : Numeral -> Numeral
checkAbbreviation numeral =
  let
    {language, format, value} = numeral
    abbrK = String.contains "aK" format
    abbrM = String.contains "aM" format
    abbrB = String.contains "aB" format
    abbrT = String.contains "aT" format
    abbrForce = abbrK || abbrM || abbrB || abbrT |> not
    absValue = abs value
    (abbr, format1) =
      if String.contains " a" format then
        (" ", emptyReplace " a" format)
      else
        ("", emptyReplace "a" format)
  in
    if not <| String.contains "a" format then
      numeral
    else if absValue >= 10^12 && abbrForce || abbrT then
      {numeral | format=format1, abbreviation=abbr ++ language.abbreviations.trillion, value=value / 10^12}
    else if absValue < 10^12 && absValue >= 10^9 && abbrForce || abbrB then
      {numeral | format=format1, abbreviation=abbr ++ language.abbreviations.billion, value=value / 10^9}
    else if absValue < 10^9 && absValue >= 10^6 && abbrForce || abbrM then
      {numeral | format=format1, abbreviation=abbr ++ language.abbreviations.million, value=value / 10^6}
    else if absValue < 10^6 && absValue >= 10^3 && abbrForce || abbrK then
      {numeral | format=format1, abbreviation=abbr ++ language.abbreviations.thousand, value=value / 10^3}
    else
      {numeral | format=format1, abbreviation=abbr}


checkByte : Numeral -> Numeral
checkByte numeral =
  let
    {format, value} = numeral
    (bytes, format1) =
      if String.contains " b" format then
        (" ", emptyReplace " b" format)
      else
        ("", emptyReplace "b" format)

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
      Array.get suffixIndex suffixes
      |> Maybe.withDefault ""
  in
    if String.contains "b" format then
      {numeral | format=format1, value=value1, bytes=bytes ++ suffix}
    else
      numeral


checkOrdinal : Numeral -> Numeral
checkOrdinal numeral =
  let
    {language, format, value} = numeral
    (ord, format1) =
      if String.contains " o" format then
        (" ", emptyReplace " o" format)
      else
        ("", emptyReplace "o" format)
  in
    if String.contains "o" format then
      {numeral | format=format1, ordinal=ord ++ (value |> numeral.language.ordinal)}
    else
      numeral


checkOptionalDec : Numeral -> Numeral
checkOptionalDec numeral =
  if String.contains "[.]" numeral.format then
    {numeral 
      | format=Regex.replace All ("[.]" |> Regex.escape |> regex) (\_ -> ".") numeral.format
      , optionalDecimal=True}
  else
    numeral


checkForCustomSuffix : Numeral -> Numeral
checkForCustomSuffix numeral =
  let
    hasSuffix = 
      Regex.find All (regex "\\[\\D+\\]$") numeral.format
      |> List.head
  in
    case hasSuffix of
      Nothing ->
        numeral
      Just {match} ->
        { numeral 
          | format=(Regex.replace All (match |> Regex.escape |> regex) (\_ -> "") numeral.format)
          , customSuffix=(Regex.replace All (regex "\\[|\\]") (\_ -> "") match) }


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


processPrecision : Numeral -> Numeral
processPrecision numeral =
  let
    {language, format, value, precision} = numeral
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
    All
    (regex "(\\d)(?=(\\d{3})+(?!\\d))")
    (\{match} -> match ++ lang.delimiters.thousands)
    word


updateStringValue : Numeral -> Numeral
updateStringValue numeral =
  {numeral | strValue=toString numeral.value}


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
    precision =
      String.split "." numeral.format
      |> List.drop 1
      |> List.head
      |> Maybe.withDefault ""
  in
    {numeral | precision=precision}


processDecimal : Numeral -> Numeral
processDecimal numeral =
  let
    d = numeral.decimal
    result =
      String.slice 1 (String.length d) d
      |> String.toInt
      |> Result.toMaybe
      |> Maybe.withDefault 1
  in
    if numeral.optionalDecimal && result == 0 then
      {numeral | decimal=""}
    else
      {numeral | decimal=d}


checkThousandsDelimiter : Numeral -> Numeral
checkThousandsDelimiter numeral =
  if String.contains "," numeral.format then
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
  if indexOf "." numeral.format == 0 then
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
formatWithLanguage lang format value =
  if String.contains "$" format then
    formatCurrency lang format value (toString value)
  else if String.contains "%" format then
    formatPercentage lang format value (toString value)
  else if String.contains ":" format then
    formatTime lang format value (toString value)
  else
    formatNumber (empty lang format value)


{-| Same as formatWithLanguage, but English is set as default language.

    format "$0,0.00" 1000.234 == "$1,000.23"
-}
format : String -> Float -> String
format =
  formatWithLanguage English.lang

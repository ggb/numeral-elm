module Language exposing(Language, Ordinal)

{-| Type definition for language configurations.

To implement a new language configuration, copy an existing one and modify it.

@docs Ordinal, Language

-}

type alias Delimiters =
  { thousands:String
  , decimal:String
  }

type alias Abbreviations =
  { thousand:String
  , million:String
  , billion:String
  , trillion:String
  }

{-| Type of a function that takes a float as input and returns an ordinal abbreviation string.
-}
type alias Ordinal = Float -> String

type alias Currency =
  { symbol:String
  }

{-| Language defines the delimiters, abbreviations, ordinal and currency symbol.
-}
type alias Language =
  { delimiters:Delimiters
  , abbreviations:Abbreviations
  , ordinal:Ordinal
  , currency:Currency
  }

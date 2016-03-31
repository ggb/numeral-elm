module Language where


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

type alias Ordinal = Float -> String

type alias Currency =
  { symbol:String
  }

type alias Language =
  { delimiters:Delimiters
  , abbreviations:Abbreviations
  , ordinal:Ordinal
  , currency:Currency
  }
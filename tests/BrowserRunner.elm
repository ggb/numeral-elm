module BrowserRunner exposing (..)

import String
import Html exposing(..)
import Tests exposing (tests)

main : Html String
main =
  let
    lines = tests |> stringRunner |> String.lines
  in
    div [ ] (List.map (\s -> line s) lines)

line : String -> Html String
line s =
  div [ ] [
    text s,
    br [ ] [ ]
  ]

module ConsoleRunner exposing (..)

import String
import Task
import Console
import ElmTest exposing (..)
import Tests exposing (tests)


port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)
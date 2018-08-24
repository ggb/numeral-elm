module Tests exposing (..)

import Test exposing (Test, test, describe)
import Expect
import Numeral
import Tuple exposing (pair)


type alias CaseCollection = (String, List Case)
type alias Case = (Float, String, String)


numbers : CaseCollection
numbers =
  pair "Numbers"
  [
    (10000,"0,0.0000","10,000.0000"),
    (10000.23,"0,0","10,000"),
    (-10000,"0,0.0","-10,000.0"),
    (10000.1234,"0.000","10000.123"),
    (10000,"0[.]00","10000"),
    (10000.1,"0[.]00","10000.10"),
    (10000.123,"0[.]00","10000.12"),
    (10000.456,"0[.]00","10000.46"),
    (10000.001,"0[.]00","10000"),
    (10000.45,"0[.]00[0]","10000.45"),
    (10000.456,"0[.]00[0]","10000.456"),
    (-10000,"(0,0.0000)","(10,000.0000)"),
    (-12300,"+0,0.0000","-12,300.0000"),
    (1230,"+0,0","+1,230"),
    (100.78, "0", "101"),
    (100.28, "0", "100"),
    (1.932,"0.0","1.9"),
    (1.9687,"0","2"),
    (1.9687,"0.0","2.0"),
    (-0.23,".00","-.23"),
    (-0.23,"(.00)","(.23)"),
    (0.23,"0.00000","0.23000"),
    (0.67,"0.0[0000]","0.67"),
    (2000000000,"0.0a","2.0b"),
    (1230974,"0.0a","1.2m"),
    (1460,"0a","1k"),
    (-104000,"0 a","-104 k"),
    (1,"0o","1st"),
    (52,"0 o","52 nd"),
    (23,"0o","23rd"),
    (100,"0o","100th"),

    -- specified abbreviations
    (-5444333222111.0, "0,0 aK", "-5,444,333,222 k"),
    (-5444333222111.0, "0,0 aM", "-5,444,333 m"),
    (-5444333222111.0, "0,0 aB", "-5,444 b"),
    (-5444333222111.0, "0,0 aT", "-5 t")
  ]


currency : CaseCollection
currency =
  pair "Currency"
  [
    (1000.234,"$0,0.00","$1,000.23"),
    (1001,"$ 0,0.[00]","$ 1,001"),
    (1000.234,"0,0.00 $","1,000.23 $"),
    (-1000.234,"($0,0)","($1,000)"),
    (-1000.234,"(0,0$)","(1,000$)"),
    (-1000.234,"$0.00","-$1000.23"),
    (1230974,"($0.00 a)","$1.23 m"),

    -- test symbol position before negative sign / open parens
    (-1000.234,"$ (0,0)","$ (1,000)"),
    (-1000.234,"$(0,0)","$(1,000)"),
    (-1000.234,"$ (0,0.00)","$ (1,000.23)"),
    (-1000.234,"$(0,0.00)","$(1,000.23)"),
    (-1000.238,"$(0,0.00)","$(1,000.24)"),
    (-1000.234,"$-0,0","$-1,000"),
    (-1000.234,"$ -0,0","$ -1,000"),

    (1000.234,"$ (0,0)","$ 1,000"),
    (1000.234,"$(0,0)","$1,000"),
    (1000.234,"$ (0,0.00)","$ 1,000.23"),
    (1000.234,"$(0,0.00)","$1,000.23"),
    (1000.238,"$(0,0.00)","$1,000.24"),
    (1000.234,"$-0,0)","$1,000"),
    (1000.234,"$ -0,0","$ 1,000")
  ]


bytes : CaseCollection
bytes =
  pair "Bytes"
  [
    (100,"0b","100B"),
    (1024*2,"0 b","2 KB"),
    (1024*1024*5,"0b","5MB"),
    (1024*1024*1024*7.343,"0.[0] b","7.3 GB"),
    (1024*1024*1024*1024*3.1536544,"0.000b","3.154TB"),
    (1024*1024*1024*1024*1024*2.953454534534,"0b","3PB")
  ]


percentages : CaseCollection
percentages =
  pair "Percentages"
  [
    (1,"0%","100%"),
    (0.974878234,"0.000%","97.488%"),
    (-0.43,"0 %","-43 %"),
    (0.43,"(0.00[0]%)","43.00%")
  ]


times : CaseCollection
times =
  pair "Times"
  [
    (25,"00:00:00","0:00:25"),
    (238,"00:00:00","0:03:58"),
    (63846,"00:00:00","17:44:06")
  ]


customUnitSuffix : CaseCollection
customUnitSuffix = 
  pair "Custom Unit Suffix"
  [
    (12345,"0,0[ pcs.]","12,345 pcs."),
    (12345,"0,0[pcs.]","12,345pcs."),
    (300000, "0,0 [ ponies]", "300,000 ponies"),
    (22, "0.00[ blobs]", "22.00 blobs"),
    (4, "0[ ants]", "4 ants")
  ]


rounding : CaseCollection
rounding =
  pair "Rounding"
  [
    (2.385, "0,0.00", "2.39"),
    (28.885, "0,0.00", "28.89")
  ]


leadingZeros : CaseCollection
leadingZeros =
  pair "Leading Zeros"
  [
    (12, "000", "012"),
    (1.932,"000.0","001.9"),
    (1.9687,"0000","0002"),
    (22, "000.00[ blobs]", "022.00 blobs"),
    (1230974,"($00.00 a)","$01.23 m"),
    (1,"000%","100%"),
    (0.974878234,"000.000%","097.488%"),
    (1024*2,"000 b","002 KB"),
    (1024*1024*5,"000b","005MB")
  ]


createTest : Case -> Test
createTest (source, format, expected) = 
  test (expected ++ " == " ++ "Numeral.format " ++ format ++ " " ++ (String.fromFloat source)) <|
    \() -> 
        Expect.equal expected (Numeral.format format source)


createSuite : CaseCollection -> Test
createSuite (suiteName, cases) = 
  describe suiteName (List.map createTest cases)


all : Test
all =
  describe "Numeral Tests"
    (List.map createSuite 
      [ numbers
      , currency
      , rounding
      , bytes
      , percentages
      , times
      , customUnitSuffix
      , leadingZeros])

module DayOneTests exposing (suite)
import List exposing(foldl, foldr)

import Expect exposing (Expectation)
import Test exposing (..)
import DayOne exposing (fuel, moduleFuel, puzzleInput, solvePartOne, solvePartTwo)
import Tuple exposing (first, second)

fuelSubjects : List (Int, Int)
fuelSubjects =
  [ (12, 2)
  , (14, 2)
  , (1969, 654)
  , (100756, 33583)
  ]

generateFuelTest : (Int, Int) -> Test
generateFuelTest tuple =
  let
    (input, output) = tuple
    message = String.join "" <|
      [ "given "
      , String.fromInt input
      , " calculates "
      , String.fromInt output
      ]
  in
    test message <|
      \() -> Expect.equal (fuel input) output

moduleFuelSubjects : List (Int, Int)
moduleFuelSubjects =
  [ (12, 2)
  , (1969, 966)
  , (100756, 50346)
  ]

generateModuleFuelTest : (Int, Int) -> Test
generateModuleFuelTest tuple =
  let
    (input, output) = tuple
    message = String.join "" <|
      [ "given "
      , String.fromInt input
      , " calculates "
      , String.fromInt output
      ]
  in
    test message <|
      \() -> Expect.equal (moduleFuel 0 input) output

suite : Test
suite =
  describe "The day one module"
    [ describe "DayOne.fuel" <|
        List.map generateFuelTest fuelSubjects
    , describe "DayOne.moduleFuel" <|
        List.map generateModuleFuelTest moduleFuelSubjects
    , describe "DayOne.solvePartOne" <|
        [test "returns the correct solution" <|
          \() -> Expect.equal (solvePartOne puzzleInput) 3331523
        ]
    , describe "DayOne.solvePartTwo" <|
        [test "returns the correct solution" <|
          \() -> Expect.equal (solvePartTwo puzzleInput) 4994396
        ]
    ]


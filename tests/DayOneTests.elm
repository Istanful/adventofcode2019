module DayOneTests exposing (suite)
import List exposing(foldl, foldr)

import Expect exposing (Expectation)
import Test exposing (..)
import DayOne exposing (calculateFuel, puzzleInput, solve)
import Tuple exposing (first, second)

subjects : List (Int, Int)
subjects =
  [ (12, 2)
  , (14, 2)
  , (1969, 654)
  , (100756, 33583)
  ]

generateTest : (Int, Int) -> Test
generateTest tuple =
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
      \() -> Expect.equal (calculateFuel input) output

suite : Test
suite =
  describe "The day one module"
    [ describe "DayOne.calculateFuel" <|
        List.map generateTest subjects
    , describe "DayOne.solve" <|
        [test "returns the correct solution" <|
          \() -> Expect.equal (solve puzzleInput) 3331523
        ]
    ]


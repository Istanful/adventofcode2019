module DayTwoTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import DayTwo exposing (..)
import Maybe exposing (withDefault)


suite : Test
suite =
  describe "The day two module"
    [ describe "DayTwo.computer" <|
        [ test "adds values" <|
            \() -> Expect.equal (computer [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99]
        , test "multiplies values" <|
            \() -> Expect.equal (computer [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99]
        , test "puzzle example 2" <|
            \() -> Expect.equal (computer [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]) [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        , test "puzzle example 3" <|
            \() -> Expect.equal (computer [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801]
        , test "puzzle example 4" <|
            \() -> Expect.equal (computer [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99]
        ]
    , describe "DayTwo.solvePartOne" <|
        [ test "solves part one" <|
            \() -> Expect.equal (withDefault 0 (solvePartOne puzzleInput)) 3716293
        ]
    , describe "DayTwo.solvePartTwo" <|
        [ skip <| test "solves part two" <|
            \() -> Expect.equal (solvePartTwo puzzleInput) 6429
        ]
    ]

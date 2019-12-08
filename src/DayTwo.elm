module DayTwo exposing(puzzleInput, solvePartOne, solvePartTwo, computer)

import List exposing(foldl, tail, take, drop, indexedMap, member, filter, map, any, concatMap)
import Tuple exposing(second, first)
import Basics exposing(remainderBy, modBy)
import Maybe exposing (withDefault)

solvePartOne input =
  List.head <|
    computer (setInputPairs input 12 2)

solvePartTwo input =
  let
    (noun, verb) = test input 0 0
  in
    100 * noun + verb

test input noun verb =
  let
    expected = 19690720
    result = computer (setInputPairs input noun verb)
    actual = withDefault 0 (List.head result)
    (nextNoun, nextVerb) = (nextInputPairs noun verb)
  in
    if actual == expected then
      (noun, verb)
    else
      test input nextNoun nextVerb

nextInputPairs noun verb =
  if (verb == 99) then
    (noun + 1, 0)
  else
    (noun, verb + 1)

setInputPairs input noun verb =
  input
    |> indexedMap (\i num ->
      if (i == 1) then
        noun
      else if (i == 2) then
        verb
      else
        num
    )

computer : List Int -> List Int
computer instructions =
  run 0 instructions

run index instructions=
  let
    operations = instructions
      |> indexedMap (\i num -> (i, num))
      |> filter (\ins -> (first ins) == index)
      |> map (\ins -> second ins)
      |> prepare
  in
    if any (\operation -> operation.num == 99) operations then
      instructions
    else
      operations
        |> map (\operation -> operate
          { num = operation.num
          , i = index
          , list = instructions
          }
        )
        |> concatMap (\operation -> operation.list)
        |> run (index + 4)

operate operation =
  if operation.num == 1 then
    perform add operation
  else if operation.num == 2 then
    perform multiply operation
  else
    operation

perform method operation =
  let
    list = operation.list
    data = method (collectData operation)
    indexes = cellIndexes operation
  in
    { num = operation.num
    , i = operation.i
    , list = setData operation.list indexes data
    }

collectData operation =
  let
    list = operation.list
    positions = list
      |> indexedMap (\i num -> (i, num))
      |> filter (\ins -> member (first ins) [operation.i + 1, operation.i + 2])
      |> List.map (\ins -> second ins)
  in
    positions
      |> List.concatMap (\i ->
        list
          |> List.indexedMap (\b num -> (b, num))
          |> filter (\ins -> (first ins) == i)
      )
      |> List.map (\ins -> second ins)

prepare instructions =
  indexedMap (\i num ->
    { num = num
    , i = i
    , list = instructions
    }) instructions


cellIndexes operation =
  let
    list = operation.list
  in
    list
      |> indexedMap (\i num -> (i, num))
      |> filter (\ins -> (first ins) == operation.i + 3)
      |> List.map (\ins -> (second ins))

print d =
  Debug.log (Debug.toString d)

setData : List Int -> List Int -> Int -> List Int
setData list indexes data =
  list
    |> indexedMap (\i num ->
      if member i indexes then
        data
      else
        num
    )

add data =
  List.sum data

multiply data =
  List.product data

puzzleInput =
  [ 1
  , 0
  , 0
  , 3
  , 1
  , 1
  , 2
  , 3
  , 1
  , 3
  , 4
  , 3
  , 1
  , 5
  , 0
  , 3
  , 2
  , 10
  , 1
  , 19
  , 2
  , 19
  , 6
  , 23
  , 2
  , 13
  , 23
  , 27
  , 1
  , 9
  , 27
  , 31
  , 2
  , 31
  , 9
  , 35
  , 1
  , 6
  , 35
  , 39
  , 2
  , 10
  , 39
  , 43
  , 1
  , 5
  , 43
  , 47
  , 1
  , 5
  , 47
  , 51
  , 2
  , 51
  , 6
  , 55
  , 2
  , 10
  , 55
  , 59
  , 1
  , 59
  , 9
  , 63
  , 2
  , 13
  , 63
  , 67
  , 1
  , 10
  , 67
  , 71
  , 1
  , 71
  , 5
  , 75
  , 1
  , 75
  , 6
  , 79
  , 1
  , 10
  , 79
  , 83
  , 1
  , 5
  , 83
  , 87
  , 1
  , 5
  , 87
  , 91
  , 2
  , 91
  , 6
  , 95
  , 2
  , 6
  , 95
  , 99
  , 2
  , 10
  , 99
  , 103
  , 1
  , 103
  , 5
  , 107
  , 1
  , 2
  , 107
  , 111
  , 1
  , 6
  , 111
  , 0
  , 99
  , 2
  , 14
  , 0
  , 0
  ]

module DayOne exposing(fuel, moduleFuel, puzzleInput, solvePartOne, solvePartTwo)

import List exposing(foldl)
import Debug exposing(..)

solvePartOne inputs =
  foldl (\num acc -> acc + (fuel num)) 0 inputs

solvePartTwo inputs =
  foldl (\num acc -> acc + (moduleFuel 0 num)) 0 inputs

moduleFuel : Int -> Int -> Int
moduleFuel carry mass =
  let
    result = fuel mass
    next = fuel result
  in
    if next > 0 then
      moduleFuel (carry + result) result
    else
      carry + result

fuel : Int -> Int
fuel mass =
  floor (toFloat mass / 3) - 2

puzzleInput : List Int
puzzleInput =
  [ 95249
  , 126697
  , 77237
  , 80994
  , 91186
  , 53823
  , 115101
  , 130919
  , 88127
  , 141736
  , 53882
  , 67432
  , 94292
  , 73223
  , 139947
  , 66450
  , 55710
  , 128647
  , 73874
  , 57163
  , 139502
  , 140285
  , 119987
  , 125308
  , 77561
  , 74573
  , 85364
  , 92991
  , 102935
  , 71259
  , 99622
  , 118876
  , 124482
  , 148442
  , 77664
  , 90453
  , 111933
  , 110449
  , 74172
  , 148641
  , 58574
  , 135365
  , 84703
  , 81077
  , 65290
  , 136749
  , 127256
  , 94872
  , 143534
  , 81702
  , 59493
  , 72365
  , 69497
  , 149082
  , 79552
  , 78509
  , 73759
  , 147439
  , 97535
  , 118952
  , 114301
  , 104401
  , 95080
  , 100907
  , 132914
  , 136096
  , 52451
  , 70544
  , 120717
  , 107010
  , 76840
  , 51324
  , 135258
  , 73985
  , 118067
  , 86602
  , 95127
  , 51182
  , 84838
  , 60430
  , 86347
  , 140487
  , 147777
  , 85143
  , 114215
  , 100410
  , 126504
  , 69630
  , 123656
  , 108886
  , 144192
  , 123620
  , 147217
  , 146090
  , 101966
  , 80577
  , 62193
  , 143331
  , 79947
  , 93518
  ]

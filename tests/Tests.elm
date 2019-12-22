module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (range)
import Math exposing (..)
import Test exposing (..)
import Tuple exposing (first, second)


suite : Test
suite =
    describe "Math"
        [ describe "exp" <|
            [ test "exp 1 = e" <|
                \() ->
                    Expect.within (Expect.Absolute 0.000000001) Basics.e (exp 1)
            ]
        , describe
            "gammaLn"
          <|
            [ test "approximates the gamma ln function" <|
                \() ->
                    let
                        xs =
                            range 1 10 |> List.map (\x -> toFloat x / 10)

                        testVectors =
                            [ 2.252712651734255
                            , 1.5240638224308496
                            , 1.09579799481814
                            , 0.7966778177018394
                            , 0.572364942924743
                            , 0.3982338580692666
                            , 0.2608672465316859
                            , 0.15205967839984869
                            , 6.637623973474716e-2
                            , -4.440892098500626e-16
                            ]

                        gammaLns =
                            xs |> List.map gammaLn
                    in
                    Expect.equalLists testVectors gammaLns
            ]
        ]

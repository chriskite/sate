module Tests exposing (..)

import BayesBandit.Bernoulli exposing (winnerProbabilities)
import Dict
import Distribution.Bernoulli exposing (bernoulli)
import Expect exposing (Expectation)
import List exposing (range)
import Math exposing (..)
import Random exposing (Seed)
import State
import Test exposing (..)


suite : Test
suite =
    describe "sate" <|
        [ describe "BayesBandit" <|
            [ describe "Bernoulli" <|
                [ describe "winnerProbabilities" <|
                    [ test "returns winner probabilities" <|
                        \() ->
                            let
                                variants =
                                    Maybe.map3
                                        (\a b c -> Dict.fromList [ ( "A", a ), ( "B", b ), ( "C", c ) ])
                                        (bernoulli 1 10)
                                        (bernoulli 10 100)
                                        (bernoulli 11 99)

                                maybeWinners : Maybe (Dict.Dict String Float)
                                maybeWinners =
                                    Maybe.map
                                        (\vs -> Tuple.first (State.run (Random.initialSeed 42) (winnerProbabilities vs)))
                                        variants

                                result =
                                    case maybeWinners of
                                        Just winners ->
                                            winners

                                        Nothing ->
                                            Dict.empty

                                expected =
                                    Dict.fromList [ ( "A", 0.5783366666666667 ), ( "B", 0.16731 ), ( "C", 0.2543533333333333 ) ]
                            in
                            Expect.equal result expected
                    ]
                ]
            ]
        , describe "Math"
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
        ]

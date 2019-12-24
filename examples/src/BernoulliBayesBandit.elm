port module BernoulliBayesBandit exposing (elmToJS)

import BayesBandit.Bernoulli
import Dict
import Distribution.Bernoulli exposing (bernoulli)
import Platform
import VegaLite exposing (..)


vis : Spec
vis =
    let
        variants =
            Maybe.map3
                (\a b c -> Dict.fromList [ ( "A", a ), ( "B", b ), ( "C", c ) ])
                (bernoulli 1 10)
                (bernoulli 10 100)
                (bernoulli 12 99)
                |> Maybe.withDefault Dict.empty
    in
    BayesBandit.Bernoulli.pdfsVis variants


main : Program () Spec msg
main =
    Platform.worker
        { init = always ( vis, elmToJS vis )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        }


port elmToJS : Spec -> Cmd msg

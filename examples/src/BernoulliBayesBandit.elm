port module BernoulliBayesBandit exposing (elmToJS)

import BayesBandit.Bernoulli
import Dict exposing (Dict)
import Distribution.Bernoulli exposing (BernoulliDist, bernoulli)
import Platform
import Random
import State
import Tuple exposing (first)
import VegaLite exposing (..)


type alias JSMsg =
    { uncertainty : Spec, winners : Spec }


type alias VariantData =
    { variant : String, successes : Int, failures : Int }


uncertaintyVis : Dict String BernoulliDist -> Spec
uncertaintyVis variants =
    BayesBandit.Bernoulli.pdfsVis variants


winnersVis : Dict String BernoulliDist -> Spec
winnersVis variants =
    BayesBandit.Bernoulli.winnersVis variants
        |> State.run (Random.initialSeed 42)
        |> first


init : List VariantData -> ( ( Spec, Spec ), Cmd msg )
init flags =
    let
        variants =
            flags
                |> List.map (\f -> ( f.variant, bernoulli f.successes f.failures |> Maybe.withDefault Distribution.Bernoulli.zero ))
                |> Dict.fromList

        uncertainty =
            uncertaintyVis variants

        winners =
            winnersVis variants
    in
    ( ( uncertainty, winners ), elmToJS (JSMsg uncertainty winners) )


main : Program (List VariantData) ( Spec, Spec ) msg
main =
    Platform.worker
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        }


port elmToJS : JSMsg -> Cmd msg

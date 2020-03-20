port module BernoulliBayesBandit exposing (receiveVariants, sendVegaSpecs)

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


type alias Model =
    List VariantData


type Msg
    = ReceivedFromJS Model


uncertaintyVis : Dict String BernoulliDist -> Spec
uncertaintyVis variants =
    BayesBandit.Bernoulli.pdfsVis variants


winnersVis : Dict String BernoulliDist -> Spec
winnersVis variants =
    BayesBandit.Bernoulli.winnersVis variants
        |> State.run (Random.initialSeed 42)
        |> first


init : () -> ( Model, Cmd msg )
init _ =
    ( [], Cmd.none )


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ReceivedFromJS data ->
            let
                variants =
                    data
                        |> List.map (\f -> ( f.variant, bernoulli f.successes f.failures |> Maybe.withDefault Distribution.Bernoulli.zero ))
                        |> Dict.fromList

                uncertainty =
                    uncertaintyVis variants

                winners =
                    winnersVis variants
            in
            ( data, sendVegaSpecs (JSMsg uncertainty winners) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveVariants ReceivedFromJS


port receiveVariants : (List VariantData -> msg) -> Sub msg


port sendVegaSpecs : JSMsg -> Cmd msg

module Distribution.Beta exposing (Error, beta, pdf, sample)

import Distribution.Gamma exposing (gamma)
import Math exposing (..)
import Random exposing (Seed)
import Result
import State exposing (State, state)


type Error
    = InvalidA Float
    | InvalidB Float
    | GammaDeviateError Distribution.Gamma.Error
    | NotImplemented


type alias Beta =
    { a : Float, b : Float }


beta : Float -> Float -> Result Error Beta
beta a b =
    if a <= 0.0 then
        Err (InvalidA a)

    else if b <= 0.0 then
        Err (InvalidB b)

    else
        Ok (Beta a b)


pdf : Beta -> Float -> Float
pdf bta x =
    if x > 1 || x < 0 then
        0

    else if 1 == bta.a && 1 == bta.b then
        1

    else
        exp ((bta.a - 1) * ln x + (bta.b - 1) * ln (1 - x) - betaLn bta.a bta.b)


sample : Beta -> State Seed (Result Error Float)
sample bta =
    let
        a =
            bta.a

        b =
            bta.b

        sampleParamGamma : Float -> State Seed (Result Distribution.Gamma.Error Float)
        sampleParamGamma x =
            case gamma x 1.0 of
                Ok xg ->
                    Distribution.Gamma.sample xg

                Err e ->
                    state (Err e)
    in
    if a <= 0.5 && b <= 0.5 then
        state (Err NotImplemented)

    else if a <= 1.0 && b <= 1.0 then
        state (Err NotImplemented)

    else
        State.map2
            (\aGammaResult bGammaResult ->
                Result.map2
                    (\aGamma bGamma -> aGamma / (aGamma + bGamma))
                    aGammaResult
                    bGammaResult
                    |> Result.mapError GammaDeviateError
            )
            (sampleParamGamma a)
            (sampleParamGamma b)

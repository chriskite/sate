module Distribution.Beta exposing (BetaDist, alpha, beta, betaDist, pdf, sample, uniform)

{-| Provides functions for describing and sampling a Beta distribution.
-}

import Distribution.Gamma exposing (GammaDist, gammaDist)
import Math exposing (..)
import Random exposing (Seed)
import State exposing (State, state)


type BetaDist
    = BetaDist Float Float


betaDist : Float -> Float -> Maybe BetaDist
betaDist a b =
    if a <= 0.0 || b <= 0.0 then
        Nothing

    else
        Just (BetaDist a b)


uniform : BetaDist
uniform =
    BetaDist 1 1


alpha : BetaDist -> Float
alpha (BetaDist a _) =
    a


beta : BetaDist -> Float
beta (BetaDist _ b) =
    b


pdf : BetaDist -> Float -> Float
pdf dist x =
    let
        a =
            alpha dist

        b =
            beta dist
    in
    if x > 1 || x < 0 then
        0

    else if 1 == a && 1 == b then
        1

    else
        exp ((a - 1) * ln x + (b - 1) * ln (1 - x) - betaLn a b)


sample : BetaDist -> State Seed Float
sample dist =
    let
        a =
            alpha dist

        b =
            beta dist

        {- Samples the Gamma distribution with shape x
           Ignores the Nothing case of gammaDist, as we will check that x is a valid shape.
        -}
        sampleParamGamma : Float -> State Seed Float
        sampleParamGamma x =
            Maybe.map Distribution.Gamma.sample (gammaDist x 1.0) |> Maybe.withDefault (state 0)
    in
    if a <= 0.5 && b <= 0.5 then
        -- TODO
        state 0

    else if a <= 1.0 && b <= 1.0 then
        -- TODO
        state 0

    else
        State.map2
            (\aGamma bGamma ->
                aGamma / (aGamma + bGamma)
            )
            (sampleParamGamma a)
            (sampleParamGamma b)

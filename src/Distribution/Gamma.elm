module Distribution.Gamma exposing (GammaDist, gammaDist, sample, scale, shape)

{-| Provides functions for describing and sampling a Gamma distribution.
-}

import Rand
import Random exposing (Seed)
import State exposing (State, andThen, state)
import Tuple exposing (first, second)


type GammaDist
    = GammaDist Float Float


gammaDist : Float -> Float -> Maybe GammaDist
gammaDist shape_ scale_ =
    if shape_ <= 0.0 || scale_ <= 0.0 then
        Nothing

    else
        Just (GammaDist shape_ scale_)


shape : GammaDist -> Float
shape (GammaDist sh _) =
    sh


scale : GammaDist -> Float
scale (GammaDist _ sc) =
    sc


sample : GammaDist -> State Seed Float
sample dist =
    let
        distShape =
            shape dist

        distScale =
            scale dist
    in
    if distShape == 1.0 then
        State.map (\r -> distScale * -1.0 * logBase 10 r) Rand.drawUniform

    else if distShape < 1.0 then
        State.map (\r -> distScale * r) (sampleHelper distShape)

    else
        let
            d =
                distShape - 1.0 / 3.0

            c =
                1.0 / sqrt (9.0 * d)
        in
        State.map (\r -> distScale * r) (gammaDeviateRec d c)


sampleHelper : Float -> State Seed Float
sampleHelper s =
    let
        calcResult uv =
            let
                ( u, v ) =
                    uv
            in
            if u <= 1.0 - s then
                let
                    x =
                        u ^ (1.0 / s)
                in
                if x <= v then
                    state x

                else
                    sampleHelper s

            else
                let
                    y =
                        -1.0 * logBase 10 ((1 - u) / s)

                    x =
                        (1.0 - s + s * y) ^ (1.0 / s)
                in
                if x <= v + y then
                    state x

                else
                    sampleHelper s

        drawUV =
            State.map2 (\uR vR -> ( uR, -1.0 * logBase 10 vR )) Rand.drawUniform Rand.drawUniform
    in
    drawUV |> andThen calcResult


gaussianXV : Float -> Float -> Float -> State Seed ( Float, Float )
gaussianXV c x v =
    let
        calcXV : Float -> State Seed ( Float, Float )
        calcXV newX =
            if v > 0.0 then
                state ( x, v )

            else
                gaussianXV c newX (1.0 + c * newX)
    in
    Rand.drawStandardNormal |> andThen calcXV


gammaDeviateRec : Float -> Float -> State Seed Float
gammaDeviateRec d c =
    let
        drawXVU : State Seed ( Float, Float, Float )
        drawXVU =
            State.map2 (\xv u -> ( first xv, second xv, u )) (gaussianXV c 0.0 0.0) Rand.drawUniform

        calcResult : ( Float, Float, Float ) -> State Seed Float
        calcResult xvu =
            let
                ( x, v, u ) =
                    xvu

                v3 =
                    v * v * v

                x2 =
                    x * x

                result =
                    d * v3
            in
            if u < 1.0 - 0.0331 * x2 * x2 || logBase 10 u < 0.5 * x2 + d * (1.0 - v3 + logBase 10 v3) then
                state result

            else
                gammaDeviateRec d c
    in
    drawXVU |> andThen calcResult

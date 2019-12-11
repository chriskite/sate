module Distribution.Gamma exposing (Error, gammaDeviateRec, gaussianXV, sample)

import Rand
import Random exposing (Seed)
import State exposing (State, andThen, state)
import Tuple exposing (first, second)


type Error
    = InvalidShape Float
    | InvalidScale Float


type alias Shape =
    Float


type alias Scale =
    Float


sample : Shape -> Scale -> State Seed (Result Error Float)
sample shape scale =
    if shape <= 0.0 then
        state (Err (InvalidShape shape))

    else if scale <= 0.0 then
        state (Err (InvalidScale scale))

    else if shape == 1.0 then
        State.map (\r -> Ok (scale * -1.0 * logBase 10 r)) Rand.drawUniform

    else if shape < 1.0 then
        State.map (\r -> Ok (scale * r)) (sampleHelper shape)

    else
        let
            d =
                shape - 1.0 / 3.0

            c =
                1.0 / sqrt (9.0 * d)
        in
        State.map (\r -> Ok (scale * r)) (gammaDeviateRec d c)


sampleHelper : Shape -> State Seed Float
sampleHelper shape =
    let
        calcResult uv =
            let
                ( u, v ) =
                    uv
            in
            if u <= 1.0 - shape then
                let
                    x =
                        u ^ (1.0 / shape)
                in
                if x <= v then
                    state x

                else
                    sampleHelper shape

            else
                let
                    y =
                        -1.0 * logBase 10 ((1 - u) / shape)

                    x =
                        (1.0 - shape + shape * y) ^ (1.0 / shape)
                in
                if x <= v + y then
                    state x

                else
                    sampleHelper shape

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

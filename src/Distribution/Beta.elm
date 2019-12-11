module Distribution.Beta exposing (Error, sample)

import Distribution.Gamma
import Random exposing (Seed)
import Result
import State exposing (State, state)


type Error
    = InvalidA Float
    | InvalidB Float
    | GammaDeviateError Distribution.Gamma.Error
    | NotImplemented


sample : Float -> Float -> State Seed (Result Error Float)
sample a b =
    if a <= 0.0 then
        state (Err (InvalidA a))

    else if b <= 0.0 then
        state (Err (InvalidB b))

    else if a <= 0.5 && b <= 0.5 then
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
            (Distribution.Gamma.sample a 1.0)
            (Distribution.Gamma.sample b 1.0)

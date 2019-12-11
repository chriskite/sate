module Rand exposing (drawStandardNormal, drawUniform)

import Random exposing (Seed, float, int, step)
import Random.Float exposing (standardNormal)
import State exposing (State)


drawUniform : State Seed Float
drawUniform =
    State.advance (step (float 0 1))


drawStandardNormal : State Seed Float
drawStandardNormal =
    State.advance (step standardNormal)

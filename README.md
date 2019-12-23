# sate
Statistical Analysis and Tests for Elm

## Multi-Armed Bandit
### Example Usage
```elm
import Random
import Dict
import State
import BayesBandit.Bernoulli exposing (winnerProbabilities)
import Distribution.Bernoulli exposing (bernoulli)

variants = 
    Maybe.map3
        (\a b c -> Dict.fromList [ ("A", a), ("B", b), ("C", c) ])
        (bernoulli 1 10)
        (bernoulli 10 100)
        (bernoulli 11 99)

Maybe.map (\vs -> State.run (Random.initialSeed 42) (winnerProbabilities vs)) variants

{-
Returns: 
Just (Dict.fromList [("A",0.5783366666666667),("B",0.16731),("C",0.2543533333333333)],Seed 1504636316 1013904223)
    : Maybe ( Dict.Dict String Float, Random.Seed )
-}
```

# sate
Statistical Analysis and Tests for Elm

## Multi-Armed Bandit
### Example Usage
```elm
import Random
import Dict
import State
import BayesBandit.Bernoulli exposing (Bernoulli, winnerProbabilities)

variants = Dict.fromList [ ("A", Bernoulli 1 10), ("B", Bernoulli 10 100), ("C", Bernoulli 11 99) ]

State.run (Random.initialSeed 42) (BayesBandit.Bernoulli.winnerProbabilities variants)

{-
Returns: 
(Ok (Dict.fromList [("A",0.5783366666666667),("B",0.16731),("C",0.2543533333333333)]),Seed 1504636316 1013904223)
    : ( Result
            BayesBandit.Bernoulli.Error
            (Dict.Dict BayesBandit.Bernoulli.VariantName Float)
      , Random.Seed
      )
-}
```

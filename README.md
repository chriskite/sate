# sate
Statistical Analysis and Tests for Elm

## Multi-Armed Bandit
### Example Usage
```elm
import Random
import Dict
import State
import BayesBandit.Bernoulli exposing (bernoulli, winnerProbabilities)

variants = 
    Result.map3
        (\a b c -> Dict.fromList [ ("A", a), ("B", b), ("C", c) ])
        (bernoulli 1 10)
        (bernoulli 10 100)
        (bernoulli 11 99)

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

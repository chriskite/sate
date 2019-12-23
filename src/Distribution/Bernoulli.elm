module Distribution.Bernoulli exposing (..)


type Bernoulli
    = Bernoulli Int Int


bernoulli : Int -> Int -> Maybe Bernoulli
bernoulli numSuccesses numFailures =
    if numSuccesses < 0 || numFailures < 0 then
        Nothing

    else
        Just (Bernoulli numSuccesses numFailures)


successes : Bernoulli -> Int
successes (Bernoulli s _) =
    s


failures : Bernoulli -> Int
failures (Bernoulli _ f) =
    f

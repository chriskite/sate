module Distribution.Bernoulli exposing (..)

import Distribution.Beta exposing (BetaDist, alpha, beta, betaDist)


type BernoulliDist
    = BernoulliDist Int Int


bernoulli : Int -> Int -> Maybe BernoulliDist
bernoulli numSuccesses numFailures =
    if numSuccesses < 0 || numFailures < 0 then
        Nothing

    else
        Just (BernoulliDist numSuccesses numFailures)


successes : BernoulliDist -> Int
successes (BernoulliDist s _) =
    s


failures : BernoulliDist -> Int
failures (BernoulliDist _ f) =
    f


posterior : BetaDist -> BernoulliDist -> BetaDist
posterior prior evidence =
    let
        maybePosterior =
            betaDist (alpha prior + (successes >> toFloat) evidence) (beta prior + (failures >> toFloat) evidence)
    in
    Maybe.withDefault Distribution.Beta.uniform maybePosterior

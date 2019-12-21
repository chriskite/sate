module BayesBandit.Bernoulli exposing (Error, bernoulli, choose, winnerProbabilities)

import Dict exposing (Dict)
import Distribution.Beta
import List.Extra exposing (maximumBy)
import Random exposing (Seed)
import Result.Extra
import State exposing (State, Step(..), state, tailRecM)
import Tuple exposing (first, second)


type alias Bernoulli =
    { successes : Int
    , failures : Int
    }


type alias VariantName =
    String


type alias VariantsTimesBest =
    Dict VariantName Int


type Error
    = InvalidBernoulli
    | BetaSamplingError Distribution.Beta.Error
    | NoVariants


bernoulli : Int -> Int -> Result Error Bernoulli
bernoulli successes failures =
    if successes < 0 || failures < 0 then
        Err InvalidBernoulli

    else
        Ok (Bernoulli successes failures)


{-| Given a Dict of variant names and their Bernoulli distributions, return the probabilities that each variant
is the best by Thompson sampling of the conjugate posterior Beta distribution.
-}
winnerProbabilities : Dict VariantName Bernoulli -> State Seed (Result Error (Dict VariantName Float))
winnerProbabilities variants =
    let
        numSamples =
            300000
    in
    thompsonSample variants numSamples
        |> State.map (Result.map (Dict.map (\_ timesBest -> (toFloat <| timesBest) / numSamples)))


{-| Given a Dict of variant names and their Bernoulli distributions, choose a winning variant by sampling
the conjugate posterior Beta distributions and choosing the variant with the highest sampled probability.
-}
choose : Dict VariantName Bernoulli -> State Seed (Result Error VariantName)
choose variants =
    let
        variantSamples : State Seed (Result Error (List ( VariantName, Float )))
        variantSamples =
            variants
                |> Dict.toList
                |> List.map (\vb -> betaSampleVariant (first vb) (second vb))
                |> State.combine
                |> State.map Result.Extra.combine

        bestVariant : List ( a, comparable ) -> Result Error a
        bestVariant vs =
            Result.fromMaybe NoVariants
                (vs |> maximumBy second |> Maybe.map first)
    in
    variantSamples
        |> State.map (Result.andThen <| bestVariant)


{-| Sample from the conjugate posterior Beta distribution for the given Bernoulli distribution,
assuming a uniform prior (1,1)
-}
betaSample : Bernoulli -> State Seed (Result Error Float)
betaSample brn =
    let
        betaResult =
            Distribution.Beta.beta (toFloat brn.successes + 1) (toFloat brn.failures + 1)

        sampleBeta =
            case betaResult of
                Ok beta ->
                    Distribution.Beta.sample beta

                Err e ->
                    state (Err e)
    in
    sampleBeta
        |> State.map (Result.mapError BetaSamplingError)


{-| Given a variant name and Bernoulli distribution, return a sample from the conjugate posterior
Beta distribution along with the variant name
-}
betaSampleVariant : VariantName -> Bernoulli -> State Seed (Result Error ( VariantName, Float ))
betaSampleVariant variant brn =
    betaSample brn |> State.map (Result.map (\b -> ( variant, b )))


{-| Given a Dict of variant names and their Bernoulli distributions, run `numSamples` trials
where the Beta conjugate prior distribution of all variants is sampled, and the highest probability
variant is marked as the winner.
Return a Dict of the variant names with the number of times each variant was the winner.

See the following for more information:
<https://en.wikipedia.org/wiki/Thompson_sampling>
<http://proceedings.mlr.press/v23/agrawal12/agrawal12.pdf>

-}
thompsonSample : Dict VariantName Bernoulli -> Int -> State Seed (Result Error VariantsTimesBest)
thompsonSample variants numSamples =
    let
        {- Sample from the Beta conjugate posterior of all variants, and increment the "times best" count
           for the variant with the highest sampled probability.
           Return Done if that was the last sample requested, or Loop with the updated counts.
        -}
        go ( vsR, n ) =
            if 0 == n then
                state (Done vsR)

            else
                case vsR of
                    Ok vs ->
                        let
                            bestVariant : State Seed (Result Error VariantName)
                            bestVariant =
                                choose (Dict.map (\_ x -> first x) vs)

                            incrTimesBest =
                                Maybe.map (\x -> ( first x, second x + 1 ))

                            updateTimesBest variant =
                                Dict.update variant incrTimesBest vs
                        in
                        bestVariant
                            |> State.map (\bvR -> Loop ( bvR |> Result.map updateTimesBest, n - 1 ))

                    err ->
                        state (Done err)

        -- Create a Dict with 0 as the "number of times best" value for every variant
        initialTimesBest =
            Dict.map (\_ brn -> ( brn, 0 )) variants
    in
    tailRecM go ( Ok initialTimesBest, numSamples )
        -- strip out Bernoullis to return just times best
        |> State.map (Result.map (Dict.map (\_ v -> second v)))

module BayesBandit.Bernoulli exposing (choose, pdfsVis, winnerProbabilities)

import Dict exposing (Dict)
import Distribution.Bernoulli exposing (BernoulliDist, failures, successes)
import Distribution.Beta exposing (BetaDist)
import Float.Extra as Float
import List.Extra exposing (maximumBy)
import Random exposing (Seed)
import State exposing (State, Step(..), state, tailRecM)
import Tuple exposing (first, second)
import VegaLite exposing (..)


type alias LabelledPoint =
    { label : String, x : Float, y : Float }


{-| Given a Dict of variant names and their Bernoulli distributions, return the probabilities that each variant
is the best by Thompson sampling of the conjugate posterior Beta distribution.
-}
winnerProbabilities : Dict String BernoulliDist -> State Seed (Dict String Float)
winnerProbabilities variants =
    let
        numSamples =
            300000
    in
    thompsonSample variants numSamples
        |> State.map (Dict.map (\_ timesBest -> toFloat timesBest / numSamples))


{-| Create a VegaLite visualization Spec for the PDFs of the Beta posteriors of each variant
-}
pdfsVis : Dict String BernoulliDist -> Spec
pdfsVis variants =
    let
        xs =
            Float.range { start = 0, end = 1, steps = 100 }

        posteriors : Dict String BetaDist
        posteriors =
            Dict.map
                (\_ v -> Distribution.Bernoulli.posterior Distribution.Beta.uniform v)
                variants

        pdfPoints : List LabelledPoint
        pdfPoints =
            posteriors
                |> Dict.map
                    (\name posterior ->
                        -- for each variant, calculate the pdf of the posterior for all x
                        List.map (\x -> LabelledPoint name x (Distribution.Beta.pdf posterior x)) xs
                    )
                -- convert to flattened List and throw away extraneous variant name from Dict
                |> Dict.toList
                |> List.concatMap second

        dataValueLists =
            List.map (\lp -> [ ( "x", num lp.x ), ( "y", num lp.y ), ( "name", str lp.label ) ]) pdfPoints

        data =
            List.foldl (\dvl acc -> acc << dataRow dvl) (dataFromRows []) dataValueLists

        enc =
            encoding
                << position X [ pName "x", pMType Quantitative ]
                << position Y [ pName "y", pMType Quantitative ]
                << color [ mName "name", mMType Nominal ]
    in
    toVegaLite [ title "Beta PDFs", data [], enc [], line [ maInterpolate miMonotone ] ]


{-| Given a Dict of variant names and their Bernoulli distributions, choose a winning variant by sampling
the conjugate posterior Beta distributions and choosing the variant with the highest sampled probability.
-}
choose : Dict String BernoulliDist -> State Seed (Maybe String)
choose variants =
    let
        variantSamples : State Seed (List ( String, Float ))
        variantSamples =
            variants
                |> Dict.toList
                |> List.map (\vb -> betaSampleVariant (first vb) (second vb))
                |> State.combine

        bestVariant : List ( a, comparable ) -> Maybe a
        bestVariant vs =
            vs |> maximumBy second |> Maybe.map first
    in
    variantSamples
        |> State.map bestVariant


{-| Sample from the conjugate posterior Beta distribution for the given Bernoulli distribution,
assuming a uniform prior (1,1)
-}
betaSample : BernoulliDist -> State Seed Float
betaSample bernoulli =
    let
        betaResult =
            Distribution.Beta.betaDist (toFloat (successes bernoulli + 1)) (toFloat (failures bernoulli + 1))
    in
    Maybe.map Distribution.Beta.sample betaResult |> Maybe.withDefault (state 0)


{-| Given a variant name and Bernoulli distribution, return a sample from the conjugate posterior
Beta distribution along with the variant name
-}
betaSampleVariant : String -> BernoulliDist -> State Seed ( String, Float )
betaSampleVariant variant bernoulli =
    betaSample bernoulli |> State.map (\b -> ( variant, b ))


{-| Given a Dict of variant names and their Bernoulli distributions, run `numSamples` trials
where the Beta conjugate prior distribution of all variants is sampled, and the highest probability
variant is marked as the winner.
Return a Dict of the variant names with the number of times each variant was the winner.

See the following for more information:
<https://en.wikipedia.org/wiki/Thompson_sampling>
<http://proceedings.mlr.press/v23/agrawal12/agrawal12.pdf>

-}
thompsonSample : Dict String BernoulliDist -> Int -> State Seed (Dict String Int)
thompsonSample variants numSamples =
    let
        {- Sample from the Beta conjugate posterior of all variants, and increment the "times best" count
           for the variant with the highest sampled probability.
           Return Done if that was the last sample requested, or Loop with the updated counts.
        -}
        go ( vs, n ) =
            if 0 == n then
                state (Done vs)

            else
                let
                    maybeBestVariant : State Seed (Maybe String)
                    maybeBestVariant =
                        choose (Dict.map (\_ x -> first x) vs)

                    incrTimesBest =
                        Maybe.map (\x -> ( first x, second x + 1 ))

                    updateTimesBest variant =
                        Dict.update variant incrTimesBest vs
                in
                State.map
                    (\maybeBV ->
                        case maybeBV of
                            Just bv ->
                                Loop ( updateTimesBest bv, n - 1 )

                            Nothing ->
                                Done vs
                    )
                    maybeBestVariant

        -- Create a Dict with 0 as the "number of times best" value for every variant
        initialTimesBest =
            Dict.map (\_ brn -> ( brn, 0 )) variants
    in
    tailRecM go ( initialTimesBest, numSamples )
        -- strip out Bernoullis to return just times best
        |> State.map (Dict.map (\_ v -> second v))

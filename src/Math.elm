module Math exposing (..)

import Float.Extra as Float
import List exposing (map, range, sum)
import List.Extra exposing (zip)
import Tuple exposing (first, second)


exp : Float -> Float
exp x =
    Basics.e ^ x


ln : Float -> Float
ln x =
    logBase Basics.e x


{-| Lanczos approximation of the ln gamma function
<https://en.wikipedia.org/wiki/Lanczos_approximation>
-}
gammaLn : Float -> Float
gammaLn xx =
    let
        cof =
            [ 76.18009172947146, -86.50532032941678, 24.01409824083091, -1.231739572450155, 0.001208650973866179, -0.000005395239384953 ]

        tmp =
            (xx + 5.5) - (xx + 0.5) * ln (xx + 5.5)

        divisors =
            Float.range { start = xx + 1, end = xx + 7, steps = 6 }

        ser =
            1.000000000190015 + sum (zip cof divisors |> map (\p -> first p / second p))
    in
    ln (2.5066282746310007 * ser / xx) - tmp


betaLn : Float -> Float -> Float
betaLn x y =
    gammaLn x + gammaLn y - gammaLn (x + y)

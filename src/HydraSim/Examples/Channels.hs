module HydraSim.Examples.Channels (AWSCenters (..), channel, getSOrError) where

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import HydraSim.Channel

data AWSCenters
    = NVirginiaAWS
    | OhioAWS
    | NCaliforniaAWS
    | OregonAWS
    | CanadaAWS
    | IrelandAWS
    | LondonAWS
    | FrankfurtAWS
    | TokyoAWS
    | SeoulAWS
    | SingaporeAWS
    | SydneyAWS
    | MumbaiAWS
    | SaoPauloAWS
    | GL10
    deriving (Eq, Ord, Bounded, Enum, Show, Read)

{- | Create a delayed channel, with an @S@ taken from measurements between data
 centers across the world.
-}
channel ::
    (MonadAsync m, MonadTimer m, MonadTime m) =>
    AWSCenters ->
    AWSCenters ->
    m (Channel m a, Channel m a)
channel loc loc' =
    createConnectedConstDelayChannels (getSOrError loc loc')

-- | Get the @S@ for two data centers. Errors if the list is incomplete.
getSOrError :: AWSCenters -> AWSCenters -> DiffTime
getSOrError loc loc' =
    case (getS loc loc', getS loc' loc) of
        (Just s, _) -> s
        (_, Just s) -> s
        _ -> error $ "Can't get s for " ++ show (loc, loc')

getS :: AWSCenters -> AWSCenters -> Maybe DiffTime
getS NVirginiaAWS OhioAWS = Just 0.006
getS NVirginiaAWS NCaliforniaAWS = Just 0.037
getS NVirginiaAWS OregonAWS = Just 0.037
getS NVirginiaAWS CanadaAWS = Just 0.007
getS NVirginiaAWS IrelandAWS = Just 0.038
getS NVirginiaAWS LondonAWS = Just 0.038
getS NVirginiaAWS FrankfurtAWS = Just 0.044
getS NVirginiaAWS TokyoAWS = Just 0.083
getS NVirginiaAWS SeoulAWS = Just 0.097
getS NVirginiaAWS SingaporeAWS = Just 0.12
getS NVirginiaAWS SydneyAWS = Just 0.103
getS NVirginiaAWS MumbaiAWS = Just 0.091
getS NVirginiaAWS SaoPauloAWS = Just 0.06
getS OhioAWS NCaliforniaAWS = Just 0.025
getS OhioAWS OregonAWS = Just 0.035
getS OhioAWS CanadaAWS = Just 0.013
getS OhioAWS IrelandAWS = Just 0.04
getS OhioAWS LondonAWS = Just 0.043
getS OhioAWS FrankfurtAWS = Just 0.048
getS OhioAWS TokyoAWS = Just 0.083
getS OhioAWS SeoulAWS = Just 0.096
getS OhioAWS SingaporeAWS = Just 0.112
getS OhioAWS SydneyAWS = Just 0.097
getS OhioAWS MumbaiAWS = Just 0.095
getS OhioAWS SaoPauloAWS = Just 0.065
getS NCaliforniaAWS OregonAWS = Just 0.011
getS NCaliforniaAWS CanadaAWS = Just 0.038
getS NCaliforniaAWS IrelandAWS = Just 0.071
getS NCaliforniaAWS LondonAWS = Just 0.069
getS NCaliforniaAWS FrankfurtAWS = Just 0.073
getS NCaliforniaAWS TokyoAWS = Just 0.059
getS NCaliforniaAWS SeoulAWS = Just 0.071
getS NCaliforniaAWS SingaporeAWS = Just 0.09
getS NCaliforniaAWS SydneyAWS = Just 0.074
getS NCaliforniaAWS MumbaiAWS = Just 0.123
getS NCaliforniaAWS SaoPauloAWS = Just 0.096
getS OregonAWS CanadaAWS = Just 0.034
getS OregonAWS IrelandAWS = Just 0.064
getS OregonAWS LondonAWS = Just 0.079
getS OregonAWS FrankfurtAWS = Just 0.079
getS OregonAWS TokyoAWS = Just 0.052
getS OregonAWS SeoulAWS = Just 0.067
getS OregonAWS SingaporeAWS = Just 0.089
getS OregonAWS SydneyAWS = Just 0.081
getS OregonAWS MumbaiAWS = Just 0.109
getS OregonAWS SaoPauloAWS = Just 0.091
getS CanadaAWS IrelandAWS = Just 0.039
getS CanadaAWS LondonAWS = Just 0.044
getS CanadaAWS FrankfurtAWS = Just 0.051
getS CanadaAWS TokyoAWS = Just 0.084
getS CanadaAWS SeoulAWS = Just 0.096
getS CanadaAWS SingaporeAWS = Just 0.11
getS CanadaAWS SydneyAWS = Just 0.106
getS CanadaAWS MumbaiAWS = Just 0.097
getS CanadaAWS SaoPauloAWS = Just 0.065
getS IrelandAWS LondonAWS = Just 0.005
getS IrelandAWS FrankfurtAWS = Just 0.013
getS IrelandAWS TokyoAWS = Just 0.121
getS IrelandAWS SeoulAWS = Just 0.138
getS IrelandAWS SingaporeAWS = Just 0.087
getS IrelandAWS SydneyAWS = Just 0.142
getS IrelandAWS MumbaiAWS = Just 0.061
getS IrelandAWS SaoPauloAWS = Just 0.092
getS LondonAWS FrankfurtAWS = Just 0.008
getS LondonAWS TokyoAWS = Just 0.126
getS LondonAWS SeoulAWS = Just 0.147
getS LondonAWS SingaporeAWS = Just 0.083
getS LondonAWS SydneyAWS = Just 0.14
getS LondonAWS MumbaiAWS = Just 0.055
getS LondonAWS SaoPauloAWS = Just 0.098
getS FrankfurtAWS TokyoAWS = Just 0.132
getS FrankfurtAWS SeoulAWS = Just 0.155
getS FrankfurtAWS SingaporeAWS = Just 0.083
getS FrankfurtAWS SydneyAWS = Just 0.145
getS FrankfurtAWS MumbaiAWS = Just 0.055
getS FrankfurtAWS SaoPauloAWS = Just 0.104
getS TokyoAWS SeoulAWS = Just 0.016
getS TokyoAWS SingaporeAWS = Just 0.034
getS TokyoAWS SydneyAWS = Just 0.052
getS TokyoAWS MumbaiAWS = Just 0.061
getS TokyoAWS SaoPauloAWS = Just 0.154
getS SeoulAWS SingaporeAWS = Just 0.049
getS SeoulAWS SydneyAWS = Just 0.067
getS SeoulAWS MumbaiAWS = Just 0.077
getS SeoulAWS SaoPauloAWS = Just 0.17
getS SingaporeAWS SydneyAWS = Just 0.085
getS SingaporeAWS MumbaiAWS = Just 0.028
getS SingaporeAWS SaoPauloAWS = Just 0.187
getS SydneyAWS MumbaiAWS = Just 0.111
getS SydneyAWS SaoPauloAWS = Just 0.16
getS MumbaiAWS SaoPauloAWS = Just 0.153
getS x y
    | x == y = Just 0.005
getS _ _ = Nothing

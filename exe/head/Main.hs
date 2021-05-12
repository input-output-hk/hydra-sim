module Main where

import Control.Monad (forM_, unless, when)
import Control.Monad.Class.MonadTime
import Control.Monad.IOSim (ThreadLabel)
import Data.List (intercalate)
import Data.Maybe
import HydraSim.Analyse
import HydraSim.Examples.Baselines
import HydraSim.Examples.Channels
import HydraSim.Options
import HydraSim.Run
import HydraSim.Types
import Numeric.Natural
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO

data Datum = Datum
    { dCapacity :: Natural
    , dTime :: Maybe DiffTime
    , dSnapSize :: Maybe String
    , dNode :: Maybe ThreadLabel
    , dObject :: String
    , dValue :: String
    }

main :: IO ()
main = do
    let parser =
            info
                (cli <**> helper)
                ( fullDesc
                    <> progDesc "Simulations of the Hydra head protocol."
                )
    opts <- execParser parser
    when (verbosity opts > 0) $ print opts
    let fp = output opts
    doesExist <- doesFileExist fp
    let mode = if doesExist then AppendMode else WriteMode
    withFile fp mode $ \h -> do
        unless doesExist (hPutStrLn h csvHeader)

        let baseline scenario =
                Baseline
                    { blScenario = scenario
                    , blConc = FiniteConc (fromIntegral $ concurrency opts)
                    , blBandwidth = fromIntegral $ minimum (networkCapacity opts)
                    , blLocations = regions opts
                    , blAsigTimes = secondsToDiffTimeTriplet $ asigTime opts
                    , blSnapshots = NoSnapshots
                    , blTxType = txType opts
                    }
            baselines =
                concat
                    [ [("full-trust", (baseline FullTrust){blSnapshots = NoSnapshots})]
                    , [("full-trust-infinte-conc", (baseline FullTrust){blConc = UnlimitedConc})]
                    , [ ( "hydra-unlimited"
                        , (baseline HydraUnlimited){blSnapshots = snap}
                        )
                      | snap <- baselineSnapshots opts
                      ]
                    , [ ( "hydra-unlimited-infinte-conc"
                        , (baseline HydraUnlimited)
                            { blConc = UnlimitedConc
                            , blSnapshots = snap
                            }
                        )
                      | snap <- baselineSnapshots opts
                      ]
                    , [("sprites-unlimited", (baseline SpritesUnlimited){blSnapshots = NoSnapshots})]
                    , [("sprites-unlimited-infinte-conc", (baseline SpritesUnlimited){blConc = UnlimitedConc})]
                    ]
            snapsize bl = case blSnapshots bl of
                NoSnapshots -> "infinite"
                SnapAfter n -> show n
        forM_ baselines $ \(scenario, bl) -> do
            let tpsBound =
                    findIntersection
                        bl
                        ( fromIntegral $ minimum (networkCapacity opts)
                        , fromIntegral $ maximum (networkCapacity opts)
                        )
            forM_ tpsBound $ \(capacity, bound) ->
                hPutStrLn h $
                    csvLine
                        opts
                        Datum
                            { dCapacity = fromIntegral capacity
                            , dTime = Nothing
                            , dSnapSize = Just $ snapsize bl
                            , dNode = Nothing
                            , dObject = scenario ++ "-tps"
                            , dValue = show $ tpsTotalBound bound
                            }

        forM_ (networkCapacity opts) $ \capacity -> do
            let minConfTimes = minConfTime ((baseline HydraUnlimited){blBandwidth = fromIntegral capacity})
            forM_ minConfTimes $ \(region, confTime) ->
                hPutStrLn h $
                    csvLine
                        opts
                        Datum
                            { dCapacity = capacity
                            , dTime = Nothing
                            , dSnapSize = Nothing
                            , dNode = Just $ show region
                            , dObject = "min-conftime"
                            , dValue = showt confTime
                            }

            let traceRun = runSimulation opts capacity
            (txs, snaps) <- analyseRun (verbosity opts) traceRun
            writeCSV h opts capacity txs snaps
            when (verbosity opts > 0) $ do
                let tpsUnlimited = baselineTPS (baseline HydraUnlimited)
                    tpsFullTrust = baselineTPS (baseline FullTrust)
                putStrLn $ "Minimal confirmation time: " ++ show (minConfTime $ baseline HydraUnlimited)
                putStrLn $
                    concat
                        [ "Maximal throughput (Hydra Unlimited): "
                        , show tpsUnlimited
                        , percent (tps txs) (tpsTotalBound tpsUnlimited)
                        ]
                putStrLn $
                    concat
                        [ "Maximal throughput (Full Trust): "
                        , show tpsFullTrust
                        , percent (tps txs) (tpsTotalBound tpsFullTrust)
                        ]
  where
    percent :: Double -> Double -> String
    percent x y = concat [" (", show $ x / y * 100, "%)"]

writeCSV :: Handle -> Options -> Natural -> [TxConfirmed] -> [SnConfirmed] -> IO ()
writeCSV h opts capacity txs snaps = do
    hPutStrLn h $
        csvLine
            opts
            Datum
                { dCapacity = capacity
                , dTime = Nothing
                , dSnapSize = Nothing -- TODO: maybe use average snapshot size?
                , dNode = Nothing
                , dObject = "tps"
                , dValue = show $ tps txs
                }
    forM_ (drop (discardEdges opts) . reverse . drop (discardEdges opts) $ txs) $ \tx -> case tx of
        TxConfirmed node t dt ->
            hPutStrLn h $
                csvLine
                    opts
                    Datum
                        { dCapacity = capacity
                        , dTime = Just $ timeToDiffTime t
                        , dSnapSize = Nothing
                        , dNode = Just node
                        , dObject = "conftime-tx"
                        , dValue = showt dt
                        }
        TxUnconfirmed _ _ -> return ()
    forM_ snaps $ \snap -> case snap of
        SnConfirmed node size t dt ->
            hPutStrLn h $
                csvLine
                    opts
                    Datum
                        { dCapacity = capacity
                        , dTime = Just $ timeToDiffTime t
                        , dSnapSize = Just $ show size
                        , dNode = Just node
                        , dObject = "conftime-snap"
                        , dValue = showt dt
                        }
        SnUnconfirmed{} -> return ()

showt :: DiffTime -> String
showt = show . diffTimeToSeconds

timeToDiffTime :: Time -> DiffTime
timeToDiffTime t = t `diffTime` Time 0

showCenterList :: [AWSCenters] -> String
showCenterList centers = intercalate "-" $ map show centers

csvHeader :: String
csvHeader = "bandwidth,txtype,conc,regions,node,clustersize,t,snapsize,object,value"

csvLine :: Options -> Datum -> String
csvLine opts dat =
    intercalate
        ","
        [ show $ dCapacity dat
        , show $ txType opts
        , show $ concurrency opts
        , showCenterList $ regions opts
        , nodeString
        , show (length (regions opts))
        , tString
        , snapSizeString
        , dObject dat
        , dValue dat
        ]
  where
    tString = maybe "na" show $ dTime dat
    snapSizeString = fromMaybe "na" $ dSnapSize dat
    nodeString = fromMaybe "na" $ dNode dat

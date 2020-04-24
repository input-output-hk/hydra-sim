{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (when, forM_)
import Control.Monad.Class.MonadTime
import Data.List (intercalate)
import Data.Semigroup ((<>))
import HydraSim.Analyse
import HydraSim.Examples.Baselines
import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes
import HydraSim.Types
import Numeric.Natural
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)

data CLI = CLI {
  regions :: [AWSCenters],
  networkCapacity :: [Natural],
  txType :: Txs,
  concurrency :: Natural,
  numberTxs :: Natural,
  snapStrategy :: SnapStrategy,
  baselineSnapshots :: [SnapStrategy],
  asigTime :: (Double, Double, Double),
  output :: FilePath,
  discardEdges :: Int,
  verbosity :: Natural
  } deriving Show

cli :: Parser CLI
cli = CLI
  <$> some (argument auto (metavar "NVirginiaAWS | OhioAWS | NCaliforniaAWS | OregonAWS | CanadaAWS | IrelandAWS | LondonAWS | FrankfurtAWS | TokyoAWS | SeoulAWS | SingaporeAWS | SydneyAWS | MumbaiAWS | SaoPauloAWS | GL10"))
  <*> (option auto (short 'b'
                    <> long "bandwidth"
                    <> value [500, 1000, 2000, 3000, 4000, 5000]
                    <> help "Network bandwidth (inbound and outbound) of the nodes, in kbits/s."))
  <*> (option auto (short 't'
                    <> long "txType"
                    <> metavar "Plutus | Simple"
                    <> value Simple
                    <> help "Types of transactions to send."))
  <*> (option auto (short 'c'
                    <> long "concurrency"
                    <> value 1
                    <> help "Determines how many transaction any node will send before older transactions are confirmed."))
  <*> (option auto (short 'n'
                    <> value 50
                    <> help "Number of transactions each node will send."))
  <*> (option auto (long "snapshots"
                    <> help "Sets the strategy for when to create snapshots"
                    <> metavar "NoSnapshots | SnapAfter N"
                    <> value (SnapAfter 1)))
  <*> (option auto (long "baseline-snapshots"
                    <> help "Sets the strategy for when to create snapshots"
                    <> metavar "NoSnapshots | SnapAfter N"
                    <> value [NoSnapshots]))
  <*> (option auto (long "aggregate-signature-time"
                    <> help "time (in seconds) for MSig operations (signing, aggregating, validating)"
                    <> value (0.00015, 0.000010, 0.00085)
                    <> metavar "(T_SIGN, T_AGGREGATE, T_VERIFY)"))
  <*> (strOption (short 'o'
                   <> long "output"
                   <> help "Write output to CSV file"
                   <> value "out.csv" ))
  <*> (option auto (long "discard-edges"
                    <> help "When writing data for confirmation time, discard the first and last N samples (allow for warmup/cooldown)"
                    <> metavar "N"
                    <> value 0))
  <*> (option auto (short 'v'
                    <> long "verbosity"
                    <> value 1
                    <> help "How much to print on the command line." ))

main :: IO ()
main = do
  let parser = info (cli <**> helper)
        ( fullDesc
          <> progDesc "Simulations of the Hydra head protocol.")
  opts <- execParser parser
  when (verbosity opts > 0) $ print opts
  let fp = output opts
  doesExist <- doesFileExist fp
  let mode = if doesExist then AppendMode else WriteMode
  withFile fp mode $ \h -> do
    when (not doesExist) (hPutStrLn h "t,object,conftime,bandwidth,txtype,conc,regions,node,tps,snapsize,clustersize")

    let baseline scenario = Baseline {
          blScenario = scenario,
          blConc = FiniteConc (fromIntegral $ concurrency opts),
          blBandwidth = fromIntegral $ minimum (networkCapacity opts),
          blLocations = regions opts,
          blAsigTimes = secondsToDiffTimeTriplet $ asigTime opts,
          blSnapshots = NoSnapshots,
          blTxType = txType opts
          }
        baselines = concat [
          [("full-trust", (baseline FullTrust) {blSnapshots = NoSnapshots})],
          [("full-trust-infinte-conc", (baseline FullTrust) {blConc = UnlimitedConc})],
          [("hydra-unlimited",
            (baseline HydraUnlimited) {blSnapshots = snap})
          | snap <- baselineSnapshots opts ],
          [("hydra-unlimited-infinte-conc",
            (baseline HydraUnlimited) {blConc = UnlimitedConc,
                                       blSnapshots = snap})
          | snap <- baselineSnapshots opts ]
          ]
        snapsize bl = case blSnapshots bl of
          NoSnapshots -> "infinite"
          SnapAfter n -> show n
    forM_ baselines $ \(scenario, bl) -> do
      let tpsBound = findIntersection bl (fromIntegral $ minimum (networkCapacity opts),
                                          fromIntegral $ maximum (networkCapacity opts))
      forM_ tpsBound $ \(capacity, bound) ->
        hPutStrLn h $ intercalate ","
          [showt 0, scenario, "NA", show $ capacity, show $ txType opts,
           show $ concurrency opts, showCenterList $ regions opts, "NA",
           show (tpsTotalBound bound), snapsize bl, show (length $ regions opts)]



    forM_ (networkCapacity opts) $ \capacity -> do

      let minConfTimes = minConfTime ((baseline HydraUnlimited) {blBandwidth = fromIntegral capacity})
      forM_ minConfTimes $ \(region, confTime) -> do
        hPutStrLn h $ intercalate ","
          [showt 0, "tx-baseline", showt confTime, show $ capacity, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, show region, "NA", "NA", show (length $ regions opts)]


      let specs = flip map (regions opts) $ \center ->
            NodeSpec {nodeRegion = center,
                      nodeNetworkCapacity = fromIntegral capacity,
                      nodeTxs = txType opts,
                      nodeTxConcurrency = fromIntegral $ concurrency opts,
                      nodeTxNumber = fromIntegral $ numberTxs opts,
                      nodeSnapStrategy = snapStrategy opts,
                      nodeASigTime = secondsToDiffTimeTriplet $ asigTime opts
                     }
      (txs, snaps) <- analyseRun (verbosity opts) (runNodes specs)
      writeCSV h opts capacity specs txs snaps
      when (verbosity opts > 0) $ do
        let tpsUnlimited = baselineTPS (baseline HydraUnlimited)
            tpsFullTrust = baselineTPS (baseline FullTrust)
        putStrLn $ concat ["Minimal confirmation time: ", show (minConfTime $ baseline HydraUnlimited)]
        putStrLn $ concat ["Maximal throughput (Hydra Unlimited): ",
                           show tpsUnlimited,
                           percent (tps txs) (tpsTotalBound tpsUnlimited)]
        putStrLn $ concat ["Maximal throughput (Full Trust): ",
                           show tpsFullTrust,
                           percent (tps txs) (tpsTotalBound tpsFullTrust)]
  where
    percent :: Double -> Double -> String
    percent x y = concat [" (", show $ x/y * 100, "%)"]

secondsToDiffTime :: Double -> DiffTime
secondsToDiffTime = picosecondsToDiffTime . round . (*1e12)

secondsToDiffTimeTriplet :: (Double, Double, Double) -> (DiffTime, DiffTime, DiffTime)
secondsToDiffTimeTriplet (a,b,c) = (secondsToDiffTime a, secondsToDiffTime b, secondsToDiffTime c)

writeCSV :: Handle -> CLI -> Natural -> [NodeSpec] -> [TxConfirmed] -> [SnConfirmed] -> IO ()
writeCSV h opts capacity specs txs snaps = do
  let nNodes = length specs
      tpsInRun = tps txs
  forM_ (drop (discardEdges opts) . reverse . drop (discardEdges opts) $ txs) $ \tx -> case tx of
    TxConfirmed node t dt -> hPutStrLn h $ intercalate ","
      [showt (timeToDiffTime t), "tx", showt dt, show $ capacity, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node, show tpsInRun, "NA", show nNodes]
    TxUnconfirmed _ _ -> return ()
  forM_ snaps $ \snap -> case snap of
    SnConfirmed node size t dt -> hPutStrLn h $ intercalate ","
      [showt (timeToDiffTime t), "snap", showt dt, show $ capacity, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node, show tpsInRun, show size, show nNodes]
    SnUnconfirmed _ _ _ -> return ()

showt :: DiffTime -> String
showt = show . diffTimeToSeconds

timeToDiffTime :: Time -> DiffTime
timeToDiffTime t = t `diffTime` Time 0

showCenterList :: [AWSCenters] -> String
showCenterList centers = intercalate "-" $ map show centers

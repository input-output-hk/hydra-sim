{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (when, forM_)
import Control.Monad.Class.MonadTime
import Data.List (intercalate)
import Data.Semigroup ((<>))
import HydraSim.Analyse
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
  networkCapacity :: Natural,
  txType :: Txs,
  concurrency :: Natural,
  numberTxs :: Natural,
  snapStrategy :: SnapStrategy,
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
                    <> value 500
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
  <*> (option auto (long "aggregate-signature-time"
                    <> help "time (in seconds) for MSig operations (signing, aggregating, validating)"
                    <> value (0.0005, 0.0005, 0.0005)
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
  let specs = flip map (regions opts) $ \center ->
        NodeSpec {nodeRegion = center,
                  nodeNetworkCapacity = fromIntegral $ networkCapacity opts,
                  nodeTxs = txType opts,
                  nodeTxConcurrency = fromIntegral $ concurrency opts,
                  nodeTxNumber = fromIntegral $ numberTxs opts,
                  nodeSnapStrategy = snapStrategy opts,
                  nodeASigTime = secondsToDiffTimeTriplet $ asigTime opts
                 }
  when (verbosity opts > 0) $ print opts
  (txs, snaps) <- analyseRun (verbosity opts) (runNodes specs)
  writeCSV opts specs txs snaps
  when (verbosity opts > 0) $ do
    let (minConfTime, maxTPS) = performanceLimit specs
    putStrLn $ concat ["Minimal confirmation time: ", show $ minConfTime]
    putStrLn $ concat ["Maximal throughput: ", show $ maxTPS]

secondsToDiffTime :: Double -> DiffTime
secondsToDiffTime = picosecondsToDiffTime . round . (*1e12)

secondsToDiffTimeTriplet :: (Double, Double, Double) -> (DiffTime, DiffTime, DiffTime)
secondsToDiffTimeTriplet (a,b,c) = (secondsToDiffTime a, secondsToDiffTime b, secondsToDiffTime c)

writeCSV :: CLI -> [NodeSpec] -> [TxConfirmed] -> [SnConfirmed] -> IO ()
writeCSV opts specs txs snaps = do
  let fp = output opts
  doesExist <- doesFileExist fp
  let mode = if doesExist then AppendMode else WriteMode
  withFile fp mode $ \h -> do
        when (not doesExist) (hPutStrLn h "t,object,conftime,bandwidth,txtype,conc,regions,node,tps")
        let tpsInRun = tps txs
        forM_ (drop (discardEdges opts) . reverse . drop (discardEdges opts) $ txs) $ \tx -> case tx of
          TxConfirmed node t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "tx", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node, show tpsInRun]
          TxUnconfirmed _ _ -> return ()
        forM_ snaps $ \snap -> case snap of
          SnConfirmed node _size t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "snap", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node, show tpsInRun]
          SnUnconfirmed _ _ _ -> return ()
        let (minConfTimes, maxTPS) = performanceLimit specs
        forM_ minConfTimes $ \(region, minConfTime) ->
          hPutStrLn h $ intercalate ","
          [showt 0, "tx-baseline", showt minConfTime, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, show region, show $ maxTPS]
showt :: DiffTime -> String
showt = show . diffTimeToSeconds

timeToDiffTime :: Time -> DiffTime
timeToDiffTime t = t `diffTime` Time 0

showCenterList :: [AWSCenters] -> String
showCenterList centers = intercalate "-" $ map show centers

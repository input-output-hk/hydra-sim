module Main where

import Control.Monad (when, forM_)
import Control.Monad.Class.MonadTime
import Data.List (intercalate)
import Data.Semigroup ((<>))
import HydraSim.Analyse
import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO

data CLI = CLI {
  regions :: [AWSCenters],
  networkCapacity :: Integer,
  txType :: Txs,
  concurrency :: Int,
  numberTxs :: Int,
  fileConfTime :: FilePath,
  fileTPS :: FilePath,
  quiet :: Bool
  }

cli :: Parser CLI
cli = CLI
  <$> some (argument auto (metavar "NVirginiaAWS | OhioAWS | NCaliforniaAWS | OregonAWS | CanadaAWS | IrelandAWS | LondonAWS | FrankfurtAWS | TokyoAWS | SeoulAWS | SingaporeAWS | SydneyAWS | MumbaiAWS | SaoPauloAWS | GL10"))
  <*> (option auto (short 'b'
                    <> long "bandwidth"
                    <> help "Network bandwidth (inbound and outbound) of the nodes, in kbits/s."))
  <*> (option auto (short 't'
                    <> long "txType"
                    <> metavar "Plutus | Simple"
                    <> help "Types of transactions to send."))
  <*> (option auto (short 'c'
                    <> long "concurrency"
                    <> help "Determines how many transaction any node will send before older transactions are confirmed."))
  <*> (option auto (short 'n'
                    <> help "Number of transactions each node will send."))
  <*> (strOption (short 'o'
                    <> long "output"
                    <> help "Write confirmation times to this CSV file"
                    <> value "conftime.csv" ))
  <*> (strOption (short 'r'
                    <> long "rate-output"
                    <> help "Write transaction throughput (tps) to this CSV file"
                    <> value "tps.csv" ))
  <*> switch ( long "quiet" <> short 'q' <> help "Whether to be quiet" )

main :: IO ()
main = do
  let parser = info (cli <**> helper)
        ( fullDesc
          <> progDesc "Simulations of the Hydra head protocol.")
  opts <- execParser parser
  let specs = flip map (regions opts) $ \center ->
        NodeSpec {nodeRegion = center,
                  nodeNetworkCapacity = networkCapacity opts,
                  nodeTxs = txType opts,
                  nodeTxConcurrency = concurrency opts,
                  nodeTxNumber = numberTxs opts
                 }
  (txs, snaps) <- analyseRun (quiet opts) (runNodes specs)
  writeConfTimes opts txs snaps
  writeTPS opts txs

writeConfTimes :: CLI -> [TxConfirmed] -> [SnConfirmed] -> IO ()
writeConfTimes opts txs snaps = do
  let fp = fileConfTime opts
  doesExist <- doesFileExist fp
  let mode = if doesExist then AppendMode else WriteMode
  withFile fp mode $ \h -> do
        when (not doesExist) (hPutStrLn h "t,object,conftime,bandwidth,txtype,conc,regions,node")
        forM_ txs $ \tx -> case tx of
          TxConfirmed node t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "tx", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node]
          TxUnconfirmed _ _ -> return ()
        forM_ snaps $ \snap -> case snap of
          SnConfirmed node _size t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "snap", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node]
          SnUnconfirmed _ _ _ -> return ()

writeTPS :: CLI -> [TxConfirmed] -> IO ()
writeTPS opts txs = do
  let fp = fileTPS opts
  doesExist <- doesFileExist fp
  let mode = if doesExist then AppendMode else WriteMode
  withFile fp mode $ \h -> do
        when (not doesExist) (hPutStrLn h "tps,bandwidth,txtype,conc,regions")
        hPutStrLn h $ intercalate ","
            [show (tps txs), show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts]


showt :: DiffTime -> String
showt = show . diffTimeToSeconds

timeToDiffTime :: Time -> DiffTime
timeToDiffTime t = t `diffTime` Time 0

showCenterList :: [AWSCenters] -> String
showCenterList centers = intercalate "-" $ map show centers

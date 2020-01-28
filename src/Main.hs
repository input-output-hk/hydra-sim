module Main where

import Control.Monad (when, forM_)
import Control.Monad.Class.MonadTime
import Data.List (intercalate)
import Data.Semigroup ((<>))
import HydraSim.Analyse
import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes
import Numeric.Natural
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO

data CLI = CLI {
  regions :: [AWSCenters],
  networkCapacity :: Natural,
  txType :: Txs,
  concurrency :: Natural,
  numberTxs :: Natural,
  output :: FilePath,
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
  <*> (strOption (short 'o'
                    <> long "output"
                    <> help "Write output to CSV file"
                    <> value "out.csv" ))
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
                  nodeTxNumber = fromIntegral $ numberTxs opts
                 }
  when (verbosity opts > 0) $ print opts
  (txs, snaps) <- analyseRun (verbosity opts) (runNodes specs)
  writeCSV opts txs snaps

writeCSV :: CLI -> [TxConfirmed] -> [SnConfirmed] -> IO ()
writeCSV opts txs snaps = do
  let fp = output opts
  doesExist <- doesFileExist fp
  let mode = if doesExist then AppendMode else WriteMode
  withFile fp mode $ \h -> do
        when (not doesExist) (hPutStrLn h "t,object,conftime,bandwidth,txtype,conc,regions,node,tps")
        let tpsInRun = tps txs
        forM_ txs $ \tx -> case tx of
          TxConfirmed node t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "tx", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node, show tpsInRun]
          TxUnconfirmed _ _ -> return ()
        forM_ snaps $ \snap -> case snap of
          SnConfirmed node _size t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "snap", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts, node, show tpsInRun]
          SnUnconfirmed _ _ _ -> return ()

showt :: DiffTime -> String
showt = show . diffTimeToSeconds

timeToDiffTime :: Time -> DiffTime
timeToDiffTime t = t `diffTime` Time 0

showCenterList :: [AWSCenters] -> String
showCenterList centers = intercalate "-" $ map show centers

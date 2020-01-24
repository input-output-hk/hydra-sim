module Main where

import Control.Monad (when, forM_)
import Control.Monad.Class.MonadTime
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Time.Clock (diffTimeToPicoseconds)
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
  output :: FilePath,
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
                    <> help "Write output to CSV file"
                    <> value "out.csv" ))
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
  let fp = output opts
  doesExist <- doesFileExist fp
  let mode = if doesExist then AppendMode else WriteMode
  withFile fp mode $ \h -> do
        when (not doesExist) (hPutStrLn h "t,object,conftime,bandwidth,txtype,conc,regions")
        forM_ txs $ \tx -> case tx of
          TxConfirmed t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "tx", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts]
          TxUnconfirmed _ -> return ()
        forM_ snaps $ \snap -> case snap of
          SnConfirmed _size t dt -> hPutStrLn h $ intercalate ","
            [showt (timeToDiffTime t), "snap", showt dt, show $ networkCapacity opts, show $ txType opts, show $ concurrency opts, showCenterList $ regions opts]
          SnUnconfirmed _ _ -> return ()

showt :: DiffTime -> String
showt x = show (1e-12 * fromInteger (diffTimeToPicoseconds x) :: Double)
timeToDiffTime :: Time -> DiffTime
timeToDiffTime t = t `diffTime` Time 0
showCenterList :: [AWSCenters] -> String
showCenterList centers = intercalate "-" $ map show centers

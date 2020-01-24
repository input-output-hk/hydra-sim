module Main where

import HydraSim.Analyse
import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes

import Options.Applicative
import Data.Semigroup ((<>))

data CLI = CLI {
  regions :: [AWSCenters],
  networkCapacity :: Integer,
  txType :: Txs,
  concurrency :: Int,
  numberTxs :: Int
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
  analyseRun (runNodes specs)

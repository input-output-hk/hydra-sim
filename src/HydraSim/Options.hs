module HydraSim.Options where

import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes
import HydraSim.Types
import Numeric.Natural
import Options.Applicative

data Options = Options
  { regions :: [AWSCenters]
  , networkCapacity :: [Natural]
  , txType :: Txs
  , concurrency :: Natural
  , numberTxs :: Natural
  , snapStrategy :: SnapStrategy
  , baselineSnapshots :: [SnapStrategy]
  , protocolFlavor :: ProtocolFlavor
  , asigTime :: (Double, Double, Double)
  , output :: FilePath
  , discardEdges :: Int
  , verbosity :: Natural
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { regions = [FrankfurtAWS, FrankfurtAWS, FrankfurtAWS]
    , networkCapacity = [500, 1000, 2000, 3000, 4000, 5000]
    , txType = Simple
    , concurrency = 1
    , numberTxs = 50
    , snapStrategy = SnapAfter 1
    , baselineSnapshots = [NoSnapshots]
    , protocolFlavor = Vanilla
    , asigTime = (0.00015, 0.000010, 0.00085)
    , output = "out.csv"
    , discardEdges = 0
    , verbosity = 1
    }

cli :: Parser Options
cli =
  Options
    <$> some (argument auto (metavar "NVirginiaAWS | OhioAWS | NCaliforniaAWS | OregonAWS | CanadaAWS | IrelandAWS | LondonAWS | FrankfurtAWS | TokyoAWS | SeoulAWS | SingaporeAWS | SydneyAWS | MumbaiAWS | SaoPauloAWS | GL10"))
    <*> option
      auto
      ( short 'b'
          <> long "bandwidth"
          <> help "Network bandwidth (inbound and outbound) of each node, in kbits/s. It is "
          <> value (networkCapacity defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( short 't'
          <> long "txType"
          <> metavar "Plutus | Simple"
          <> help "Types of transactions to send."
          <> value (txType defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( short 'c'
          <> long "concurrency"
          <> help "Determines how many transaction any node will send before older transactions are confirmed."
          <> value (concurrency defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( short 'n'
          <> help "Number of transactions each node will send."
          <> value (numberTxs defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( long "snapshots"
          <> help "Sets the strategy for when to create snapshots"
          <> metavar "NoSnapshots | SnapAfter N"
          <> value (snapStrategy defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( long "baseline-snapshots"
          <> help "Sets the strategy for when to create snapshots"
          <> metavar "NoSnapshots | SnapAfter N"
          <> value (baselineSnapshots defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( long "protocol-flavor"
          <> help "Sets the flavor of the head protocol to use by nodes"
          <> metavar "Vanilla | CoordinatedVanilla"
          <> value (protocolFlavor defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( long "aggregate-signature-time"
          <> help "time (in seconds) for MSig operations (signing, aggregating, validating)"
          <> value (asigTime defaultOptions)
          <> metavar "(T_SIGN, T_AGGREGATE, T_VERIFY)"
          <> showDefault
      )
    <*> strOption
      ( short 'o'
          <> long "output"
          <> help "Write output to CSV file"
          <> value (output defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( long "discard-edges"
          <> help "When writing data for confirmation time, discard the first and last N samples (allow for warmup/cooldown)"
          <> metavar "N"
          <> value (discardEdges defaultOptions)
          <> showDefault
      )
    <*> option
      auto
      ( short 'v'
          <> long "verbosity"
          <> help "How much to print on the command line. Set it to 4 or more to print debug messages."
          <> value (verbosity defaultOptions)
          <> showDefault
      )

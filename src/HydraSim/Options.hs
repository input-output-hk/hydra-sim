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
    , asigTime :: (Double, Double, Double)
    , output :: FilePath
    , discardEdges :: Int
    , verbosity :: Natural
    }
    deriving (Show)

cli :: Parser Options
cli =
    Options
        <$> some (argument auto (metavar "NVirginiaAWS | OhioAWS | NCaliforniaAWS | OregonAWS | CanadaAWS | IrelandAWS | LondonAWS | FrankfurtAWS | TokyoAWS | SeoulAWS | SingaporeAWS | SydneyAWS | MumbaiAWS | SaoPauloAWS | GL10"))
        <*> option
            auto
            ( short 'b'
                <> long "bandwidth"
                <> value [500, 1000, 2000, 3000, 4000, 5000]
                <> help "Network bandwidth (inbound and outbound) of each node, in kbits/s. It is "
            )
        <*> option
            auto
            ( short 't'
                <> long "txType"
                <> metavar "Plutus | Simple"
                <> value Simple
                <> help "Types of transactions to send."
            )
        <*> option
            auto
            ( short 'c'
                <> long "concurrency"
                <> value 1
                <> help "Determines how many transaction any node will send before older transactions are confirmed."
            )
        <*> option
            auto
            ( short 'n'
                <> value 50
                <> help "Number of transactions each node will send."
            )
        <*> option
            auto
            ( long "snapshots"
                <> help "Sets the strategy for when to create snapshots"
                <> metavar "NoSnapshots | SnapAfter N"
                <> value (SnapAfter 1)
            )
        <*> option
            auto
            ( long "baseline-snapshots"
                <> help "Sets the strategy for when to create snapshots"
                <> metavar "NoSnapshots | SnapAfter N"
                <> value [NoSnapshots]
            )
        <*> option
            auto
            ( long "aggregate-signature-time"
                <> help "time (in seconds) for MSig operations (signing, aggregating, validating)"
                <> value (0.00015, 0.000010, 0.00085)
                <> metavar "(T_SIGN, T_AGGREGATE, T_VERIFY)"
            )
        <*> strOption
            ( short 'o'
                <> long "output"
                <> help "Write output to CSV file"
                <> value "out.csv"
            )
        <*> option
            auto
            ( long "discard-edges"
                <> help "When writing data for confirmation time, discard the first and last N samples (allow for warmup/cooldown)"
                <> metavar "N"
                <> value 0
            )
        <*> option
            auto
            ( short 'v'
                <> long "verbosity"
                <> value 1
                <> help "How much to print on the command line. Set it to 4 or more to print debug messages."
            )

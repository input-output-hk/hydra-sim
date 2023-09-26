{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Tail.Simulation.Options (
  Command (..),
  parseCommand,
  AnalyzeOptions (..),
  defaultAnalyzeOptions,
  RunOptions (..),
  PrepareOptions (..),
  ClientOptions (..),
  ServerOptions (..),
  NetworkCapacity (..),
  kbitsPerSecond,
  Verbosity (..),
  withProgressReport,
) where

import Options.Applicative
import Prelude

import Control.Monad (
  guard,
  when,
 )
import Data.Ratio (
  (%),
 )
import Data.Time.Clock (
  DiffTime,
  picosecondsToDiffTime,
 )
import GHC.Generics (
  Generic,
 )
import Safe (
  initMay,
  lastMay,
  readMay,
 )
import System.Console.ANSI (
  hClearLine,
  setCursorColumn,
 )
import System.IO (
  BufferMode (..),
  hSetBuffering,
  hSetEncoding,
  stdout,
  utf8,
 )
import Text.Pretty.Simple (
  pPrint,
 )

import Hydra.Tail.Simulation.PaymentWindow (
  Lovelace (..),
 )
import Hydra.Tail.Simulation.SlotNo (
  SlotNo (..),
 )
import HydraSim.Examples.Channels (
  AWSCenters (..),
 )
import HydraSim.Sized (
  Size (..),
 )

data Command
  = Prepare PrepareOptions FilePath
  | Run RunOptions FilePath
  | Analyze AnalyzeOptions FilePath
  deriving (Generic, Show)

parseCommand :: IO Command
parseCommand =
  customExecParser (prefs showHelpOnEmpty) parserInfo
 where
  parser = subparser (prepareMod <> runMod <> analyzeMod)

  parserInfo :: ParserInfo Command
  parserInfo =
    info
      (helper <*> parser)
      (progDesc "Hydra Tail Simulation")

prepareMod :: Mod CommandFields Command
prepareMod =
  command "prepare" $
    info
      (helper <*> (Prepare <$> prepareOptionsParser <*> filepathArgument))
      (progDesc "Prepare client events for a simulation")

runMod :: Mod CommandFields Command
runMod =
  command "run" $
    info
      (helper <*> (Run <$> runOptionsParser <*> filepathArgument))
      (progDesc "Run and analyze a simulation given an event schedule")

analyzeMod :: Mod CommandFields Command
analyzeMod =
  command "analyze" $
    info
      (helper <*> (Analyze <$> analyzeOptionsParser <*> filepathArgument))
      (progDesc "Re-analyze results of a previous simulation run")

filepathArgument :: Parser FilePath
filepathArgument =
  strArgument $
    mempty
      <> metavar "FILEPATH"
      <> value "events.csv"
      <> showDefault
      <> completer (bashCompleter "file")
      <> help "Filepath to write to / read from comma-separated events."

data PrepareOptions = PrepareOptions
  { numberOfClients :: Integer
  -- ^ Total number of client
  , duration :: SlotNo
  -- ^ Duration of the simulation, in slots.
  , clientOptions :: ClientOptions
  -- ^ Options specific to each 'Client'
  }
  deriving (Generic, Show)

data RunOptions = RunOptions
  { slotLength :: DiffTime
  -- ^ Slot length
  , paymentWindow :: Maybe Lovelace
  -- ^ payment window parameter (a.k.a W), that is, the budget of each client before needing a snapshot.
  , settlementDelay :: SlotNo
  -- ^ Number of slots needed for a snapshot to be settled.
  , proactiveSnapshot :: Maybe Double
  -- ^ If and when to pro-actively snapshot as a client, e.g. 0.8 would
  -- snapshot when reaching 80% of the window budget.
  , paymentCutOff :: Double
  -- ^ Fraction of the window budget which fixes the maximum admissible payment. 0.5 means that we discard any transaction which is bigger than half the payment window.
  , enableBackupWallet :: Bool
  -- ^ Whether each client should use two wallets with independent snapshot cycles
  , verbosity :: Verbosity
  -- ^ Whether to print progress and additional information.
  , serverOptions :: ServerOptions
  -- ^ Options specific to the 'Server'
  }
  deriving (Generic, Show)

data AnalyzeOptions = AnalyzeOptions
  { discardEdges :: Maybe Int
  , paymentWindow :: Maybe Lovelace
  , settlementDelay :: SlotNo
  }
  deriving (Generic, Show)

defaultAnalyzeOptions :: AnalyzeOptions
defaultAnalyzeOptions = AnalyzeOptions Nothing Nothing 0

prepareOptionsParser :: Parser PrepareOptions
prepareOptionsParser =
  PrepareOptions
    <$> numberOfClientsOption
    <*> durationOption
    <*> clientOptionsOption

numberOfClientsOption :: Parser Integer
numberOfClientsOption =
  option auto $
    mempty
      <> long "number-of-clients"
      <> metavar "INT"
      <> value 1000
      <> showDefault
      <> help "Total / Maximum number of clients in the simulation."

durationOption :: Parser SlotNo
durationOption =
  option (maybeReader readSlotNo) $
    mempty
      <> long "duration"
      <> metavar "SLOT-NO"
      <> value 60
      <> showDefault
      <> help "Duration in slots of the entire simulation."

runOptionsParser :: Parser RunOptions
runOptionsParser =
  RunOptions
    <$> slotLengthOption
    <*> optional paymentWindowOption
    <*> settlementDelayOption
    <*> optional proactiveSnapshotOption
    <*> paymentCutOffOption
    <*> withBackupWalletFlag
    <*> verbosityFlag
    <*> serverOptionsOption

slotLengthOption :: Parser DiffTime
slotLengthOption =
  option (maybeReader readDiffTime) $
    mempty
      <> long "slot-length"
      <> metavar "SECONDS"
      <> value 1
      <> showDefault
      <> help "Length of slot in seconds considered for the simulation."

paymentWindowOption :: Parser Lovelace
paymentWindowOption =
  fmap Lovelace $
    option auto $
      mempty
        <> long "payment-window"
        <> metavar "Lovelace"
        <> help "Payment window parameter (a.k.a. `W`), that is, the budget of each client before needing a snapshot."

settlementDelayOption :: Parser SlotNo
settlementDelayOption =
  option (maybeReader readSlotNo) $
    mempty
      <> long "settlement-delay"
      <> metavar "SLOT-NO"
      <> value 100
      <> showDefault
      <> help "Number of slots needed for a snapshot to be settled."

proactiveSnapshotOption :: Parser Double
proactiveSnapshotOption =
  option auto $
    mempty
      <> long "pro-active-snapshot"
      <> metavar "FRAC"
      <> showDefault
      <> help "At which fraction of the payment window, a client does perform a snapshot pro-actively."

paymentCutOffOption :: Parser Double
paymentCutOffOption =
  option auto $
    mempty
      <> long "payment-cut-off"
      <> metavar "FRAC"
      <> value 1.0
      <> showDefault
      <> help "Fraction of the window budget which fixes the maximum admissible payment. 0.5 means that we discard any transaction which is bigger than half the payment window."

withBackupWalletFlag :: Parser Bool
withBackupWalletFlag =
  flag False True $
    mempty
      <> long "with-backup-wallet"
      <> showDefault
      <> help "Enable a 2nd backup wallets for each client."

verbosityFlag :: Parser Verbosity
verbosityFlag =
  flag Verbose Quiet $
    mempty
      <> long "quiet"
      <> showDefault
      <> help "Turn off progress report."

serverOptionsOption :: Parser ServerOptions
serverOptionsOption =
  ServerOptions
    <$> serverRegionOption
    <*> serverReadCapacityOption
    <*> serverWriteCapacityOption

clientOptionsOption :: Parser ClientOptions
clientOptionsOption =
  ClientOptions
    <$> clientSubmitLikelihoodOption

data ServerOptions = ServerOptions
  { region :: AWSCenters
  -- ^ 'Server' region
  , readCapacity :: NetworkCapacity
  -- ^ 'Server' network read capacity, in KBits/s
  , writeCapacity :: NetworkCapacity
  -- ^ 'Server' network write capacity, in KBits/s
  }
  deriving (Generic, Show)

serverRegionOption :: Parser AWSCenters
serverRegionOption =
  option auto $
    mempty
      <> long "region"
      <> metavar "AWSCenter"
      <> value LondonAWS
      <> showDefault
      <> help "Location for a peer; influence the network latency."
      <> completer (listCompleter awsCenters)
 where
  awsCenters = [show @AWSCenters center | center <- [minBound .. maxBound]]

serverReadCapacityOption :: Parser NetworkCapacity
serverReadCapacityOption =
  networkCapacityOption Read (100 * 1024)

serverWriteCapacityOption :: Parser NetworkCapacity
serverWriteCapacityOption =
  networkCapacityOption Write (100 * 1024)

data ClientOptions = ClientOptions
  { submitLikelihood :: Rational
  -- ^ Likelihood of a 'Client' to submit a transaction at the current slot.
  -- This models the behavior of clients that only go online to check on the
  -- server state but not necessarily submit any transactions.
  }
  deriving (Generic, Show)

clientSubmitLikelihoodOption :: Parser Rational
clientSubmitLikelihoodOption =
  option auto $
    mempty
      <> long "submit-likelihood"
      <> metavar "NUM%DEN"
      <> value (1 % 3)
      <> showDefault
      <> help "Likelihood of a client to submit a transaction at each slot."

analyzeOptionsParser :: Parser AnalyzeOptions
analyzeOptionsParser =
  AnalyzeOptions
    <$> optional discardEdgesOption
    <*> optional paymentWindowOption
    <*> settlementDelayOption

discardEdgesOption :: Parser Int
discardEdgesOption =
  option auto $
    long "discard-edges"
      <> help "Discard the first and last N samples (allow for warmup/cooldown)"
      <> showDefault

--
-- NetworkCapacity
--

data NetworkCapacity = NetworkCapacity
  { rate :: Integer
  -- ^ in KBits/s
  , capacity :: Size -> DiffTime
  -- ^ Measure time needed to transfer a payload of the given 'Size'
  }
  deriving (Generic)

instance Show NetworkCapacity where
  showsPrec i NetworkCapacity{rate} =
    showParen (i >= 10) $ showString (show rate <> " KBits/s")

kbitsPerSecond :: Integer -> NetworkCapacity
kbitsPerSecond rate =
  NetworkCapacity{rate, capacity}
 where
  capacity (Size bytes) =
    fromIntegral bytes * fromRational (recip $ (1024 * toRational rate) / 8)

--
-- Helpers / Internal
--

data Verbosity = Verbose | Quiet deriving (Show, Eq)

readSlotNo :: String -> Maybe SlotNo
readSlotNo = fmap SlotNo . readMay

readDiffTime :: String -> Maybe DiffTime
readDiffTime "∞" =
  pure 1e99
readDiffTime s = do
  lastMay s >>= guard . (== 's')
  n <- readMay @Integer =<< initMay s
  pure $ picosecondsToDiffTime (n * 1_000_000_000_000)

data ReadOrWrite = Read | Write

prettyReadOrWrite :: ReadOrWrite -> String
prettyReadOrWrite = \case
  Read -> "read"
  Write -> "write"

networkCapacityOption :: ReadOrWrite -> Integer -> Parser NetworkCapacity
networkCapacityOption readOrWrite defaultValue =
  fmap kbitsPerSecond $
    option auto $
      mempty
        <> long (readOrWriteS <> "-capacity")
        <> metavar "KBITS/S"
        <> value defaultValue
        <> showDefault
        <> help ("server " <> readOrWriteS <> " network capacity, in KBits/s.")
 where
  readOrWriteS = prettyReadOrWrite readOrWrite

withProgressReport ::
  Show analyze =>
  SlotNo ->
  RunOptions ->
  ((SlotNo -> Maybe analyze -> IO ()) -> IO a) ->
  IO a
withProgressReport lastSlot options io = do
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  a <- case verbosity options of
    Verbose -> io $ \(SlotNo sl) mAnalyze -> do
      setCursorColumn 0
      maybe (pure ()) pPrint mAnalyze
      putStr $ spinner sl <> " Slot  " <> show sl <> "/" <> show (unSlotNo lastSlot)
    Quiet ->
      io $ \_sl _analyze -> pure ()
  when (verbosity options == Verbose) (hClearLine stdout >> setCursorColumn 0)
  return a
 where
  spinner :: Integer -> String
  spinner i = ["◰◳◲◱" !! (fromIntegral i `mod` 4)]

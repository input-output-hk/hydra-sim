{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Tail.Simulation.Options
  ( Command (..)
  , parseCommand

  , RunOptions (..)
  , PrepareOptions (..)
  , ClientOptions(..)
  , ServerOptions(..)

  , NetworkCapacity(..)
  , kbitsPerSecond

  , Verbosity (..)

  , withProgressReport
  ) where

import Options.Applicative
import Prelude

import Control.Monad
    ( guard, when )
import Data.Ratio
    ( (%) )
import Data.Time.Clock
    ( DiffTime, picosecondsToDiffTime )
import GHC.Generics
    ( Generic )
import Safe
    ( initMay, lastMay, readMay )
import System.Console.ANSI
    ( hClearLine, setCursorColumn )
import System.IO
    ( BufferMode (..), hSetBuffering, hSetEncoding, stdout, utf8 )

import Hydra.Tail.Simulation.PaymentWindow
    ( Lovelace, ada )
import Hydra.Tail.Simulation.SlotNo
    ( SlotNo (..) )
import HydraSim.Examples.Channels
    ( AWSCenters (..) )
import HydraSim.Sized
    ( Size (..) )

data Command
  = Prepare PrepareOptions FilePath
  | Run RunOptions FilePath
  deriving (Generic, Show)

parseCommand :: IO Command
parseCommand =
  customExecParser (prefs showHelpOnEmpty) parserInfo
 where
  parser = subparser (prepareMod <> runMod)

  parserInfo :: ParserInfo Command
  parserInfo = info
    (helper <*> parser)
    (progDesc "Hydra Tail Simulation")

prepareMod :: Mod CommandFields Command
prepareMod = command "prepare" $ info
  (helper <*> (Prepare <$> prepareOptionsParser <*> filepathArgument))
  (progDesc "Prepare client events for a simulation")

runMod :: Mod CommandFields Command
runMod = command "run" $ info
  (helper <*> (Run <$> runOptionsParser <*> filepathArgument))
  (progDesc "Run a simulation given an event schedule")

filepathArgument :: Parser FilePath
filepathArgument = strArgument $ mempty
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
  } deriving (Generic, Show)

data RunOptions = RunOptions
  { slotLength :: DiffTime
    -- ^ Slot length
  , paymentWindow :: Maybe Lovelace
    -- ^ payment window parameter (a.k.a W), that is, the budget of each client before needing a snapshot.
  , settlementDelay :: SlotNo
    -- ^ Number of slots needed for a snapshot to be settled.
  , verbosity :: Verbosity
    -- ^ Whether to print progress and additional information.
  , serverOptions :: ServerOptions
    -- ^ Options specific to the 'Server'
  } deriving (Generic, Show)

prepareOptionsParser :: Parser PrepareOptions
prepareOptionsParser = PrepareOptions
  <$> numberOfClientsOption
  <*> durationOption
  <*> clientOptionsOption

numberOfClientsOption :: Parser Integer
numberOfClientsOption = option auto $ mempty
  <> long "number-of-clients"
  <> metavar "INT"
  <> value 1000
  <> showDefault
  <> help "Total / Maximum number of clients in the simulation."

durationOption :: Parser SlotNo
durationOption = option (maybeReader readSlotNo) $ mempty
  <> long "duration"
  <> metavar "SLOT-NO"
  <> value 60
  <> showDefault
  <> help "Duration in slots of the entire simulation."

runOptionsParser :: Parser RunOptions
runOptionsParser = RunOptions
  <$> slotLengthOption
  <*> optional paymentWindowOption
  <*> settlementDelayOption
  <*> verbosityFlag
  <*> serverOptionsOption

slotLengthOption :: Parser DiffTime
slotLengthOption = option (maybeReader readDiffTime) $ mempty
  <> long "slot-length"
  <> metavar "SECONDS"
  <> value 1
  <> showDefault
  <> help "Length of slot in seconds considered for the simulation."

paymentWindowOption :: Parser Lovelace
paymentWindowOption = fmap ada $ option auto $ mempty
  <> long "payment-window"
  <> metavar "ADA"
  <> help "Payment window parameter (a.k.a. `W`), that is, the budget of each client before needing a snapshot."

settlementDelayOption :: Parser SlotNo
settlementDelayOption = option (maybeReader readSlotNo) $ mempty
  <> long "settlement-delay"
  <> metavar "SLOT-NO"
  <> value 100
  <> showDefault
  <> help "Number of slots needed for a snapshot to be settled."

verbosityFlag :: Parser Verbosity
verbosityFlag = flag Verbose Quiet $ mempty
  <> long "quiet"
  <> showDefault
  <> help "Turn off progress report."

serverOptionsOption :: Parser ServerOptions
serverOptionsOption = ServerOptions
  <$> serverRegionOption
  <*> serverConcurrencyOption
  <*> serverReadCapacityOption
  <*> serverWriteCapacityOption

clientOptionsOption :: Parser ClientOptions
clientOptionsOption = ClientOptions
  <$> clientOnlineLikelihoodOption
  <*> clientSubmitLikelihoodOption

data ServerOptions = ServerOptions
  { region :: AWSCenters
    -- ^ 'Server' region
  , concurrency :: Int
    -- ^ 'Server' concurrency capabilities, that is, how many requests can it handle in parallel.
  , readCapacity :: NetworkCapacity
    -- ^ 'Server' network read capacity, in KBits/s
  , writeCapacity :: NetworkCapacity
    -- ^ 'Server' network write capacity, in KBits/s
  } deriving (Generic, Show)

serverRegionOption :: Parser AWSCenters
serverRegionOption = option auto $ mempty
  <> long "region"
  <> metavar "AWSCenter"
  <> value LondonAWS
  <> showDefault
  <> help "Location for a peer; influence the network latency."
  <> completer (listCompleter awsCenters)
 where
  awsCenters = [ show @AWSCenters center | center <- [minBound .. maxBound] ]

serverConcurrencyOption :: Parser Int
serverConcurrencyOption = option auto $ mempty
  <> long "concurrency"
  <> metavar "INT"
  <> value 16
  <> showDefault
  <> help "Number of request the server can process concurrently."

serverReadCapacityOption :: Parser NetworkCapacity
serverReadCapacityOption =
  networkCapacityOption Read (100*1024)

serverWriteCapacityOption :: Parser NetworkCapacity
serverWriteCapacityOption =
  networkCapacityOption Write (100*1024)

data ClientOptions = ClientOptions
  { onlineLikelihood  :: Rational
    -- ^ Likelihood of an offline 'Client' to go online at the current slot.
  , submitLikelihood :: Rational
    -- ^ Likelihood of a 'Client' to submit a transaction at the current slot.
    -- This models the behavior of clients that only go online to check on the
    -- server state but not necessarily submit any transactions.
  } deriving (Generic, Show)

clientOnlineLikelihoodOption :: Parser Rational
clientOnlineLikelihoodOption = option auto $ mempty
  <> long "online-likelihood"
  <> metavar "NUM%DEN"
  <> value (1%10)
  <> showDefault
  <> help "Likelihood of a client to go online on the next slot."

clientSubmitLikelihoodOption :: Parser Rational
clientSubmitLikelihoodOption = option auto $ mempty
  <> long "submit-likelihood"
  <> metavar "NUM%DEN"
  <> value (1%3)
  <> showDefault
  <> help "Likelihood of a client to submit a transaction when it goes online."

--
-- NetworkCapacity
--

data NetworkCapacity = NetworkCapacity
  { rate :: Integer
    -- ^ in KBits/s
  , capacity :: Size -> DiffTime
    -- ^ Measure time needed to transfer a payload of the given 'Size'
  } deriving (Generic)

instance Show NetworkCapacity where
  showsPrec i NetworkCapacity{rate} =
    showParen (i >= 10) $ showString (show rate <> " KBits/s")

kbitsPerSecond :: Integer -> NetworkCapacity
kbitsPerSecond rate =
  NetworkCapacity{rate,capacity}
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
  fmap kbitsPerSecond $ option auto $ mempty
    <> long (readOrWriteS <> "-capacity")
    <> metavar "KBITS/S"
    <> value defaultValue
    <> showDefault
    <> help ("server " <> readOrWriteS <> " network capacity, in KBits/s.")
 where
  readOrWriteS = prettyReadOrWrite readOrWrite

withProgressReport
  :: SlotNo
  -> RunOptions
  -> ((SlotNo -> IO ()) -> IO a)
  -> IO a
withProgressReport lastSlot options io = do
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  a <- case verbosity options of
    Verbose -> io $ \(SlotNo sl) -> do
      setCursorColumn 0
      putStr $ spinner sl <> " SlotNo " <> show sl <> "/" <> show (unSlotNo lastSlot)
    Quiet ->
      io $ \_sl -> pure ()
  when (verbosity options == Verbose) (hClearLine stdout >> setCursorColumn 0)
  return a
 where
  spinner :: Integer -> String
  spinner i = ["◰◳◲◱" !! (fromIntegral i `mod` 4)]

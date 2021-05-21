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
  ) where

import Options.Applicative
import Prelude

import Control.Monad
    ( guard )
import Data.Ratio
    ( (%) )
import Data.Time.Clock
    ( DiffTime, picosecondsToDiffTime )
import GHC.Generics
    ( Generic )
import Safe
    ( initMay, lastMay, readMay )

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
  , serverOptions :: ServerOptions
    -- ^ Options specific to the 'Server'
  } deriving (Generic, Show)

prepareOptionsParser :: Parser PrepareOptions
prepareOptionsParser = PrepareOptions
  <$> numberOfClientsOption
  <*> durationOption
  <*> clientOptionsOption

runOptionsParser :: Parser RunOptions
runOptionsParser = RunOptions
  <$> slotLengthOption
  <*> serverOptionsOption

numberOfClientsOption :: Parser Integer
numberOfClientsOption = option auto $ mempty
  <> long "number-of-clients"
  <> metavar "INT"
  <> value 1000
  <> showDefault
  <> help "Total / Maximum number of clients in the simulation."

slotLengthOption :: Parser DiffTime
slotLengthOption = option (maybeReader readDiffTime) $ mempty
  <> long "slot-length"
  <> metavar "SECONDS"
  <> value 1
  <> showDefault
  <> help "Length of slot in seconds considered for the simulation."

durationOption :: Parser SlotNo
durationOption = option (maybeReader readSlotNo) $ mempty
  <> long "duration"
  <> metavar "SLOT-NO"
  <> value 60
  <> showDefault
  <> help "Duration in slots of the entire simulation."

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

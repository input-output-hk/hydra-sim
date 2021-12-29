{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hydra.Tail.Simulation.Csv (
  writeEvents,
  writeRetries,
  writeTransactions,
  readEventsThrow,
  readRetriesThrow,
  readTransactionsThrow,
) where

import Prelude

import Control.Exception (
  Exception,
 )
import Control.Monad (
  liftM5,
 )
import Control.Monad.Class.MonadThrow (
  throwIO,
 )
import Data.Foldable (
  fold,
 )
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe (
  mapMaybe,
 )
import Data.Text (
  Text,
 )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (
  DiffTime,
 )
import Hydra.Tail.Simulation (
  ClientId,
  Event (..),
  Msg (..),
 )
import Hydra.Tail.Simulation.Analyze (
  Retries,
  Transactions,
 )
import Hydra.Tail.Simulation.MockTx (
  MockTx (..),
  TxRef (..),
  mockTx,
 )
import Hydra.Tail.Simulation.PaymentWindow (
  Lovelace (..),
 )
import Hydra.Tail.Simulation.SlotNo (
  SlotNo (..),
 )
import HydraSim.Analyse (
  diffTimeToSeconds,
 )
import HydraSim.Sized (
  Size (..),
 )
import HydraSim.Tx.Class (
  Tx (..),
 )
import HydraSim.Types (
  NodeId (..),
 )
import Safe (
  readMay,
 )

writeEvents :: FilePath -> [Event] -> IO ()
writeEvents filepath events = do
  TIO.writeFile filepath $
    T.unlines $
      "slot,clientId,event,size,amount,recipients" :
      (eventToCsv <$> events)

readEventsThrow :: FilePath -> IO [Event]
readEventsThrow filepath = do
  text <- TIO.readFile filepath
  case traverse eventFromCsv . drop 1 . T.lines $ text of
    Nothing -> throwIO $ CouldntParseCsv filepath ""
    Just events -> pure events

eventToCsv :: Event -> Text
eventToCsv = \case
  -- slot,clientId,new-tx,size,amount,recipients
  Event (SlotNo sl) (NodeId cl) (NewTx (MockTx _ (Size sz) (Lovelace am) rs)) ->
    T.intercalate
      ","
      [ T.pack (show sl)
      , T.pack (show cl)
      , "new-tx"
      , T.pack (show sz)
      , T.pack (show am)
      , T.intercalate " " (T.pack . show . getNodeId <$> rs)
      ]
  e ->
    error $ "eventToCsv: invalid event to serialize: " <> show e

eventFromCsv :: Text -> Maybe Event
eventFromCsv line =
  case T.splitOn "," line of
    -- slot,clientId,new-tx,size,amount,recipients
    [sl, cl, "new-tx", sz, am, rs] ->
      Event
        <$> readSlotNo sl
        <*> readClientId cl
        <*> ( NewTx
                <$> liftM5
                  mockTx
                  (readClientId cl)
                  (readSlotNo sl)
                  (readAmount am)
                  (readSize sz)
                  (readRecipients rs)
            )
    _ ->
      Nothing
 where
  readClientId :: Text -> Maybe ClientId
  readClientId =
    fmap NodeId . readMay . T.unpack

  readSlotNo :: Text -> Maybe SlotNo
  readSlotNo =
    fmap SlotNo . readMay . T.unpack

  readAmount :: Text -> Maybe Lovelace
  readAmount =
    readMay . T.unpack

  readSize :: Text -> Maybe Size
  readSize =
    fmap Size . readMay . T.unpack

  readRecipients :: Text -> Maybe [ClientId]
  readRecipients = \case
    "" -> Just []
    ssv -> traverse readClientId (T.splitOn " " ssv)

writeTransactions :: FilePath -> Transactions -> IO ()
writeTransactions filepath transactions = do
  TIO.writeFile filepath $
    T.unlines $
      "slot,ref,confirmationTime" :
      mapMaybe toCsv (Map.toList transactions)
 where
  toCsv :: (TxRef MockTx, [DiffTime]) -> Maybe Text
  toCsv (TxRef{slot, ref}, [end, start]) =
    Just $ text slot <> "," <> replaceCommas ref <> "," <> text (diffTimeToSeconds (end - start))
  toCsv _ =
    Nothing

writeRetries :: FilePath -> Retries -> IO ()
writeRetries filepath retries = do
  TIO.writeFile filepath $
    T.unlines $
      "slot,ref,retries" : fmap toCsv (Map.toList retries)
 where
  toCsv :: (TxRef MockTx, Integer) -> Text
  toCsv (TxRef{slot, ref}, n) =
    text slot <> "," <> replaceCommas ref <> "," <> text n

replaceCommas :: Text -> Text
replaceCommas = T.map (\c -> if c == ',' then ';' else c)

readTransactionsThrow :: FilePath -> IO Transactions
readTransactionsThrow filepath = do
  text <- TIO.readFile filepath
  fmap fold . mapM fromCsv $ drop 1 . T.lines $ text
 where
  fromCsv line =
    case T.splitOn "," line of
      [slot, ref, ct] -> do
        i <- readIO $ T.unpack slot
        Map.singleton (TxRef i ref) <$> readConfirmationTimeAsInterval ct
      _ ->
        throwIO $ CouldntParseCsv filepath $ "invalid line: " <> line

  readConfirmationTimeAsInterval t = do
    -- REVIEW(SN): Uses 'NominalDiffTime' for reading, why not everything?
    case readMay (T.unpack t) of
      Nothing -> throwIO $ CouldntParseCsv filepath $ "when parsing confirmation time: " <> t
      Just secs -> pure [realToFrac (secs :: Double), 0]

readRetriesThrow :: FilePath -> IO Retries
readRetriesThrow filepath = do
  text <- TIO.readFile filepath
  fmap fold . mapM fromCsv $ drop 1 . T.lines $ text
 where
  fromCsv line =
    case T.splitOn "," line of
      [slot, ref, retry] -> do
        i <- readIO $ T.unpack slot
        n <- readIO $ T.unpack retry
        pure $ Map.singleton (TxRef i ref) n
      _ ->
        throwIO $ CouldntParseCsv filepath $ "invalid line: " <> line

text :: Show a => a -> Text
text = T.pack . show

data CouldntParseCsv = CouldntParseCsv FilePath Text
  deriving (Show)
instance Exception CouldntParseCsv

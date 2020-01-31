module HydraSim.Examples.Txs
  ( cardanoTx,
    plutusTx
  ) where

import HydraSim.Tx.Mock
import HydraSim.Sized
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = picosecondsToDiffTime . (* 1000000000)

-- | Simple (non-script) address, with @n@ inputs and @n@ outputs.
--
-- Properties are extraced from Cardano benchmarks.
cardanoTx
  :: TxRef (MockTx)
  -> Int
  -- ^ @n@
  -> MockTx
cardanoTx txref nInOuts = MockTx {
  mtxRef = txref,
  -- we have an upper bound of 0.4 ms validation time for Cardano transactions.
  mtxValidationDelay = picosecondsToDiffTime 4 * 1e8,
  -- this is a rough fit for measured transaction sizes.
  mtxSize = Size $ 65 + 100 * nInOuts }

-- | Plutus Script transactions.
--
-- Those are generally larger, and take longer to validate.
--
-- Of course, the actual size and validation times depend on the nature of the
-- scripts, so these are only ballpark numbers.
plutusTx
  :: TxRef (MockTx)
  -> MockTx
plutusTx txref = MockTx {
  mtxRef = txref,
  -- Measurements of exemplatory use cases for Plutus gave us execution times of
  -- 1-5 milliseconds, with a mean of about 2.56 (on a laptop with a 1.6GHz
  -- cpu). Together with the overhead for other checks, we use a delay of 3
  -- milliseconds.
  mtxValidationDelay = millisecondsToDiffTime 3,
  mtxSize = Size $ 10 * 1024 }

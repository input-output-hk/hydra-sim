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
  -- we do not have microbenchmarks for transaction validation at the moment,
  -- but we do know that we are not hitting the limit with 150 transactions
  -- per second.
  mtxValidationDelay = millisecondsToDiffTime 2,
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
  -- cpu). Together with the overhead for signature checking, etc., we use a
  -- delay of 4 milliseconds.
  mtxValidationDelay = millisecondsToDiffTime 4,
  mtxSize = Size $ 10 * 1024 }

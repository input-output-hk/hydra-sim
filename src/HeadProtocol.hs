module HeadProtocol where

import Data.Time.Clock (DiffTime)

newtype TxId = TxId Int
  deriving (Show, Eq, Ord)

data Tx = Tx {
  txId :: TxId,
  txValidationTime :: DiffTime
  }
  deriving (Show, Ord, Eq)

newtype NodeId = NodeId Int
  deriving (Show, Ord, Eq)


data HeadProtocol =
  -- | Send a message to another HeadNode, to request a signature.
    RequestTxSignature Tx
  -- | Respond to a signature request. We jut use the NodeId to keep track of
  -- who has signed a transaction.
  | ProvideTxSignature (Tx, NodeId)
  deriving (Show, Eq)

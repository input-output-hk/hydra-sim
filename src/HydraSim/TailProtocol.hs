module TailProtocol where

data TailServer

data TailClient

data Tx

-- | Addresses affected by this transaction.
txAddresses :: Tx -> [Address]

data Address

data Register

data Pubkey

data Snapshot

-- | Messages sent from the client to the server (and a response is expected).
data ClientMessage
    = Register Pubkey
    | NewTx Tx
    | Subscribe Address
    | Unsubscribe Address
    | Leave

data Response
    = Success
    | Fail

-- | Messages sent from the server to the client upon a subscription.
data ServerMessage = OtherTxSubmitted

-- | The client sends request to the server (and expects a response).
clientRequest :: TailClient -> TailServer -> ClientMessage -> Response

-- | The Server does match a transaction with against a Register which tells him
-- for what client a tx is interesting.
matchTx :: Tx -> Register -> [TailClient]

-- | The Server enqueues a tx to be submitted to a client when that client is
-- online (again).
queueTxForClient :: TailServer -> TailClient -> Tx -> TailServer

-- | The client subscribes to be notified about all txs having outputs to some
-- address.
subscribe :: TailClient -> TailServer -> Address -> Register -> Register

-- | The server finds all txs a client was subscribed to.
findTxForClient :: TailServer -> TailClient -> [Tx]

{-
              Client
             /
         Server
         /    \
    Client   Client

-}

connectToServer :: Client -> Server -> Connection
disconnect :: Client -> Connection -> ()

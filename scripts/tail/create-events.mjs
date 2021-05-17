import { Transform } from 'stream';

/**
 * Post-process transactions and transform them as 'events' for the Tail simulation. An
 * event really is a transaction from a certain client, to some other. Clients are identified
 * by their stake keys. This also discards:
 *
 * (a) Transactions with unknown (i.e. `null`) inputs
 * (b) All unknown (i.e. `null`) outputs from transactions.
 *
 * It also translate and compress the chain both in terms of clients and slot time. That is, the
 * first event will be emitted from slot #0 and the next `compression` slots will be considered the
 * same slot. Also, all wallets will be mapped to clients, with a limited number of clients. Multiple
 * wallets may be therefore associated to the same client, increasing the traffic associated to a
 * single wallet.
 */
export function createEvents({ numberOfClients = 1000, compression = 10 } = {}) {
  const getClientId = newClientIdCreator(numberOfClients);

  const firstTransaction = {};
  const translateSlot = slot => Math.round((slot - firstTransaction.slot) / compression)

  return new Transform({
    transform(chunk, encoding, callback) {
      const txs = JSON.parse(chunk);
      const events = txs.reduce((es, tx) => {
        firstTransaction.slot = firstTransaction.slot || tx.slot;

        // We assume that the first input designate the client fully. In practice, inputs
        // are generally all from the same wallet so it is an okay approximation. There
        // are cases however where the sender is `null` when source client is unknown;
        // we discard such transactions.
        const clientId = getClientId((tx.inputs[0] || {}).wallet);
        if (clientId) {
          return es.concat(mkEvents(getClientId, translateSlot, clientId, tx));
        } else {
          return es;
        }
      }, []);
      callback(null, events.length > 0 ? `${JSON.stringify(events)}\n` : undefined);
    }
  })
}

function mkEvents(getClientId, translateSlot, from, { slot, ref, size, inputs, outputs }) {
  const recipients = outputs
    .map(({ wallet }) => getClientId(wallet))
    .filter(id => id != null && id != from);

  const amount = getAmount(inputs, outputs);

  return amount == 0 ? [] : [
    { slot: translateSlot(slot), from, msg: 'Pull' },
    { slot: translateSlot(slot), from, msg: { NewTx: { ref, size, recipients, amount } } },
  ];
}

function getAmount(inputs, outputs) {
  const inputWallets = inputs.map(x => x == null ? x : x.wallet);
  return outputs
    .filter(({ wallet }) => !inputWallets.includes(wallet))
    .reduce((amount, { value }) => amount + value, 0);
}

// Client ids are created from 1 and onward (0 is reserved for the server).
// It has a maximum number of possible ids, and will cycle from the beginning
// once reached.
function newClientIdCreator(n) {
  const ids = {};

  return stakeKey => {
    if (stakeKey == null) { return null; }
    if (ids[stakeKey]) { return ids[stakeKey]; }

    ids[stakeKey] = 1 + (Object.keys(ids).length % n)

    return ids[stakeKey];
  };
}

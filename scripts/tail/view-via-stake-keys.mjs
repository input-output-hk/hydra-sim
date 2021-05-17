import { Transform } from 'stream';

/**
 * Process blocks from an input file line by line and produce a sequence of transactions
 * where inputs and outputs are mapped to their corresponding stake key hash (if any).
 *
 */
export function viewViaStakeKeys() {
  let ledger = {};
  return new Transform({
    transform(chunk, _encoding, callback) {
      const txs = applyBlock(ledger, JSON.parse(chunk));
      if (txs.length > 0) {
        callback(null, JSON.stringify(txs));
      } else {
        callback();
      }
    },
  });
}

const MAINNET_FEE_CONSTANT = 155381;
const MAINNET_FEE_COEFF = 44;

// Apply one block to our ledger
function applyBlock(ledger, block) {
  return block.body.reduce((transactions, sourceTx) => {
    const tx = applyTransaction(ledger, sourceTx);
    transactions.push({ ...tx, slot: block.header.slot });
    return transactions;
  }, []);
}

function applyTransaction(ledger, sourceTx) {
  const outputs = sourceTx.body.outputs.map((out, index) => {
    const utxo = {
      wallet: getStakeKey(out),
      value: getCoin(out.value),
    };
    const ref = mkTxRef(sourceTx.id, index);
    ledger[ref] = utxo;
    return utxo;
  });

  const inputs = sourceTx.body.inputs.map(({ txId, index }) => {
    const ref = mkTxRef(txId, index);
    const utxo = ledger[ref] || null;
    delete ledger[ref];
    return utxo;
  });

  // Extrapolate the size from the fees, by reversing the fee calculation. Note that in practice
  // there's nothing preventing wallets to put more fees than the minimum required, but it's a
  // good-enough approximation in most cases.
  const size = Math.round((sourceTx.body.fee - MAINNET_FEE_CONSTANT) / MAINNET_FEE_COEFF);

  const tx = { ref: sourceTx.id, size, inputs, outputs };
  return tx;
}

// Make a unique transaction reference to use as our tiny ledger keys
function mkTxRef(txId, index) {
  return txId + '#' + index;
}

// Normalize output values to coin because since 'Mary', outputs may also contain native assets.
function getCoin(value) {
  if (typeof value === 'number') {
    return value;
  } else {
    return value.coins;
  }
}

// Extract a stake key hash from a bech32-encoded address.
function getStakeKey({ address }) {
  if (address.startsWith("addr1") && address.length > 60) {
    return address.slice(52, -6);
  } else {
    return null
  }
}

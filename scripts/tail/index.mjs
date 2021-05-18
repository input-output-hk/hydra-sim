import fs from 'fs';
import assert from 'assert';
import { Readable } from 'stream';
import { downloadIfMissing } from './utils.mjs';
import { downloadChain } from './download-chain.mjs'
import { viewViaStakeKeys } from './view-via-stake-keys.mjs';
import { createEvents } from './create-events.mjs';
import { lineSeparatedFile } from './line-separated-file.mjs';
import * as points from './points-of-interest.mjs';

const reader = await downloadIfMissing('blocks.json', filepath => {
  const url = 'ws://localhost:1337';
  const point = points.lastByronBlock;
  console.log(`Downloading blocks from '${url}'`);
  return Readable.from(downloadChain(url, [point], filepath));
});

const numberOfClients = parseInt(process.argv[2], 10);
assert(typeof numberOfClients === 'number', 'Expected number of clients as 1st argument (e.g. 1000)');

const compression = parseInt(process.argv[3], 10);
assert(typeof compression === 'number' && compression > 1, 'Expected compression rate as 2nd argument (e.g. 10)');

await reader
  .pipe(viewViaStakeKeys())
  .pipe(createEvents({ numberOfClients, compression }))
  .pipe(lineSeparatedFile(`datasets/events-clients:${numberOfClients}-compression:${compression}.csv`))

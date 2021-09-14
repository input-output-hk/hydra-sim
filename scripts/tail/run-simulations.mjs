import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

const CONCURRENCY = 16;
const __dirname = path.resolve();


const base = { "window": 100, "delay": 600, "numberOfClients": 5000, "compression": 3672 }

const clients = [ 2000 ]
const cutOffs    = [ 1.0, 0.8, 0.5 ];
const proActives = [ 0.8, 0.6 ];

const matrix = [];
for (let h in clients) {
  for (let i in cutOffs) {
    for (let j in proActives) {
      matrix.push({ ...base, numberOfClients: clients[h], cutOff: cutOffs[i], proActive: proActives[j] });
    }
  }
}

let cursor = 0;
let pipeline = [];
for (let i = 0; i < CONCURRENCY; i += 1) {
  pipeline.push(schedule())
}
await Promise.all(pipeline);

async function schedule() {
  if (matrix[cursor]) {
    const { window, delay, numberOfClients, compression, cutOff, proActive } = matrix[cursor];
    console.log(`Running simulation ${JSON.stringify(matrix[cursor])}`);
    cursor += 1;
    const filename = `events-clients:${numberOfClients}-compression:${compression}`;
    const writer = fs.createWriteStream(path.join(__dirname, "..", "..", `${filename}-window:${window}-delay:${delay}-proActive:${proActive}-cutOff:${cutOff}`));
    const pipeline = spawn("hydra-tail-simulation", [ "run"
      , "--payment-window", window
      , "--settlement-delay", delay
      , "--pro-active-snapshot", proActive
      , "--payment-cut-off", cutOff
      , path.join(__dirname, "datasets", `${filename}.csv`)
      ])
    const promise = new Promise((resolve) => {
      pipeline.stdout.on('data', chunk => writer.write(chunk));
      pipeline.on('close', () => {
        writer.end();
        resolve()
      });
    });
    return promise.then(schedule)
  }
}

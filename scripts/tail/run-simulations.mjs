import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

const CONCURRENCY = 16;
const __dirname = path.resolve();

const matrix =
  [ { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 0.9, "proActive": 1.0 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 0.8, "proActive": 1.0 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 0.7, "proActive": 1.0 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 0.6, "proActive": 1.0 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 0.5, "proActive": 1.0 }

  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 0.9 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 0.8 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 0.7 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 0.6 }
  , { "window": 100, "delay": 600, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 0.5 }

  , { "window": 100, "delay": 900, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  , { "window": 100, "delay": 800, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  , { "window": 100, "delay": 700, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  , { "window": 100, "delay": 500, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  , { "window": 100, "delay": 400, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  , { "window": 100, "delay": 300, "numberOfClients": 1000, "compression": 4320, "cutOff": 1.0, "proActive": 1.0 }
  ]

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

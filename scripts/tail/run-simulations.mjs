import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

const CONCURRENCY = 16;
const __dirname = path.resolve();

const matrix =
  // Settlement Delay
  [ { "window": 100, "delay":  30, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay":  60, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay":  90, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 150, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 180, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 210, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 240, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 270, numberOfClients: 5000,  compression: 20000 }
  , { "window": 100, "delay": 300, numberOfClients: 5000,  compression: 20000 }

  // Payment Window
  , { "window":   50, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  // , { "window":   100, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  200, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  300, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  400, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  500, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  600, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  700, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  800, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window":  900, "delay": 120, numberOfClients: 5000,  compression: 20000 }
  , { "window": 1000, "delay": 120, numberOfClients: 5000,  compression: 20000 }

  // Number of Clients
  , { "window": 100, "delay": 120, numberOfClients:  1000, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  1500, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  2000, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  2500, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  3000, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  3500, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  4000, compression: 20000 }
  , { "window": 100, "delay": 120, numberOfClients:  4500, compression: 20000 }
  // , { "window": 100, "delay": 120, numberOfClients:  5000, compression: 20000 }
  ]

let cursor = 0;
let pipeline = [];
for (let i = 0; i < CONCURRENCY; i += 1) {
  pipeline.push(schedule())
}
await Promise.all(pipeline);

async function schedule() {
  if (matrix[cursor]) {
    const { window, delay, numberOfClients, compression } = matrix[cursor];
    console.log(`Running simulation ${JSON.stringify(matrix[cursor])}`);
    cursor += 1;
    const filename = `events-clients:${numberOfClients}-compression:${compression}`;
    const writer = fs.createWriteStream(path.join(__dirname, "..", "..", `${filename}-window:${window}-delay:${delay}`));
    const pipeline = spawn("hydra-tail-simulation", [ "run"
      , "--payment-window", window
      , "--settlement-delay", delay
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

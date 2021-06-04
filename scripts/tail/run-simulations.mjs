import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

const CONCURRENCY = 5;
const __dirname = path.resolve();

const matrix =
  // Number of Clients
  [ { "window": 10000, "delay": 120, numberOfClients: 15000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients: 14000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients: 13000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients: 12000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients: 11000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients: 10000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  9000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  8000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  7000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  6000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  5000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  4000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  3000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  2000, compression: 20000 }
  , { "window": 10000, "delay": 120, numberOfClients:  1000, compression: 20000 }

  // Compression / Density
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression:  1000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression:  2500 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression:  5000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 10000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 11000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 12000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 13000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 14000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 15000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 16000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 17000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 18000 }
  , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 19000 }
  // , { "window": 10000, "delay": 120, numberOfClients: 1000,  compression: 20000 }

  // Settlement Delay
  , { "window": 10000, "delay":  30, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay":  60, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay":  90, numberOfClients: 1000,  compression: 20000 }
  // , { "window":  10000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay": 150, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay": 180, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay": 210, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay": 240, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay": 270, numberOfClients: 1000,  compression: 20000 }
  , { "window": 10000, "delay": 300, numberOfClients: 1000,  compression: 20000 }

  // Payment Window
  , { "window":   1000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":   2500, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":   5000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":   7500, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  // , { "window":  10000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":  12500, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":  15000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":  20000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":  25000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  , { "window":  50000, "delay": 120, numberOfClients: 1000,  compression: 20000 }
  ]

for (let i = 0; i < (matrix.length + CONCURRENCY); i += CONCURRENCY) {
  let pipelines = [];

  for (let j = 0; j < CONCURRENCY; j += 1) {
    if (matrix[i+j]) {
      const { window, delay, numberOfClients, compression } = matrix[i+j];
      console.log(`Running simulation ${JSON.stringify(matrix[i+j])}`);
      const filename = `events-clients:${numberOfClients}-compression:${compression}`;
      const writer = fs.createWriteStream(path.join(__dirname, "..", "..", `${filename}-window:${window}-delay:${delay}-clients:${numberOfClients}`));
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
      pipelines.push(promise);
    }
  }

  await Promise.all(pipelines);
}

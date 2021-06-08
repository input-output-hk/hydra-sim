import { spawn } from 'child_process';

const CONCURRENCY = 6;

const matrix =
  [ { numberOfClients: 5000,  compression: 1000,  maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 2500,  maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 5000,  maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 10000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 11000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 12000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 13000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 14000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 15000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 16000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 17000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 18000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 19000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 1000,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 1500,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 2000,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 2500,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 3000,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 3500,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 4000,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 4500,  compression: 20000, maxSlot: 591 }
  , { numberOfClients: 5000,  compression: 20000, maxSlot: 591 }
  ]

for (let i = 0; i < (matrix.length + CONCURRENCY); i += CONCURRENCY) {
  let pipelines = [];

  for (let j = 0; j < CONCURRENCY; j += 1) {
    if (matrix[i+j]) {
      const { numberOfClients, compression, maxSlot } = matrix[i+j];
      console.log(`Running pipeline ${JSON.stringify(matrix[i+j])}`);
      const pipeline = spawn("yarn", ["pipeline", numberOfClients, compression, maxSlot])
      const promise = new Promise((resolve) => pipeline.on('close', () => resolve()));
      pipelines.push(promise);
    }
  }

  await Promise.all(pipelines);
}

#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

const CONCURRENCY = 16;
const __dirname = path.resolve();


const base = { "window": 100, "delay": 600 }
const proActives = [ 1, 0.8, 0.6 ];
const durations = [ 3600 ];
const tpss = [ 250 ];

const matrix = [];
for (let duration of durations) {
  for (let proActive of proActives) {
    for (let tps of tpss) {
      matrix.push({ ...base, proActive, duration, tps });
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
    const { window, delay, duration, tps, proActive } = matrix[cursor];
    console.log(`Running simulation ${JSON.stringify(matrix[cursor])}`);
    cursor += 1;
    const filename = path.join(__dirname, `bitcoin-otc__duration:${duration}-tps:${tps}.csv`)
    const writer = fs.createWriteStream(path.join(__dirname, `${path.basename(filename, ".csv")}-window:${window}-delay:${delay}-proActive:${proActive}`));
    const pipeline = spawn("hydra-tail-simulation", [ "run"
      , "--payment-window", window
      , "--settlement-delay", delay
      , "--pro-active-snapshot", proActive
      , filename
      ])
    const promise = new Promise((resolve) => {
      pipeline.stderr.on('data', chunk => console.log(chunk.toString()))
      pipeline.stdout.on('data', chunk => writer.write(chunk));
      pipeline.on('close', () => {
        writer.end();
        resolve()
      });
    });
    return promise.then(schedule)
  }
}

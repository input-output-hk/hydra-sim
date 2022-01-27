#!/usr/bin/env node

/**
 * Run simulations in parallel (one per core) for the given dataset folder. Corresponding d
 * datasets must have been generated upfront.
 *
 * Usage:
 *     ./run-simulations.mjs
 *
 * Available ENV vars:
 *
 *     CONCURRENCY     Maximum concurrency [default: 16]
 *     PROACTIVE       Comma-separated proactive snapshot parameters [default: 1,0.8,0.6]
 *     TPH             Comma-separated target TPH values [default: 2,1,0.5]
 *     DURATION        Comma-separated durations [default: 1800]
 */

import assert from 'assert';
import fs from "fs";
import path from "path";
import { spawn } from "child_process";

const workdir = process.argv[2];
assert(typeof workdir === 'string', 'Expected directory as 1st argument');

const windows = (process.env["PAYMENT_WINDOW"] || "100").split(",").map(x => parseInt(x, 10));
const delays = (process.env["SETTLEMENT_DELAY"] || "300").split(",").map(x => parseInt(x, 10));
const proActives = (process.env["PROACTIVE"] || "1,0.8,0.6").split(",").map(x => Number(x));
const durations = (process.env["DURATION"] || "1800").split(",").map(x => parseInt(x, 10));
const tphs = (process.env["TPH"] || "2,1,0.5").split(",").map(x => Number(x));

console.log(`SETTLEMENT DELAY:  ${delays}`);
console.log(`PAYMENT WINDOW:    ${windows}`);
console.log(`DURATION:          ${durations}`);
console.log(`TPH:               ${tphs}`);
console.log(`PROACTIVE:         ${proActives}`);
console.log("");

const matrix = [];
for (let window of windows) {
  for (let delay of delays) {
    for (let duration of durations) {
      for (let proActive of proActives) {
        for (let tph of tphs) {
          matrix.push({ window: 2*window, withBackupWallet: false, delay, proActive, duration, tph });
          matrix.push({ window, withBackupWallet: true, delay, proActive, duration, tph });
        }
      }
    }
  }
}

let cursor = 0;
let pipeline = [];
const MAX_CONCURRENCY = parseInt(process.env["CONCURRENCY"] || "16", 10);
for (let i = 0; i < MAX_CONCURRENCY; i += 1) {
  pipeline.push(schedule())
}
await Promise.all(pipeline);

async function schedule() {
  if (matrix[cursor]) {
    const { window, delay, duration, tph, proActive, withBackupWallet } = matrix[cursor];
    console.log(`Running simulation ${JSON.stringify(matrix[cursor])}`);
    cursor += 1;
    const filename = path.join(workdir, `duration:${duration}-tph:${tph}.csv`)
    fs.mkdirSync(path.join(workdir, "simulations"), { recursive: true });
    const writer = fs.createWriteStream(path.join(workdir, "simulations", `${path.basename(filename, ".csv")}-proActive:${proActive}-window:${window}-delay:${delay}-withBackup:${withBackupWallet}.out`));
    const pipeline = spawn("hydra-tail-simulation", [ "run"
      , "--payment-window", window
      , "--settlement-delay", delay
      , "--pro-active-snapshot", proActive
      ].concat(withBackupWallet ? ["--with-backup-wallet"] : [])
       .concat(filename)
    );
    const promise = new Promise((resolve) => {
      pipeline.stderr.on("data", chunk => console.log(chunk.toString()))
      pipeline.stdout.on("data", chunk => writer.write(chunk));
      pipeline.on("close", () => {
        writer.end();
        resolve()
      });
    });
    return promise.then(schedule)
  }
}

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
 *     TPS             Comma-separated target TPS values [default: 100,250,500]
 *     DURATION        Comma-separated durations [default: 3600]
 */

import assert from 'assert';
import fs from "fs";
import path from "path";
import { spawn } from "child_process";

const workdir = process.argv[2];
assert(typeof workdir === 'string', 'Expected directory as 1st argument');

const base = { "window": 100, "delay": 600 }
const proActives = (process.env["PROACTIVE"] || "1,0.8,0.6").split(",").map(x => Number(x));
const durations = (process.env["DURATION"] || "3600").split(",").map(x => parseInt(x, 10));
const tpss = (process.env["TPS"] || "100,250,500").split(",").map(x => parseInt(x, 10));

console.log(`DURATION:  ${durations}`);
console.log(`TPS:       ${tpss}`);
console.log(`PROACTIVE: ${proActives}`);
console.log("");

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
const MAX_CONCURRENCY = parseInt(process.env["CONCURRENCY"] || "16", 10);
for (let i = 0; i < MAX_CONCURRENCY; i += 1) {
  pipeline.push(schedule())
}
await Promise.all(pipeline);

async function schedule() {
  if (matrix[cursor]) {
    const { window, delay, duration, tps, proActive } = matrix[cursor];
    console.log(`Running simulation ${JSON.stringify(matrix[cursor])}`);
    cursor += 1;
    const filename = path.join(workdir, `duration:${duration}-tps:${tps}-proActive:${proActive}.csv`)
    fs.mkdirSync(path.join(workdir, "simulations"), { recursive: true });
    const writer = fs.createWriteStream(path.join(workdir, "simulations", `${path.basename(filename, ".csv")}-window:${window}-delay:${delay}`));
    const pipeline = spawn("hydra-tail-simulation", [ "run"
      , "--payment-window", window
      , "--settlement-delay", delay
      , "--pro-active-snapshot", proActive
      , filename
      ])
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

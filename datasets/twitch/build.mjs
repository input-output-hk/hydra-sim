#!/usr/bin/env node

/* Generate a T2P2 dataset from the "Twitch Gamers Social Network" dataset (https://snap.stanford.edu/data/twitch_gamers.html).
 *
 * Usage:
 *     ./build.mjs
 *     PROACTIVE=0.8 ./build.mjs
 *
 * Available ENV vars:
 *
 *     DURATION   Duration of the simulation, in seconds [default: 3600]
 *     TPS        Target TPS for the simulation (i.e. number of event per step) [default: 250]
 *     PROACTIVE  Pro-active snapshot parameter (i.e. 's') [default: 1]
 */

import fs from "fs";
import path from "path";
import { Writable } from "stream";
import
  { csvWriter
  , forEachLine
  , pickOne
  , someAmount
  , someSize
  , __dirname
  } from "../helpers.mjs";

const INPUT_FILENAME = path.join(__dirname, "twitch", "source.csv");
const DURATION = parseInt(process.env["DURATION"] || "3600", 10);
const TARGET_TPS = parseInt(process.env["TPS"] || "250", 10);
const PROACTIVE_SNAPSHOT = Number(process.env["PROACTIVE"] || "1");
const OUTPUT_FILENAME = path.join(path.dirname(INPUT_FILENAME), `duration:${DURATION}-tps:${TARGET_TPS}-proActive:${PROACTIVE_SNAPSHOT}.csv`)

console.log(`DURATION:  ${DURATION}`);
console.log(`TPS:       ${TARGET_TPS}`);
console.log(`PROACTIVE: ${PROACTIVE_SNAPSHOT}`);
console.log("");

const graph = [];
const nodes = new Map();
let ix = 0;
fs.createReadStream(INPUT_FILENAME)
  .pipe(forEachLine(cols => {
    if (!nodes.has(cols[0])) { nodes.set(cols[0], ix += 1); }
    if (!nodes.has(cols[1])) { nodes.set(cols[1], ix += 1); }
    graph.push({
      a: nodes.get(cols[0]),
      b: nodes.get(cols[1])
    })
  }))
  .on('finish', () => {
    console.log(`Processed graph with ${graph.length} edges & ${nodes.size} nodes!`)
    const writer = csvWriter(OUTPUT_FILENAME);
    writer.write("slot,clientId,event,size,amount,recipients");
    process.stdout.write("Generating events");
    for (let sl = 0; sl < DURATION; sl += 1) {
        if (sl % 10 === 0) { process.stdout.write(".") }
        for (let k = 0; k < TARGET_TPS; k += 1) {
          const edge = pickOne(graph);
          let source, target;
          if (Math.random() > 0.5) {
            source = edge.a;
            target = edge.b;
          } else {
            source = edge.b;
            target = edge.a;
          }
          const ev = [sl, source, "new-tx", someSize(), someAmount(PROACTIVE_SNAPSHOT), target].join(",")
          writer.write(ev);
        }
    }
    process.stdout.write(`\n\nDone → ${OUTPUT_FILENAME}\n`);
    writer.end();
  });

function asc(k) {
  return (a,b) => a[k] - b[k];
}

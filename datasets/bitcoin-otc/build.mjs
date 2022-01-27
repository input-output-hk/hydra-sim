#!/usr/bin/env node

/* Generate a T2P2 dataset from the "Bitcoin OTC trust weighted signed network" (https://snap.stanford.edu/data/soc-sign-bitcoin-otc.html).
 *
 * Usage:
 *     ./build.mjs
 *     PROACTIVE=0.8 ./build.mjs
 *
 * Available ENV vars:
 *
 *     DURATION   Duration of the simulation, in seconds [default: 3600]
 *     TPH        Target throughput per hour PER CLIENT  [default: 3]
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
import
  { buildPriceTable
  } from "../twitchclemson/myhelpers.mjs";

const INPUT_FILENAME = path.join(__dirname, "bitcoin-otc", "source.csv");
const DURATION = parseInt(process.env["DURATION"] || "1800", 10);
const TARGET_TPH = Number(process.env["TPH"] || "1");
const OUTPUT_FILENAME = path.join(path.dirname(INPUT_FILENAME), `duration:${DURATION}-tph:${TARGET_TPH}.csv`)
const PRICE_TABLE_FILENAME = path.join(__dirname, "twitchclemson", "clemsonDistribution.csv");

console.log(`DURATION:   ${DURATION}s`);
console.log(`THROUGHPUT: ${TARGET_TPH} txs per client per hour.`);
console.log("");

const pt = buildPriceTable(PRICE_TABLE_FILENAME);

const graph = [];
const nodes = new Map();
let ix = 0;
fs.createReadStream(INPUT_FILENAME)
  .pipe(forEachLine(cols => {
    if (!nodes.has(cols[0])) { nodes.set(cols[0], ix += 1); }
    if (!nodes.has(cols[1])) { nodes.set(cols[1], ix += 1); }
    graph.push({
      source: Number(nodes.get(cols[0])),
      target: Number(nodes.get(cols[1]))
    });
  }))
  .on('finish', () => {
    console.log(`Processed graph with ${graph.length} edges & ${nodes.size} nodes!`)
    const writer = csvWriter(OUTPUT_FILENAME);
    writer.write("slot,clientId,event,size,amount,recipients");
    const TARGET_TPS = Math.round(TARGET_TPH * nodes.size / 3600);
    process.stdout.write(`Generating events at a rate of ${TARGET_TPS} TPS.`);
    for (let sl = 0; sl < DURATION; sl += 1) {
        if (sl % 10 === 0) { process.stdout.write(".") }
        for (let k = 0; k < TARGET_TPS; k += 1) {
          const edge = pickOne(graph);
          const ev = [sl, edge.source, "new-tx", someSize(), pt.getPrice(), edge.target].join(",")
          writer.write(ev);
        }
    }
    process.stdout.write(`\n\nDone → ${OUTPUT_FILENAME}\n`);
    writer.end();
  });

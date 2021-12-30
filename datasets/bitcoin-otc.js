#!/usr/bin/env node

/* Generate a T2P2 dataset from the "Bitcoin OTC trust weighted signed network" (https://snap.stanford.edu/data/soc-sign-bitcoin-otc.html).
 *
 * Available ENV vars:
 *
 *     BITCOIN_OTC_FILENAME          Path to the dataset (csv) [default: bitcoin-otc.csv]
 *     BITCOIN_OTC_DURATION          Duration of the simulation, in seconds [default: 3600]
 *     BITCOIN_OTC_TARGET_TPS        Target TPS for the simulation (i.e. number of event per step) [default: 250]
 */

const fs = require("fs");
const path = require("path");
const { Writable } = require("stream");
const
  { csvWriter
  , forEachLine
  , pickOne
  , someAmount
  , someSize
  } = require(path.join(__dirname, "helpers.js"));

const FILENAME = process.env["BITCOIN_OTC_FILENAME"] || path.join(__dirname, "bitcoin-otc.csv");
const DURATION = parseInt(process.env["BITCOIN_OTC_DURATION"] || "3600", 10);
const TARGET_TPS = parseInt(process.env["BITCOIN_OTC_TARGET_TPS"] || "250", 10);
const OUTPUT_FILENAME = path.basename(FILENAME, ".csv") + ":" + DURATION + "s:" + TARGET_TPS + "tps.csv"

const graph = [];
fs.createReadStream(FILENAME)
  .pipe(forEachLine(cols => graph.push({
    source: Number(cols[0]),
    target: Number(cols[1])
  })))
  .on('finish', () => {
    const writer = csvWriter(OUTPUT_FILENAME);
    for (let sl = 0; sl < DURATION; sl += 1) {
        for (let k = 0; k < TARGET_TPS; k += 1) {
          const edge = pickOne(graph);
          const ev = [sl, edge.source, "new-tx", someSize(), someAmount(), edge.target].join(",")
          writer.write(ev);
        }
    }
    writer.end();
  });

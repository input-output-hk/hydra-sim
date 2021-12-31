#!/usr/bin/env node

/* Plots information about dataset given as argument.
 *
 * Usage:
 *
 *     ./plot.mjs bitcoin-otc/duration:3600-tps:250-proActive:1.csv
 */

import chartjs from "chartjs-node-canvas";
import fs from "fs";
import assert from "assert";
import path from "path";
import { readCsvFileSync, __dirname } from "./helpers.mjs";

const width = 960;
const height = 500;
const chartJSNodeCanvas = new chartjs.ChartJSNodeCanvas({ width, height });

const inputFile = process.argv[2];
assert(typeof inputFile === 'string', 'Expected input filepath as 1st argument');

const events = readCsvFileSync(inputFile);

await
  [ [path.join(__dirname, "plots", "sizes.svg"), sizes]
  , [path.join(__dirname, "plots", "amounts.svg"), amounts]
  , [path.join(__dirname, "plots", "recipients.svg"), recipients]
  , [path.join(__dirname, "plots", "volume-usd.svg"), volume]
  , [path.join(__dirname, "plots", "density.svg"), density]
  , [path.join(__dirname, "plots", "received-vs-sent.svg"), receivedVsSent]
  ].forEach(async ([filepath, fn]) => {
    console.log(`Plotting: ${filepath}`);
    fs.writeFileSync(filepath, await fn(events));
  });

const outputFile = path.join(__dirname, "plots", "index.html");
fs.writeFileSync(outputFile, `<blockquote><i>generated from: \`${inputFile}\`</i></blockquote>

<h3>Transactions sizes (bytes)</h3>
<img src="sizes.svg">

<h3>Transactions amounts</h3>
<img src="amounts.svg">

<h3>Received vs Sent</h3>
<img src="received-vs-sent.svg">

<h3>Recipients</h3>
<img src="recipients.svg">

<h3>Volume over time</h3>
<img src="volume-usd.svg">

<h3>density</h3>
<img src="density.svg">
`);

console.log(`Done → ${outputFile}`);

async function sizes(events) {
  const bounds =
    [ [0, 192]
    , [192, 512]
    , [512, 1024]
    , [1024, 2048]
    , [2048, 4096]
    , [4096, 8192]
    , [8192, Number.POSITIVE_INFINITY]
    ];

  const labels = bounds.map(([inf, sup]) => sup >= Number.POSITIVE_INFINITY
    ? `${inf}+`
    : `${inf}-${sup}`
  );

  const data = bounds.map(([inf,sup]) => events
    .filter(([_0, _1, _2, size]) => size && Number(size) >= inf && Number(size) < sup)
    .length
  );

  const configuration = {
    type: 'bar',
    data: {
      labels,
      datasets: [{
        label: "transaction size (bytes)",
        backgroundColor: "#e55039",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

async function receivedVsSent(events) {
  const receivedAndSent = events
    .reduce((acc, [_0, clientId, _2, _3, amt, recipients]) => {
      if (amt) {
        acc[clientId] = acc[clientId] || { received: 0, sent: 0 };
        acc[clientId].sent += Number(amt);

        recipients.split(" ").forEach(recipientId => {
          acc[recipientId] = acc[recipientId] || { received: 0, sent: 0 };
          acc[recipientId].received += Number(amt);
        });
      }

      return acc;
    }, {})

  const bounds =
    [ [[0, 4], "~"]
    , [[4, Number.POSITIVE_INFINITY], "🠕"]
    ];

  const data = bounds.map(([[inf, sup], _]) => Object.keys(receivedAndSent)
    .filter(clientId => {
      const { sent, received } = receivedAndSent[clientId];
      const r = received / sent;

      return (r >= 1/(1+sup) && r < 1/(1+inf)) || (r <= 1+sup && r > 1+inf)
    })
    .length
  );

  const labels = bounds.map(([_, lbl]) => lbl);

  const backgroundColor =
    [ "#786fa6"
    , "#cf6a87"
    , "#63cdda"
    , "#596275"
    , "#e77f67"
    , "#f7d794"
    ]

  const configuration = {
    type: 'pie',
    data: {
      labels,
      datasets: [{
        backgroundColor,
        data,
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

async function amounts(events) {
  const bounds =
    [ [0,    1]
    , [1,   10]
    , [10,  20]
    , [20,  30]
    , [30,  40]
    , [40,  50]
    , [50,  60]
    , [60,  70]
    , [70,  80]
    , [80,  90]
    , [90, 100]
    ];

  const labels = bounds.map(([inf, sup]) => sup >= Number.POSITIVE_INFINITY
    ? `${inf}+`
    : `${inf}-${sup}`
  );

  const data = bounds.map(([inf,sup]) => events
    .filter(([_0, _1, _2, _3, amt]) => amt && +amt >= inf && +amt < sup)
    .length
  );

  const configuration = {
    type: 'bar',
    data: {
      labels,
      datasets: [{
        label: "transaction amount",
        backgroundColor: "#82ccdd",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

async function recipients(events) {
  const bounds =
    [ [0, 1]
    , [1, 2]
    , [2, 3]
    , [3, Number.POSITIVE_INFINITY]
    ];

  const labels = bounds.map(([inf, sup]) => sup >= Number.POSITIVE_INFINITY
    ? `${inf}+`
    : `${inf}`
  );

  const data = bounds.map(([inf,sup]) => events
    .filter(([_0, _1, event, _3, _4, recipients]) => {
      if (event != "new-tx") { return false; }
      let n = (recipients && recipients.split(" ").length) || 0;
      return n >= inf && n < sup
    })
    .length
  );

  const configuration = {
    type: 'bar',
    data: {
      labels,
      datasets: [{
        label: "recipients",
        backgroundColor: "#3c6382",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

async function volume(events) {
  const volumePerSlot = events.reduce((acc, [slot,_1,event,_3,amount]) => {
    if (event === "new-tx") {
      acc[slot] = (acc[slot] || 0) + Number(amount);
    }
    return acc;
  }, {});

  const labels = Object.keys(volumePerSlot).sort((a,b) => a-b);
  const data = labels.map(slot => volumePerSlot[slot]);

  const configuration = {
    type: 'line',
    options: {
      scales: {
        x: { display: true, },
        y: { display: true, type: 'logarithmic', }
      }
    },
    data: {
      labels,
      datasets: [{
        label: `volume (in million of ADA, normalized to USD)`,
        fill: true,
        backgroundColor: "#b8e994",
        data,
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

async function density(events) {
  const txsPerSlot = events.reduce((acc, [slot,_1,event]) => {
    if (event === "new-tx") {
      acc[slot] = (acc[slot] || 0) + 1;
    }
    return acc;
  }, {});

  const labels = Object.keys(txsPerSlot).sort((a,b) => a-b);
  const data = labels.map(slot => txsPerSlot[slot]);

  const configuration = {
    type: 'line',
    options: {
      scales: {
        x: { display: true, },
        y: { display: true, type: 'logarithmic', }
      }
    },
    data: {
      labels,
      datasets: [{
        label: "number of transactions",
        fill: true,
        backgroundColor: "#f8c291",
        data,
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

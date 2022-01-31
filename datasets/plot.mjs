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

console.log(`Plotting: sizes`);
const sizesUrl = await sizes(events);
console.log(`Plotting: amounts`);
const amountsUrl = await amounts(events);
console.log(`Plotting: receivedVsSent`);
const receivedVsSentUrl = await receivedVsSent(events);
console.log(`Plotting: recipients`);
const recipientsUrl = await recipients(events);
console.log(`Plotting: volume`);
const volumeUrl = await volume(events);
console.log(`Plotting: density`);
const densityUrl = await density(events);
console.log(`Plotting: busy`);
const busyUrl = await busy(events);

const outputFileName = path.basename(inputFile, '.csv') + "-plots.html";
const outputFile = path.join(path.dirname(inputFile), outputFileName);
fs.writeFileSync(outputFile, `<blockquote><i>generated from: \`${inputFile}\`</i></blockquote>

<h3>Transactions sizes (bytes)</h3>
<img src="${sizesUrl}">

<h3>Transactions amounts</h3>
<img src="${amountsUrl}">

<h3>Received vs Sent</h3>
<img src="${receivedVsSentUrl}">

<h3>Recipients</h3>
<img src="${recipientsUrl}">

<h3>Volume over time</h3>
<img src="${volumeUrl}">

<h3>density</h3>
<img src="${densityUrl}">

<h3>busy</h3>
<img src="${busyUrl}">
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

  return chartJSNodeCanvas.renderToDataURL(configuration);
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
    [ [ 1, "~"]
    , [ 2, "2x"]
    , [ 3, "3x"]
    , [ 4, "4x"]
    , [ Number.POSITIVE_INFINITY, "🠕"]
    ];

  let data = {};
  Object
    .keys(receivedAndSent)
    .forEach(clientId =>  {
      const { sent, received } = receivedAndSent[clientId];
      const r = received / sent;

      for (let [t, lbl] of bounds) {
        if (r > 1/(1+t) && r < 1+t) {
          data[lbl] = data[lbl] || 0;
          data[lbl] += 1;
          break;
        }
      }
    });
  const labels = bounds.map(([_, lbl]) => lbl);
  data = labels.map(lbl => data[lbl]);

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

  return chartJSNodeCanvas.renderToDataURL(configuration);
}

async function amounts(events) {
  const bounds =
    [ [0,   20]
    , [20,  40]
    , [40,  60]
    , [60,  80]
    , [80, 100]
    , [100, 120]
    , [120, 140]
    , [140, 160]
    , [160, 180]
    , [180, 200]
    , [200, 220]
    , [220,  240]
    , [240,  260]
    , [260,  280]
    , [280,  300]
    , [300,  Number.POSITIVE_INFINITY]
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

  return chartJSNodeCanvas.renderToDataURL(configuration);
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

  return chartJSNodeCanvas.renderToDataURL(configuration);
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

  return chartJSNodeCanvas.renderToDataURL(configuration);
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
        y: { display: true, }
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

  return chartJSNodeCanvas.renderToDataURL(configuration);
}

// Find number of client which would not be able to directly snapshot after a new-tx, by:
// - In each event check whether sender or receiver is active in any of the next x slots
// - Sum busy parties and plot them per slot
async function busy(events) {
  const nextSlots = (startIndex, lookahead) => {
    let nextEvents = [];
    let cur = startIndex;
    if (events[cur] == undefined) {
      return nextEvents;
    }
    let slot = Number(events[cur][0]);
    const startSlot = slot;
    while (slot <= startSlot + lookahead) {
      // Only collect future slots
      if (slot > startSlot) {
        nextEvents.push(events[cur]);
      }
      cur += 1;
      if (events[cur] == undefined) {
        break;
      }
      slot = Number(events[cur][0]);
    }
    return nextEvents;
  };

  const countActivity = (clientId, events) =>
    events.filter((e) => e[1] == clientId || e[5] == clientId).length

  let busy = {};
  let nodes = {};
  for (let eix = 1; eix < events.length; eix++) {
    // Skip empty lines
    if (events[eix] == undefined) {
      continue;
    }
    // NOTE: slot is a string!
    const [slot,sender,_event,_size, _amount,recipient] = events[eix];
    nodes[sender] = true;
    nodes[recipient] = true;

    const lookahead = nextSlots(eix+1, 300);
    // console.log(events[eix]);
    // console.log(lookahead);
    // console.log('slot', slot, 'activity of', sender, 'in next 60', countActivity(sender, lookahead));
    // console.log('slot', slot, 'activity of', recipient, 'in next 60', countActivity(recipient, lookahead));

    const senderBusy = countActivity(sender, lookahead) > 0;
    const recipientBusy = countActivity(recipient, lookahead) > 0;
    if (busy[slot] == undefined) {
      busy[slot] = {};
    }
    if (senderBusy) {
      busy[slot][sender] = senderBusy;
    }
    if (recipientBusy) {
      busy[slot][recipient] = recipientBusy;
    }
  }
  // console.log(busy[0])
  // console.log(busy[1])
  const uniqueClientIds = Object.keys(nodes).length;
  console.log('Unique clientIds:', uniqueClientIds);

  const labels = Object.keys(busy).sort((a,b) => a-b);
  const data = labels.map(slot => Object.keys(busy[slot]).length);

  const configuration = {
    type: 'line',
    options: {
      scales: {
        x: { display: true, },
        y: { display: true, }
      }
    },
    data: {
      labels,
      datasets: [{
        label: "number of clients busy in this AND the next 300 slots (no time to snapshot), total: " + uniqueClientIds,
        fill: true,
        backgroundColor: "#f8c291",
        data,
      }],
    },
  };

  return chartJSNodeCanvas.renderToDataURL(configuration);
}

import { ChartJSNodeCanvas } from 'chartjs-node-canvas';
import { lastByronBlock } from './points-of-interest.mjs';
import fs from 'fs';
import assert from 'assert';

const width = 960;
const height = 500;
const chartJSNodeCanvas = new ChartJSNodeCanvas({ width, height });

const inputFile = process.argv[2];
assert(typeof inputFile === 'string', 'Expected input filepath as 1st argument');

const events = readCsvFileSync(inputFile);
const compressionRate = Number(inputFile.split("compression:")[1].split(".")[0]);
const adaToUsd = readCsvFileSync('./datasets/ada-usd-max.csv')
  .map(([snapshotAt, price]) => {
    return { snapshotAt: new Date(snapshotAt), price: Number(price) };
  });

function usd(slot, x) {
  const date = new Date(lastByronBlock.date.getTime() + slot * 1000 * compressionRate);
  const { price } = adaToUsd.filter(({ snapshotAt }) => snapshotAt >= date)[0];
  return price * x;
}

function ada(slot, x) {
  return x;
}

await
  [ ["datasets/plots/sizes.svg", sizes]
  , ["datasets/plots/amounts.svg", amounts]
  , ["datasets/plots/recipients.svg", recipients]
  , ["datasets/plots/volume-ada.svg", volume.bind(null, ada, "ADA")]
  , ["datasets/plots/volume-usd.svg", volume.bind(null, usd, "USD")]
  , ["datasets/plots/density.svg", density]
  ].forEach(async ([filepath, fn]) => {
    console.log(`Plotting: ${filepath}`);
    fs.writeFileSync(filepath, await fn(events));
  });

fs.writeFileSync("datasets/README.md", `> _generated from: \`${inputFile}\`_

### Transactions sizes (bytes)

![](./plots/sizes.svg)

### Transactions amounts (ADA)

![](./plots/amounts.svg)

### Number of recipients

![](./plots/recipients.svg)

### Volume over time (ADA)

![](./plots/volume-ada.svg)

### Volume over time (USD)

![](./plots/volume-usd.svg)

### Number of transactions over time

![](./plots/density.svg)
`);

async function sizes(events) {
  const bounds =
    [ [0, 512]
    , [512, 1024]
    , [1024, Number.POSITIVE_INFINITY]
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

async function amounts(events) {
  const bounds =
    [ [0, 10]
    , [10, 100]
    , [100, 1000]
    , [1000, 10000]
    , [10000, Number.POSITIVE_INFINITY]
    ];

  const labels = bounds.map(([inf, sup]) => sup >= Number.POSITIVE_INFINITY
    ? `${inf}+`
    : `${inf}-${sup}`
  );

  function lovelace(x) {
    return x * 1000000
  }

  const data = bounds.map(([inf,sup]) => events
    .filter(([_0, _1, _2, _3, amt]) => amt && +amt >= lovelace(inf) && +amt < lovelace(sup))
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

async function volume(convert, unit, events) {
  const volumePerSlot = events.reduce((acc, [slot,_1,event,_3,amount]) => {
    if (event === "new-tx") {
      acc[slot] = (acc[slot] || 0) + Number(amount);
    }
    return acc;
  }, {});

  const labels = Object.keys(volumePerSlot).sort((a,b) => a-b);
  const data = labels.map(slot => convert(slot, volumePerSlot[slot]));

  const configuration = {
    type: 'line',
    options: {
      scales: {
        x: {
          display: true,
        },
        y: {
          display: true,
          type: 'logarithmic',
        }
      }
    },
    data: {
      labels,
      datasets: [{
        label: `volume (in million of ${unit})`,
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
        x: {
          display: true,
        },
        y: {
          display: true,
          type: 'logarithmic',
        }
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

function readCsvFileSync(filepath) {
  return fs.readFileSync(filepath)
    .toString()
    .split("\n")
    .slice(1)
    .map(s => s.split(","));
}

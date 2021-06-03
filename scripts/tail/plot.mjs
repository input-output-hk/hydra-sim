import { ChartJSNodeCanvas } from 'chartjs-node-canvas';
import fs from 'fs';
import assert from 'assert';
import { readCsvFileSync } from './utils.mjs';

const width = 960;
const height = 500;
const chartJSNodeCanvas = new ChartJSNodeCanvas({ width, height });

const inputFile = process.argv[2];
assert(typeof inputFile === 'string', 'Expected input filepath as 1st argument');

const events = readCsvFileSync(inputFile);

const [total, count] = events
  .reduce(([total, count], [_0, _1, _2, _3, amt]) => {
    if (amt) {
      return [total + Number(amt), count + 1]
    } else {
      return [total, count];
    }

  }, [0,0]);

console.log("Average transaction amount:", Math.round(total/(1e6 * count)));

await
  [ ["datasets/plots/sizes.svg", sizes]
  , ["datasets/plots/amounts.svg", amounts]
  , ["datasets/plots/recipients.svg", recipients]
  , ["datasets/plots/volume-usd.svg", volume]
  , ["datasets/plots/density.svg", density]
  , ["datasets/plots/received-vs-sent.svg", receivedVsSent]
  ].forEach(async ([filepath, fn]) => {
    console.log(`Plotting: ${filepath}`);
    fs.writeFileSync(filepath, await fn(events));
  });

fs.writeFileSync("datasets/README.md", `> _generated from: \`${inputFile}\`_

### Transactions sizes (bytes)

![](./plots/sizes.svg)

### Transactions amounts

![](./plots/amounts.svg)

### Received vs Sent Payments

![](./plots/received-vs-sent.svg)

### Number of recipients

![](./plots/recipients.svg)

### Volume over time

![](./plots/volume-usd.svg)

### Number of transactions over time

![](./plots/density.svg)
`);

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
    [ [0, 1]
    , [1, 10]
    , [10, 100]
    , [100, 1000]
    , [1000, 10000]
    , [10000, 100000]
    , [100000, 1000000]
    , [1000000, Number.POSITIVE_INFINITY]
    ];

  const labels = bounds.map(([inf, sup]) => sup >= Number.POSITIVE_INFINITY
    ? `${inf}+`
    : `${inf}-${sup}`
  );

  const data = bounds.map(([inf,sup]) => events
    .filter(([_0, _1, _2, _3, amt]) => amt && +amt >= inf * 1e6 && +amt < sup * 1e6)
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
  const data = labels.map(slot => volumePerSlot[slot] / 1e6);

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

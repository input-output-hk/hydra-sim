import chartjs from 'chartjs-node-canvas';
import fs from 'fs';
import assert from 'assert';
import { readCsvFileSync } from './utils.mjs';

const width = 1920;
const height = 1080;
const chartJSNodeCanvas = new chartjs.ChartJSNodeCanvas({ width, height });

const inputFile = process.argv[2];
assert(typeof inputFile === 'string', 'Expected "confirmedTransactions.csv" input filepath as 1st argument');

const txs = readCsvFileSync(inputFile);
fs.writeFileSync("confirmationTimes.svg", await confirmationTimes(txs));

async function confirmationTimes(txs) {
  let data = [];
  for (let i = 0; i < txs.length; i++) {
    // Parse scientific notation into 'Number'
    const t = txs[i][1];
    if (t != undefined) {
      const confTime = JSON.parse(t);
      data.push({ x: i, y: confTime });
    }
  }
  const configuration = {
    type: 'scatter',
    options: {
      scales: {
        x: { display: true, },
        y: { display: true, }
      }
    },
    data: {
      // labels,
      datasets: [{
        label: "confirmationTimes",
        fill: true,
        backgroundColor: "#f8c291",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

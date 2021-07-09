import chartjs from 'chartjs-node-canvas';
import fs from 'fs';
import assert from 'assert';
import { readCsvFileSync } from './utils.mjs';

const width = 1920;
const height = 1080;
const chartJSNodeCanvas = new chartjs.ChartJSNodeCanvas({ width, height });

const inputFile = process.argv[2];
assert(typeof inputFile === 'string', 'Expected "txs.csv" input filepath as 1st argument');

const txs = readCsvFileSync(inputFile);
const svgFile = inputFile.replace("csv", "svg");
fs.writeFileSync(svgFile, await confirmationTimes(svgFile.replace(".svg",""), txs));

async function confirmationTimes(name, txs) {
  let data = txs.filter((entry) => entry.length == 3).map((entry) => {
    const amount = JSON.parse(entry[1]);
    const confTime = JSON.parse(entry[2]);
    return {x: amount, y: confTime};
  });
  const configuration = {
    type: 'scatter',
    options: {
      scales: {
        x: { title: "amount", },
        y: { title: "confirmation time" }
      }
    },
    data: {
      datasets: [{
        label: name,
        fill: true,
        backgroundColor: "#f8c291",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

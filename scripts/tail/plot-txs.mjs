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
  const data = txs.map((entry) => {
    // Parse scientific notation into 'Number'
    const t1 = entry[0];
    const t2 = entry[2];
    if (t1 != undefined && t2 != undefined) {
      const slot = JSON.parse(t1);
      const confTime = JSON.parse(t2);
      return { x: slot, y: confTime };
    }
  });
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
        label: name,
        fill: true,
        backgroundColor: "#f8c291",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}

import fs from 'fs';
import readline from 'readline';
import { Transform, Readable, Writable } from 'stream';

export function createReadStreamByLine(filepath) {
  return Readable.from(readline.createInterface({
    input: fs.createReadStream(filepath)
  }));
}

export async function downloadIfMissing(filepath, download) {
  return new Promise(resolve => {
    fs.access(filepath, fs.constants.F_OK, exists => {
      if (!exists) {
        console.log(`'${filepath}' already exists; using local file...`);
        resolve(createReadStreamByLine(filepath));
      } else {
        resolve(download(filepath));
      }
    });
  });
}

export function readCsvFileSync(filepath) {
  return fs.readFileSync(filepath)
    .toString()
    .split("\n")
    .slice(1)
    .map(s => s.split(","));
}

export function zip(xs, ys) {
  return xs.reduce((acc, x, i) => {
    if (i < ys.length) {
      acc.push([x, ys[i]])
    }
    return acc;
  }, [])
}

export function unzip(xs) {
  return xs.reduce((acc, [x,y]) => {
    acc[0].push(x)
    acc[1].push(y)
    return acc;
  }, [[],[]])
}

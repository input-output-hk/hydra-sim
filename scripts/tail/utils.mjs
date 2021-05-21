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

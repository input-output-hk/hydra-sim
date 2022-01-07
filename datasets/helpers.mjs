import fs from "fs";
import { Writable, Transform } from "stream";
import { erf } from "mathjs";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const __filename = fileURLToPath(import.meta.url);

export const __dirname = path.dirname(__filename);

// Size distribution is extrapolated from real network data
export function someSize() {
  return frequency(
    [ [ 318, () => randomR ( 192,   512) ]
    , [ 129, () => randomR ( 512,  1024) ]
    , [ 37,  () => randomR (1024,  2048) ]
    , [ 12,  () => randomR (2048,  4096) ]
    , [ 43,  () => randomR (4096,  8192) ]
    , [ 17,  () => randomR (8192, 16384) ]
    ]);
}

export function someAmount(s = 1) {
  function w(min, max) {
    return [ 1e6 * (logNormalCdf(max, s) - logNormalCdf(min, s)), () => randomR(min, max) ]
  }
  return frequency(
    [ w( 0,  25)
    , w(25,  40)
    , w(40,  50)
    , w(50,  60)
    , w(60,  70)
    , w(70,  80)
    , w(80,  90)
    , w(90, 100)
    ]);
}

export function logNormalCdf(x, s = 1, sigma = 0.25) {
  const mu = Math.log(50 * s);
  return 0.5*(1+erf((Math.log(x)-mu)/(sigma*Math.sqrt(2))))
}

export function randomR(min, max) {
  return Math.round(min + Math.random() * (max - min));
}

export function pickOne(xs) {
  return xs[Math.floor(Math.random() * xs.length)];
}

export function frequency(xs) {
    function pick($p, $xs) {
      if ($xs.length == 0) {
        throw "frequency: empty list of generators";
      }
      const [weight, gen] = $xs[0];
      if ($p <= weight) {
        return gen();
      }
      return pick($p - weight, $xs.slice(1))
    }

    const total = xs.reduce((t, x) => t+x[0], 0);
    const p = randomR(1, total);

    return pick(p, xs);
}

export function forEachLine(next) {
  let buffer = "";
  return new Transform({
    transform(chunk, _encoding, callback) {
      buffer += chunk.toString();
      for (const line of buffer.split("\n").slice(0, -1)) {
        const cols = line.split(",");
        next(cols);
      }
      buffer = buffer.split("\n").slice(-1);
      callback();
    }
  });
}

export function readCsvFileSync(filepath) {
  return fs.readFileSync(filepath)
    .toString()
    .split("\n")
    .slice(1)
    .map(s => s.split(","));
}

export function csvWriter(filepath) {
  const file = fs.createWriteStream(filepath);

  return new Writable({
    write(chunk, _encoding, callback) {
      file.write(Buffer.concat([chunk, Buffer.from("\n")]))
      callback();
    }
  })
}

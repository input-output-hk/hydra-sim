const fs = require("fs");
const { Writable, Transform } = require("stream");

// Size distribution is extrapolated from real network data
function someSize() {
  return frequency(
    [ [318, () => randomR (192, 512)]
    , [129, () => randomR (512, 1024)]
    , [37, () => randomR (1024, 2048)]
    , [12, () => randomR (2048, 4096)]
    , [43, () => randomR (4096, 8192)]
    , [17, () => randomR (8192, 16384)]
    ]);
}

function someAmount() {
  return randomR(1, 100);
}

function randomR(min, max) {
  return Math.round(min + Math.random() * (max - min));
}

function pickOne(xs) {
  return xs[Math.floor(Math.random() * xs.length)];
}

function frequency(xs) {
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

function forEachLine(next) {
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

function csvWriter(filepath) {
  const file = fs.createWriteStream(filepath);

  return new Writable({
    write(chunk, _encoding, callback) {
      file.write(Buffer.concat([chunk, Buffer.from("\n")]))
      callback();
    }
  })
}

module.exports =
  { someSize
  , someAmount

  , pickOne
  , randomR
  , frequency

  , forEachLine
  , csvWriter
  };

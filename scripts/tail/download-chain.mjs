import WebSocket from 'ws';
import fs from 'fs';
import readline from 'readline';
import { Readable } from 'stream';

/**
 * Download chain from a local [Ogmios](https://ogmios.dev/) server.
 *
 * @param String url Websocket URL to connect to.
 * @param Array<Point> points Intersection points to start the synchronization from, see also 'point-of-interest.json'
 * @param Filepath filename Output stream filename to write data into.
 */
export async function * downloadChain(url, points, filepath) {
  const client = new WebSocket(url, ["ogmios.v1:compact"]);
  const writer = fs.createWriteStream(filepath);
  const reader = new Readable({ read() {} });

  client.ogmios = function wsp(methodname, args) {
    client.send(JSON.stringify({
      type: "jsonwsp/request",
      version: "1.0",
      servicename: "ogmios",
      methodname,
      args
    }));
  }

  const inFlight = 100;

  client.once('open', function() {
    client.ogmios("FindIntersect", { points });
    for(let n = 0; n < inFlight; n += 1) {
      client.ogmios("RequestNext");
    }
  });

  client.on('message', function(event) {
    const isOpen = client.readyState == client.OPEN;
    const forward = JSON.parse(event).result.RollForward;
    if (forward && isOpen) {
      const block = forward.block.mary || forward.block.allegra || forward.block.shelley || forward.block.byron;
      const chunk = `${JSON.stringify(block)}\n`;
      writer.write(`${chunk}`);
      reader.push(chunk);
      if (block.header.slot + inFlight >= forward.tip.slot) {
        client.close();
        writer.end();
        reader.end();
      } else {
        client.ogmios("RequestNext");
      }
    } else {
      client.ogmios("RequestNext");
    }
  });

  for await (const chunk of readline.createInterface({ input: reader })) {
    yield chunk;
  }
}

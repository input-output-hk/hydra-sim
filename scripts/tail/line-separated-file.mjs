import fs from 'fs';
import { Writable } from 'stream';

export function lineSeparatedFile(filepath) {
  const writer = fs.createWriteStream(filepath);

  writer.write('slot,clientId,event,size,amount,recipients\n');

  let buffer = "";
  return new Writable({
    write(chunk, encoding, callback) {
      const json = JSON.parse(chunk);
      buffer += `${json.map(csv).join('\n')}\n`;
      if (buffer.length > 1024*1024) {
        writer.write(buffer);
        buffer = "";
      }
      callback();
    },

    end(chunk, encoding, callback) {
      const json = JSON.parse(chunk);
      buffer += `${json.map(csv).join('\n')}\n`;
      writer.write(buffer);
      callback();
    }
  });
}

function csv({ slot, from, msg }) {
  if (msg === 'Pull') {
    const size = '';
    const amount = '';
    const recipients = '';
    return `${slot},${from},pull,${size},${amount},${recipients}`;
  } else {
    const size = msg.NewTx.size;
    const amount = msg.NewTx.amount;
    const recipients = msg.NewTx.recipients.join(' ');
    return `${slot},${from},new-tx,${size},${amount},${recipients}`;
  }
}

import fs from "fs";

function readPriceTable(filepath) {
  return fs.readFileSync(filepath)
    .toString()
    .split("\n")
    .map(s => s.split(","));
}

export function buildPriceTable(filepath) {
  const tab = [];
  let t = readPriceTable(filepath);
  let j = 0;
  for (let i = 0; i < t.length; i++) {
    while (j <  t[i][2]) { // price counts
     // console.log(" i=" + i + "; j=" + j + "; price: " + t[i][0] + ", mult: " + t[i][2]);
     tab[j] = t[i][0]; // price
     j++;
    }
  }

  const ptable = {
   table: tab,
   getPrice: function() {
    let index = Math.floor(Math.random() * ptable.table.length);

    return ptable.table[index];
   }
  }

  return ptable;
}

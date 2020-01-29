#!/bin/sh
mkdir -p csv pdf

export locations=('FrankfurtAWS FrankfurtAWS FrankfurtAWS' 'IrelandAWS FrankfurtAWS LondonAWS' 'OregonAWS FrankfurtAWS TokyoAWS')
export n=200

for loc in "${locations[@]}"; do
    for conc in 1 2 5 10 15 20 30 40 50; do
        cabal new-exec -- hydra-sim $loc \
              -b 5000 \
              -t Simple \
              -c $conc \
              -n $n \
              -v 1 \
              -o csv/conc-simple.csv
    done
done

for loc in "${locations[@]}"; do
    for conc in 1 2 5 10 15 20 30 40 50; do
        cabal new-exec -- hydra-sim $loc \
              -b 50000 \
              -t Plutus \
              -c $conc \
              -n $n \
              -v 1 \
              -o csv/conc-plutus.csv
    done
done

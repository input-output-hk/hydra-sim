#!/bin/sh
mkdir -p csv pdf

export locations=('FrankfurtAWS FrankfurtAWS FrankfurtAWS' 'IrelandAWS FrankfurtAWS LondonAWS' 'OregonAWS FrankfurtAWS TokyoAWS')
export n=1000

for loc in "${locations[@]}"; do
    echo $loc
done

for loc in "${locations[@]}"; do
    for conc in 1 2 10 20; do
        for rate in 50 100 200 300 350 400 500 600 700 800 900 1000 1500 2000 3000 5000 10000 20000; do
            cabal new-exec -- hydra-sim $loc \
                  -b $rate \
                  -t Simple \
                  -c $conc \
                  -n $n \
                  -v 1 \
                  --discard-edges 400 \
                  -o csv/bandwidth-simple.csv
        done
    done
done

for loc in "${locations[@]}"; do
    for conc in 1 2 10 20; do
        for rate in 500 1000 5000 6000 7000 8000 10000 13000 15000 20000 30000 50000 100000 1000000; do
            cabal new-exec -- hydra-sim $loc \
                  -b $rate \
                  -t Plutus \
                  -c $conc \
                  -n $n \
                  -v 1 \
                  --discard-edges 400 \
                  -o csv/bandwidth-plutus.csv
        done
    done
done



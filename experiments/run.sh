#!/bin/sh

mkdir -p csv pdf

### Simple transactions, (Cardano, 2 ins 2 outs)

## simple graph, confirmation time/throughput within a data centre.
## varying bandwidth. One transaction in flight permitted.

for rate in 50 100 200 300 500 700 1000 1500 2000 3000 5000 10000 20000; do
    cabal new-exec -- hydra-sim FrankfurtAWS FrankfurtAWS FrankfurtAWS \
          -b $rate \
          -t Simple \
          -c 1 \
          -n 50 \
          -v 1 \
          -o csv/basic.csv
done

## We get saturation at about 5 Mbits/s. Let's see what happens when we crank up the concurrency
for rate in 1000 5000 10000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim FrankfurtAWS FrankfurtAWS FrankfurtAWS \
              -b $rate \
              -t Simple \
              -c $conc \
              -n 50 \
              -v 1 \
              -o csv/conc.csv
    done
done

for rate in 1000 5000 10000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim IrelandAWS FrankfurtAWS LondonAWS \
              -b $rate \
              -t Simple \
              -c $conc \
              -n 50 \
              -v 1 \
              -o csv/conc.csv
    done
done

for rate in 1000 5000 10000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim OregonAWS FrankfurtAWS TokyoAWS \
              -b $rate \
              -t Simple \
              -c $conc \
              -n 50 \
              -v 1 \
              -o csv/conc.csv
    done
done

for rate in 1000 5000 10000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim OhioAWS OregonAWS FrankfurtAWS TokyoAWS SydneyAWS \
              -b $rate \
              -t Simple \
              -c $conc \
              -n 30 \
              -v 1 \
              -o csv/conc.csv
    done
done

### Plutus transactions, (Cardano, 2 ins 2 outs)

for rate in 50 100 200 300 500 700 1000 1500 2000 3000 5000 10000 20000 30000 70000 100000 300000 1000000; do
# # for rate in 30000 100000 1000000; do
# for rate in 70000 300000; do
    cabal new-exec -- hydra-sim FrankfurtAWS FrankfurtAWS FrankfurtAWS \
          -b $rate \
          -t Plutus \
          -c 1 \
          -n 50 \
          -v 1 \
          -o csv/basic.csv
done

# ## We get saturation at about 100 Mbits/s. Let's see what happens when we crank up the concurrency
for rate in 10000 100000 500000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim FrankfurtAWS FrankfurtAWS FrankfurtAWS \
              -b $rate \
              -t Plutus \
              -c $conc \
              -n 50 \
              -v 1 \
              -o csv/conc.csv
    done
done

for rate in 10000 100000 500000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim IrelandAWS FrankfurtAWS LondonAWS \
              -b $rate \
              -t Plutus \
              -c $conc \
              -n 50 \
              -v 1 \
              -o csv/conc.csv
    done
done

for rate in 10000 100000 500000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim OregonAWS FrankfurtAWS TokyoAWS \
              -b $rate \
              -t Plutus \
              -c $conc \
              -n 50 \
              -v 1 \
              -o csv/conc.csv
    done
done

for rate in 10000 100000 500000; do
    for conc in 1 2 3 4 5 6 7 8 9 10; do
        cabal new-exec -- hydra-sim OhioAWS OregonAWS FrankfurtAWS TokyoAWS SydneyAWS \
              -b $rate \
              -t Plutus \
              -c $conc \
              -n 30 \
              -v 1 \
              -o csv/conc.csv
    done
done

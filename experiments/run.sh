#!/bin/bash
mkdir -p csv pdf

export locations=('FrankfurtAWS FrankfurtAWS FrankfurtAWS' 'IrelandAWS FrankfurtAWS LondonAWS' 'OregonAWS FrankfurtAWS TokyoAWS')
# export n=1000
export n=200

for loc in "${locations[@]}"; do
    echo $loc
done

for loc in "${locations[@]}"; do
    for conc in 1 2 5 10 20; do
        cabal new-exec -- hydra-sim $loc \
              -b "[100, 200, 400, 800, 1600, 3200, 6400, 12800]" \
              -t Simple \
              -c $conc \
              -n $n \
              -v 1 \
              --discard-edges 90 \
              --baseline-snapshots "[NoSnapshots, SnapAfter 1, SnapAfter 2, SnapAfter 5, SnapAfter 10]" \
              -o csv/simple.csv
    done
done

for loc in "${locations[@]}"; do
    for conc in 1 2 5 10 20; do
        cabal new-exec -- hydra-sim $loc \
              -b "[4000, 8000, 16000, 32000, 64000]" \
              -t Plutus \
              -c $conc \
              -n $n \
              -v 1 \
              --discard-edges 90 \
              --baseline-snapshots "[NoSnapshots, SnapAfter 1, SnapAfter 2, SnapAfter 5, SnapAfter 10]" \
              -o csv/plutus.csv
    done
done

#!/bin/bash
# A simpler experiment with larger number of nodes
mkdir -p csv pdf

# List of locations of nodes
locations="$(for i in {1..100}; do echo -n 'FrankfurtAWS '; done)"

# Number of transactions per node
n=200

# Concurrency level of transactions processing
concurrency=(5)

for loc in "${locations[@]}"; do
    echo $loc
done

for loc in "${locations[@]}"; do
    for conc in "${concurrency[@]}"0; do
        cabal new-exec -- hydra-sim $loc \
              -b "[2000]" \
              -t Simple \
              -c $conc \
              -n $n \
              -v 1 \
              --discard-edges 90 \
              --baseline-snapshots "[SnapAfter 1]" \
              -o csv/simple.csv
    done
done

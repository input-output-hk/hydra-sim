#!/bin/sh
mkdir -p csv pdf

export locations=('FrankfurtAWS FrankfurtAWS FrankfurtAWS' 'IrelandAWS FrankfurtAWS LondonAWS' 'OregonAWS FrankfurtAWS TokyoAWS' 'NVirginiaAWS NCaliforniaAWS CanadaAWS LondonAWS FrankfurtAWS TokyoAWS SydneyAWS')
export n=1000
# export n=100

for loc in "${locations[@]}"; do
    echo $loc
done

for loc in "${locations[@]}"; do
    for conc in 1 2 10 20; do
            for rate in 180 1800 3600 4000 460 930 50 100 200 300 350 400 500 600 700 800 900 1000 1500 2000 3000 5000 10000 20000; do
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
        for rate in 1200 12000 16000 600 700 18000 500 1000 2000 3000 4000 5000 6000 7000 8000 10000 13000 15000 20000 30000 50000 100000 1000000; do
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


export locations2=('FrankfurtAWS FrankfurtAWS FrankfurtAWS' 'IrelandAWS FrankfurtAWS LondonAWS')
echo "observe how close we get:"
for loc in "${locations2[@]}"; do
    echo "--- locations $loc ---"
    cabal new-exec -- hydra-sim $loc \
          -b 7000 \
          -t Simple \
          -c 10 \
          -n 1000 \
          -v 1 \
          --discard-edges 400 \
          -o foo.csv
done



for conc in 1 2 10 20; do
    echo "--- conc $conc ---"
    cabal new-exec -- hydra-sim OregonAWS FrankfurtAWS TokyoAWS \
          -b 100000 \
          -t Plutus \
          -c $conc \
          -n 1000 \
          -v 1 \
          --discard-edges 400 \
          -o foo.csv
done

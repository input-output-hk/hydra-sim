# Hydra Tail Simulation Scripts

This folder contains a few scripts can be used to generate data-sets to inject in the simulation. It works as a pipeline of Node.js streams using real blockchain data obtained from the Mainnet. 
Why Node.js? Because JavaScript and JSON are quite convenient to rapidly prototype something and transform data on the fly. 

## How to use

```console
$ yarn install
$ yarn pipeline 1000 10
```

The first argument given to pipeline corresponds to the number of clients considered for generating events, whereas the second correspond to the compression rate of the chain (10 means that we only count 1 slot every 10 slots).

**NOTE (1):** If you haven't downloaded the chain locally, you'll need to install and setup an [Ogmios server](https://github.com/KtorZ/cardano-ogmios) to download blocks from the chain. The script assumes a local instance up-and-running with the default configuration. 

**NOTE (2):** The entire Cardano chain since the beginning of Shelley spreads across ~1.2M blocks. So, processing the whole pipeline takes time. The various intermediate representations are quite voluminous but the result of creating events spread across 1000 clients at a compression rate of 1000 can be found [here](https://raw.githubusercontent.com/input-output-hk/hydra-sim/master/scripts/tail/events-clients:1000-compression:1000.tar.gz). 

**NOTE (3):** The pipeline is single-cored, but multiple pipelines can be ran at once to help generating multiple datasets with different parameters. The output filenames are automatically generated from the script's arguments.

## Steps Overview

1. `downloadChain` (~1.2M blocks)
   
   Downloads the blockchain from a certain point (by default, from the first Shelley block and onwards). It'll download it both into a file and into a readable stream that is passed to the rest of the pipeline such that (a) The script runs in somewhat constant memory usage, (b) The pipeline produces data immediately. 

   The file produced is rather voluminous (4.5GB+) and will contain line-separated JSON blocks like this (_formatted over multiple-lines for readability_):

   ```json
   {
       "headerHash": "b51b1605cc27b0be3a1ab07dfcc2ceb0b0da5e8ab5d0cb944c16366edba92e83",
       "header": {
           "blockHeight": 4490515,
           "slot": 4492900,
           "prevHash": "23fd3b638e8f286978681567d52597b73f7567e18719cef2cbd66bba31303d98",
           "issuerVk": "5fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd441",
           "issuerVrf": "axwYeh90N9B55BQwtqn8eymybovJxGco5VE6kwTyIm8=",
           "blockSize": 1053,
           "blockHash": "f8ffe66aeeac127f30b8672857c4f6b8cb29c9ed24267104619a985105e22ba0"
       },
       "body": [
           {
               "id": "79acf08126546b68d0464417af9530473b8c56c63b2a937bf6451e96e55cb96a",
               "body": {
                   "inputs": [
                       {
                           "txId": "397eb970e7980e6ac1eb17fcb26a8df162db4e101f776138d74bbd09ad1a9dee",
                           "index": 0
                       },
                       ...
                   ],
                   "outputs": [
                       {
                           "address": "addr1qx2kd28nq8ac5prwg32hhvudlwggpgfp8utlyqxu6wqgz62f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sy0f4qd",
                           "value": 402999781127
                       },
                       ...
                   ],
                   "certificates": [],
                   "withdrawals": {},
                   "fee": 218873,
                   "timeToLive": 4500080,
                   "update": null
               },
               "metadata": {
                   "hash": null,
                   "body": null
               }
           }
       ]
   }
   ```
 
2. `viewViaStakeKeys` (~5M transactions & ~700K wallets)

   Extract transactions from each blocks and transform them so that inputs and outputs are directly associated with their corresponding stake keys. Indeed, since the beginning of the Shelley era,
   most wallets in Cardano use full delegation addresses containing both a payment part and a delegation part, but uses a single stake key per wallet. Thus, by looking at stake key hashes from 
   addresses it is possible to track down (Shelley) wallets with a quite good accuracy. This second step does exactly just that, while also trimming out informations that aren't useful for the simulation. This stream transformer produces chunks of line-separated JSON _"transactions"_ which look like the following (_formatted over multiple-lines for readability_):

   ```json
   {
       "ref": "79acf08126546b68d0464417af9530473b8c56c63b2a937bf6451e96e55cb96a",
       "size": 1443,
       "inputs": [
           null,
           null,
           null,
           null,
           null
       ],
       "outputs": [
           {
               "wallet": "f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9s",
               "value": 402999781127
           },
           {
               "wallet": "f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9s",
               "value": 39825492736
           },
           {
               "wallet": "f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9s",
               "value": 1999822602
           },
           {
               "wallet": "f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9s",
               "value": 1000000
           },
           {
               "wallet": "f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9s",
               "value": 100000000000
           }
       ],
       "slot": 4492900
   }
   ```

   Inputs or outputs marked as `null` correspond to either Byron addresses or Shelley addresses with no stake part whatsoever. 

3. `createEvents` (~ XXX client events)

  This steps consumes the stream of transactions created by `viewViaStakeKeys` and create Hydra Tail Simulation client events by assigning client ids to each stake key.
  The number of client ids is however limited and rotates. The effect creates a long stream of transactions but across a vastly smaller set of wallets / clients (the 
  main chain has about ~700.000 wallets identified by stake keys, and this pipeline step compress them down to ~1000). It also get rid of unknown inputs / outputs and
  keep transactions even simpler. 

  It generate line-separated JSON events as such (note that there's always a 'Pull' event added for every 'NewTx'):

  ```json
  {"slot":0,"from":986,"msg":"Pull"}
  {"slot":0,"from":986,"msg":{"NewTx":{"ref":"f746a18d6a17acf111109ff9a35a8c4bd130f73697188edd2d367cea5efe98a2","size":297,"recipients":[987],"amount":1002000000}}}
  {"slot":1,"from":732,"msg":"Pull"}
  {"slot":1,"from":732,"msg":{"NewTx":{"ref":"9e383d78de88fed8e222480f2f24766aa919038e3d238afb40d383e3e5069675","size":297,"recipients":[733],"amount":10000000}}}
  ```

4. `lineSeparatedFile` 

  This final steps format events as CSV and put one event per line in a rather compact format. Note that it also drop the transaction reference to save space and 
  because a unique identifier can be derived from a simple counter / line number of the corresponding event.

  The final format looks like this the following:

  ```csv
  slot  , clientId , event  , size , amount      , recipients
  63025 , 28       , pull   ,      ,             ,
  63025 , 28       , new-tx , 297  , 2000000     , 632
  63031 , 156      , pull   ,      ,             ,
  63031 , 156      , new-tx , 5212 , 1391209719  ,
  63031 , 157      , pull   ,      ,             ,
  63031 , 157      , new-tx , 232  , 148834411   , 158
  63034 , 942      , pull   ,      ,             ,
  63034 , 942      , new-tx , 320  , 23000000000 ,
  63037 , 772      , pull   ,      ,             ,
  63037 , 772      , new-tx , 287  , 5455000055  ,
  ```

  The last column `recipients` contains a space-separated list of recipients (or subscribers for a particular transaction). It may contains 0, 1 or many elements. 
  The `size`, `amount` and `recipients` columns are always empty for `pull` events. 

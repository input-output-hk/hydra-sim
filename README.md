# Simulations for Ouroboros Hydra

## Head Protocol

See [exe/head][./exe/head/Main.hs] and [experiments][./experiments]

## Tail Protocol

See [exe/tail][./exe/tail/Main.hs]. 

The tail protocol simulation works in two steps: preparation and run. 

#### Preparation

The preparation generates clients events from a set of parameters. That is, one can create a simulation over a certain period of time, from a certain number of clients with certain behavior. For example:

```console
$ hydra-tail-simulation prepare \
  --number-of-clients 1000 \
  --duration 60s \
  --slot-length 1s \
  --client-online-likelihood 50%100 \
  --client-submit-likelihood 10%100 
  events.csv
```

The events can then be fed into the simulation for execution. Having both step separated allows for creating events from other sources (e.g. from a real network) while the `prepare` command can be used to establish some baseline on simple patterns. Note that the `prepare` command is fully deterministic. Same options yield exactly the same events. 

#### Execution

To run a simulation, simply provide an events dataset with possibly some custom options for the server:

```console
$ hydra-tail-simulation run \
  --server-read-capacity 102400 \
  --server-write-capacity 102400 \
  events.csv
```

The simulation outputs two numbers: a maximum throughput and a real throughput. The real throughput is calculated by looking at the (simulated) time it took to run the simulation, compared to the max throughput which is the best that the server could achieve given the inputs.

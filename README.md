# simu

The main file is "e.ml" (ocaml) and contains various functions to "integrate" a Kandle or Uniswapv3 strategy
against a price series (we think of it as a stochastic integral)

## price series

1. generators of price series (from data, from Brownian motion (BM), from geometric Brownian motion (GBM))

2. filters of price series

vf = viscous filter

given a (time,price) series vf generates a new (time, price) series which is the "eta-viscous" filter of the input

eta is the viscosity (or fee)

the larger the viscosity eta, the more the output series lags behind the driver series

## Univ3 strategies

1. Square root variation of prices series

2. Capital allocation function (to determine the initial partitioning of wealth in quote/base)

## Kandel strategies

1. Kandel simulator with main function "sim"

```
let sim 
~tightness:width ~price_increment:gridstep ~cash:qB 
(* when we enter game and how long we play *)
(* NB: duration could be a stopping time in general, eg looking at a price-crossing event *)
~start:start ~duration:duration 
(* the future *)
~price_series:price_series 
= ...
```

sim takes as input:

1) strat parameters = price grid and cash
2) price series (delimited by start and duration)
outputs the return (the stochastic integral of strat against price) and number of up- and down-crossings

# simu


the main file is "e.ml" it contains various functions:

1. generators of price series (from data, from BM, from GBM)

2. viscous filters of price series

vf = viscous filter

given a (time,price) series vf generates a new (time, price) series which is the "eta-viscous" filter of the input

eta is the viscosity (or fee)

the higher eta, the more the output series lags behind the driver series [Q: is it functorial/idempotent?]

3. square root variation of prices series

4. Kandel simulator

5. Capital allocation function (to determine the initial partitioning of wealth in quote/base)

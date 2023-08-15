# simu


the main file is "e.ml" it contains various functions:

- generators of price series (from data, from BM, from GBM)

- viscous filters of price series

vf = viscous filter

given a (time,price) series vf generates a new (time, price) series which is the "eta-viscous" filter of the input

eta is the viscosity (or fee)

the higher eta, the more the output series lags behind the driver series



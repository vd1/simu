set terminal pngcairo enhanced
set output 'plot.png'

set title 'Returns'

set xlabel 'Index'
set ylabel 'Value'

plot "test_sim.csv" u 0:1 t "return"

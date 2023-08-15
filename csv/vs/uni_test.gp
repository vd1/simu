set terminal pngcairo size 3840,2160 enhanced font 'Axiforma,30' background rgb "#F1F7F6"
set output 'uni_test.png' 

set title  'gbm ival = 1, drift =0, vol = 0.1, dt = 0.001, duration = 1, etaTick = 0.01, rangeMultiplier = 1.4  (polynomial fit)' textcolor rgb "#F1F7F6"

f(x) = a*x**2 + b*x + c 
fit f(x) "unimr0.1.csv" using 1:2 via a, b, c

plot f(x) w l linewidth 5, "unimr0.1.csv" w p pointtype 7 pointsize 2
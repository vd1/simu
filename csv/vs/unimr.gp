set terminal pngcairo size 1920,1080 enhanced font 'Verdana,12'
# Setting the output filename to be the same as the input filename with the .png extension appended to it.
set output 'unimr3.png'

#We set the file separator to be the comma, this way we inform the engine that we will be processing a CSV file
set datafile separator ","

#Informing the engine that the X axis of our  will be date/time data type
#set xdata time
#We define how the date/time input must be parsed. In this example we expect the input to be like '2016-01-19 14:25:00'
#set timefmt '%Y-%m-%d %H:%M:%S'

#We set the output format that will be shown on the X axis. Here we expect to show '19-01 New Line 14:25"
#set format x "%d-%m\n%H:%M"
#Set the X axis label
set xlabel "fee"
#Set the Y axis label
set ylabel "mean return" 

set grid

set title \
'returns against GBM with volatility = 5.5%% as a function of fee [0.001,0.2]\
(cubic fit)'

set label 1 'number of realisations:100' at screen 0.1,0.900 
set label 2 'feeTick:0.001 feeMax:0.2 rangeMultiplier:1.4' at screen 0.1,0.88
set label 3 'initial value:1. drift:0. volatility:vol timestep:0.001 duration:1.' at screen 0.1,0.86

set xrange [0:0.2]
set yrange [0:0.015]

# fit f(x) 'meanret0.005.csv' using 1:2 via a, b, c
# plot f(x)

array A[100]
array B[100]
array C[100]
array D[100]

# quadratic
# f(x) = a*x**2 + b*x + c
# fit f(x) 'unimr0.055.csv' using 1:2 via a, b, c

# cubic
f(x) = a*x**3 + b*x**2 + c*x + d
# fit f(x) 'unimr0.055.csv' using 1:2 via a, b, c, d

do for [i=55:95:10] {
    # fit f(x) sprintf('unimr0.0%d.csv', i) using 1:2 via a, b, c
    fit f(x) sprintf('unimr0.0%d.csv', i) using 1:2 via a, b, c, d
    # Store the parameters
    A[i] = a
    B[i] = b
    C[i] = c
    D[i] = d
}

plot A[55]*x**3 + B[55]*x**2 + C[55]*x + D[55] w l linewidth 10 linecolor rgb "#021B1A" t 'daily volatility 5.5%',\
A[65]*x**3 + B[65]*x**2 + C[65]*x + D[65] w l linewidth 10 linecolor rgb "#021B1A" t 'daily volatility 6.5%',\
A[75]*x**3 + B[75]*x**2 + C[75]*x + D[75] w l linewidth 10 linecolor rgb "#021B1A" t 'daily volatility 7.5%',\
A[85]*x**3 + B[85]*x**2 + C[85]*x + D[85] w l linewidth 10 linecolor rgb "#021B1A" t 'daily volatility 8.5%',\
A[95]*x**3 + B[95]*x**2 + C[95]*x + D[95] w l linewidth 10 linecolor rgb "#021B1A" t 'daily volatility 9.5%',\
'unimr0.055.csv' using 1:2 with points pointtype 7 pointsize 1,\
'unimr0.065.csv' using 1:2 with points pointtype 7 pointsize 1,\
'unimr0.075.csv' using 1:2 with points pointtype 7 pointsize 1,\
'unimr0.085.csv' using 1:2 with points pointtype 7 pointsize 1,\
'unimr0.095.csv' using 1:2 with points pointtype 7 pointsize 1

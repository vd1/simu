# set terminal pdfcairo size 272,135 enhanced font 'Axiforma,120'
# set terminal pngcairo transparent size 3840,2160 enhanced font 'Axiforma,24' background rgb "#03624C"
set terminal pngcairo  size 3840,2160 enhanced font 'Axiforma,30' background rgb "#F1F7F6"
set key textcolor rgb "#021B1A"

# set terminal svg size 600,400 enhanced font 'Axiforma,24'

# Caribbean Green
# HEX #00DF81
# RGB 0 223 129

# Anti-Flash White
# HEX #F1F7F6
# RGB 241 247 246

# Bangladesh Green
# HEX #03624C
# RGB 3 98 76

# Dark Green
# HEX #032221
# RGB 3 34 33

# Rich Black
# HEX #021B1A
# RGB 2 27 26

# Setting the output filename to be the same as the input filename with the .png extension appended to it.
set output 'uni_Kandel_9.5.png'

# unset grid
set grid

#We set the file separator to be the comma, this way we inform the engine that we will be processing a CSV file
set datafile separator ","

#Set the X axis label
set xlabel "spread/fee" textcolor rgb "#021B1A"
#Set the Y axis label
set ylabel "mean daily return" textcolor rgb "#021B1A"

# set grid
set border linewidth 1.5 linecolor rgb "#021B1A"
set tics textcolor rgb "#021B1A"
set xtics 0.01

set title \
'(x axis) spread/fee \
 (y axis) mean return of a 1 day Kandel/Uniswapv3 \
 shown for daily volatility 9.5% (polynomial fit)' textcolor rgb "#021B1A"

set label 1 'number of realisations:1000' at screen 0.1,0.900 textcolor rgb "#021B1A"
set label 2 'Tick:0.001 spread/feeMax:0.2 rangeMultiplier:1.4' at screen 0.1,0.88 textcolor rgb "#021B1A"
set label 3 'initial value:1. drift:0. volatility:vol timestep:0.001 duration:1.' at screen 0.1,0.86 textcolor rgb "#021B1A"

set xrange [0:0.2]
set yrange [0:0.015]

# Define the function
# fk(x) =  a*x**2 + b*x + c
# fu(x) =  d*x**3 + e*x**2 + f*x + g

# # Loop over the data files
# fit fk(x) 'meanret0.095.csv' using 1:2 via a, b, c

# # a = -0.499272
# # b = 1.11097  
# # c = -0.606859

# fit fu(x) '../vs/unimr0.095.csv' using 1:2 via d, e, f, g

# d               = 2.25815    
# e               = -0.849162  
# f               = 0.0604226  
# g               = 0.00548158 

plot -0.499272*(1+x)**2 + 1.11097*(1+x) -0.606859 w l linewidth 10 linecolor rgb "#00DF81" t '(Kandel) daily volatility 9.5%',\
2.25815*x**3 - 0.849162*x**2 + 0.0604226  *x + 0.00548158 w l linewidth 10 linecolor rgb "#A329C5" t '(Uniswapv3) daily volatility 9.5%'

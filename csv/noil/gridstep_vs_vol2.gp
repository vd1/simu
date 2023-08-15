# set terminal pdfcairo size 272,135 enhanced font 'Axiforma,120'
set terminal pngcairo transparent size 3840,2160 enhanced font 'Axiforma,30' background rgb "#03624C"
set key textcolor rgb "#F1F7F6"

# set terminal svg size 600,400 enhanced font 'Axiforma,24'

# Background: #03624C (green)
# Legend: #F1F7F6 (white snow)
# Color 1: #F1F7F6 (white snow)
# Color 2: #00DF81 (light green)
# Color 3: #A329C5 (pink)
# Color 4: #3F2674 (purple)
# Color 5: #032221 (dark green)

# Setting the output filename to be the same as the input filename with the .png extension appended to it.
set output 'gridstep_vs_volb.png'

# unset grid
set grid

#We set the file separator to be the comma, this way we inform the engine that we will be processing a CSV file
set datafile separator ","

#Informing the engine that the X axis of our  will be date/time data type
#set xdata time
#We define how the date/time input must be parsed. In this example we expect the input to be like '2016-01-19 14:25:00'
#set timefmt '%Y-%m-%d %H:%M:%S'

#We set the output format that will be shown on the X axis. Here we expect to show '19-01 New Line 14:25"
#set format x "%d-%m\n%H:%M"
#Set the X axis label
set xlabel "spread" textcolor rgb "#F1F7F6"
#Set the Y axis label
set ylabel "mean daily return" textcolor rgb "#F1F7F6"

# set grid
set border linewidth 1.5 linecolor rgb "#F1F7F6"
set tics textcolor rgb "#F1F7F6"
set xtics 0.01



set title \
'(x axis) spread \
 (y axis) mean return of a 1 day Kandel\
 shown for various levels of daily volatilty (polynomial fit)' textcolor rgb "#F1F7F6"

set label 1 'number of realisations:1000' at screen 0.1,0.900 textcolor rgb "#F1F7F6"
set label 2 'spreadTick:0.001 spreadMax:0.2 rangeMultiplier:1.4' at screen 0.1,0.88 textcolor rgb "#F1F7F6"
set label 3 'initial value:1. drift:0. volatility:vol timestep:0.001 duration:1.' at screen 0.1,0.86 textcolor rgb "#F1F7F6"

set xrange [1:1.2]
set yrange [0:0.015]

# 'meanret0.005.csv' w l,\
# 'meanret0.015.csv' w l,\
# 'meanret0.025.csv' w l,\
# 'meanret0.035.csv' w l,\
# 'meanret0.045.csv' w l,\
# 'meanret0.055.csv' w l,\
# 'meanret0.065.csv' w l,\
# 'meanret0.075.csv' w l,\
# 'meanret0.085.csv' w l,\
# 'meanret0.095.csv' w l


# # plot \
# # 'meanret0.005.csv' w l,\
# # 'meanret0.015.csv' w l,\
# # 'meanret0.025.csv' w l,\
# # 'meanret0.035.csv' w l,\
# # 'meanret0.045.csv' w l,\
# # 'meanret0.055.csv' w l,\
# # 'meanret0.065.csv' w l,\
# # 'meanret0.075.csv' w l,\
# # 'meanret0.085.csv' w l,\
# # 'meanret0.095.csv' w l
 

# Define the function
f(x) = a*x**2 + b*x + c

# Array to hold the parameters for each fit
array A[100]
array B[100]
array C[100]


# Loop over the data files
do for [i=55:95:10] {
    # Fit the function to the data file
    fit f(x) sprintf('meanret0.0%d.csv', i) using 1:2 via a, b, c

    # Store the parameters
    A[i] = a
    B[i] = b
    C[i] = c
}

# Plot the fitted functions
# plot for [i=15:95:10] sprintf('meanret0.0%d.csv', i) using 1:2 title sprintf('meanret0.0%d', i), \
#      f(x) = A[i]*x**2 + B[i]*x + C[i] title sprintf('Return with daily volatility 0.0%d', i)
# plot for [i=55:95:10] A[i]*x**2 + B[i]*x + C[i] w l linewidth 10 linecolor rgb "#032221" t sprintf('daily volatility %d%', i/10)
plot A[95]*x**2 + B[95]*x + C[95] w l linewidth 10 linecolor rgb "#F1F7F6" t 'daily volatility 9.5%',\
A[85]*x**2 + B[85]*x + C[85] w l linewidth 10 linecolor rgb "#00DF81" t 'daily volatility 8.5%',\
A[75]*x**2 + B[75]*x + C[75] w l linewidth 10 linecolor rgb "#A329C5" t 'daily volatility 7.5%',\
A[65]*x**2 + B[65]*x + C[65] w l linewidth 10 linecolor rgb "#3F2674" t 'daily volatility 6.5%',\
A[55]*x**2 + B[55]*x + C[55] w l linewidth 10 linecolor rgb "#032221" t 'daily volatility 5.5%'
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

# set output
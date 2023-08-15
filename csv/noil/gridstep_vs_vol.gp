set terminal pngcairo size 1920,1080 enhanced font 'Verdana,12'
# Setting the output filename to be the same as the input filename with the .png extension appended to it.
set output 'gridstep_vs_volb.png'

#We set the file separator to be the comma, this way we inform the engine that we will be processing a CSV file
set datafile separator ","

#Informing the engine that the X axis of our  will be date/time data type
#set xdata time
#We define how the date/time input must be parsed. In this example we expect the input to be like '2016-01-19 14:25:00'
#set timefmt '%Y-%m-%d %H:%M:%S'

#We set the output format that will be shown on the X axis. Here we expect to show '19-01 New Line 14:25"
#set format x "%d-%m\n%H:%M"
#Set the X axis label
set xlabel "gridstep"
#Set the Y axis label
set ylabel "meanreturn" 

set grid

set title \
'(y axis) mean return of a 1 day Kandle as a function of gridstep (x axis) shown for various levels of daily volatilty'

set label 1 'number of realisations:1000' at screen 0.1,0.900 
set label 2 'gridstepTick:0.001 gridstepMax:0.2 rangeMultiplier:1.4' at screen 0.1,0.88
set label 3 'initial value:1. drift:0. volatility:vol timestep:0.001 duration:1.' at screen 0.1,0.86

set xrange [1:1.2]
set yrange [0:0.015]

f(x) = a*x**2 + b*x + c
g(x) = f*x**2 + e*x + d

# fit f(x) 'meanret0.005.csv' using 1:2 via a, b, c
# plot f(x)

fit f(x) 'meanret0.095.csv' using 1:2 via a, b, c 
plot f(x) t 'meanret0.095.csv'

fit g(x) 'meanret0.085.csv' using 1:2 via d, e, f 
replot g(x) t 'meanret0.085.csv'

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
 

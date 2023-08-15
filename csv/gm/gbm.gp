set terminal pngcairo size 1920,1080 enhanced font 'Verdana,12'
# Setting the output filename to be the same as the input filename with the .png extension appended to it.
set output 'gbm2.png'

#We set the file separator to be the comma, this way we inform the engine that we will be processing a CSV file
set datafile separator ","

#Informing the engine that the X axis of our  will be date/time data type
#set xdata time
#We define how the date/time input must be parsed. In this example we expect the input to be like '2016-01-19 14:25:00'
#set timefmt '%Y-%m-%d %H:%M:%S'

#We set the output format that will be shown on the X axis. Here we expect to show '19-01 New Line 14:25"
#set format x "%d-%m\n%H:%M"
#Set the X axis label
set xlabel "time"
#Set the Y axis label
set ylabel "price" 

set grid

set title \
'gbm with daily 0.1'

# set label 1 'number of rays:1000' at screen 0.1,0.900 
# set label 2 'pictick:0.001 maxpic:0.2 tightness:1.4' at screen 0.1,0.88
# set label 3 'initial value:1. drift:0. volatility:vol timestep:0.001 duration:1.' at screen 0.1,0.86


# set xrange [1:1.2]
# set yrange [0:0.015]

plot \
'simu/csv/gm/gbm20.01_10.csv' w l,\
'simu/csv/gm/gbm20.01_9.csv' w l,\
'simu/csv/gm/gbm20.01_8.csv' w l,\
'simu/csv/gm/gbm20.01_7.csv' w l,\
'simu/csv/gm/gbm20.01_6.csv' w l,\
'simu/csv/gm/gbm20.01_5.csv' w l,\
'simu/csv/gm/gbm20.01_4.csv' w l,\
'simu/csv/gm/gbm20.01_3.csv' w l,\
'simu/csv/gm/gbm20.01_2.csv' w l,\
'simu/csv/gm/gbm20.01_1.csv' w l
 

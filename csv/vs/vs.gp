set terminal pngcairo size 1920,1080 enhanced font 'Verdana,12'
# Setting the output filename to be the same as the input filename with the .png extension appended to it.
set output 'viscous.png'

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
'viscous price filters with BM input with volatility =10%'

# # "vs3.csv" with lines, \
# "vs5.csv" with lines \
# "vs1.csv" with lines, \
# "vs4.csv" with lines, \
# "vs6.csv" with lines, \
# "vs7.csv" with lines, \
# "vs8.csv" with lines, \
# "vs9.csv" with lines, \
# "vs10.csv" with lines

# with points pointtype 7 pointsize 1
plot\
"vs0.csv" with points pointtype 7 pointsize 1 t "driver",\
"vs1.csv" with points pointtype 7 pointsize 1 t "lagger 0.2",\
"vs1.csv" with points pointtype 7 pointsize 1 t "lagger 0.1",\
"vs2.csv" with points pointtype 7 pointsize 1 t "lagger 0.05",\
"vs3.csv" with points pointtype 7 pointsize 1 t "lagger 0.01"
# "vs4.csv" with points pointtype 7 pointsize 1 t "lagger 0.005"


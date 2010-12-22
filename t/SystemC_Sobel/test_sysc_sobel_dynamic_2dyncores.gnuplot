set terminal svg size 800 560 enhanced fname "Luxi Sans" fsize 11
set output "test_sysc_sobel_dynamic_2dyncores.svg.tmp"

set nologscale xy


set title "Gannet DRI SystemC Model: Service Core Status" font "Luxi Sans,14"
set xlabel "Elapsed time (us)" font "Luxi Sans,12"
unset ytics
set ylabel "Service Core Status" font "Luxi Sans,12" offset -8,0
  
set label 2 "LET" at -1,11 right
set label 6 "IMG" at -1,22 right
set label 7 "CONFIG" at -1,34 right
set label 11 "PROCIMG" at -1,45 right
set label 14 "S1" at -1,57 right
set label 15 "S2" at -1,68 right
plot [0:400] [2:80] 'core_status_1.out' using ($1/1000):($2+11.4285714285714) title "" with lines lt -1,\
'core_status_5.out' using ($1/1000):($2+22.8571428571429) title "" with lines lt -1,\
'core_status_6.out' using ($1/1000):($2+34.2857142857143) title "" with lines lt -1,\
'core_status_10.out' using ($1/1000):($2+45.7142857142857) title "" with lines lt -1,\
'core_status_13.out' using ($1/1000):($2+57.1428571428571) title "" with lines lt -1,\
'core_status_14.out' using ($1/1000):($2+68.5714285714286) title "" with lines lt -1

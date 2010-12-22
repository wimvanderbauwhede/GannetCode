set terminal svg size 800 560 enhanced fname "Luxi Sans" fsize 11
set output "test_sysc_sobel_static_buf_seq.svg.tmp"

set nologscale xy


set title "Gannet DRI SystemC Model: Service Core Status" font "Luxi Sans,14"
set xlabel "Elapsed time (us)" font "Luxi Sans,12"
unset ytics
set ylabel "Service Core Status" font "Luxi Sans,12" offset -8,0
  
set label 2 "LET" at -1,5 right
set label 3 "MIR2" at -1,11 right
set label 5 "SOBV1" at -1,17 right
set label 6 "IMG" at -1,22 right
set label 7 "MADD1" at -1,28 right
set label 8 "MADD2" at -1,34 right
set label 9 "MADD3" at -1,40 right
set label 10 "SOBH1" at -1,45 right
set label 11 "PROCIMG" at -1,51 right
set label 12 "R2Y" at -1,57 right
set label 13 "Y2R" at -1,62 right
set label 14 "SOBH2" at -1,68 right
set label 15 "SOBV2" at -1,74 right
plot [0:350] [2:80] 'core_status_1.out' using ($1/1000):($2+5.71428571428571) title "" with lines lt -1,\
'core_status_2.out' using ($1/1000):($2+11.4285714285714) title "" with lines lt -1,\
'core_status_4.out' using ($1/1000):($2+17.1428571428571) title "" with lines lt -1,\
'core_status_5.out' using ($1/1000):($2+22.8571428571429) title "" with lines lt -1,\
'core_status_6.out' using ($1/1000):($2+28.5714285714286) title "" with lines lt -1,\
'core_status_7.out' using ($1/1000):($2+34.2857142857143) title "" with lines lt -1,\
'core_status_8.out' using ($1/1000):($2+40) title "" with lines lt -1,\
'core_status_9.out' using ($1/1000):($2+45.7142857142857) title "" with lines lt -1,\
'core_status_10.out' using ($1/1000):($2+51.4285714285714) title "" with lines lt -1,\
'core_status_11.out' using ($1/1000):($2+57.1428571428571) title "" with lines lt -1,\
'core_status_12.out' using ($1/1000):($2+62.8571428571429) title "" with lines lt -1,\
'core_status_13.out' using ($1/1000):($2+68.5714285714286) title "" with lines lt -1,\
'core_status_14.out' using ($1/1000):($2+74.2857142857143) title "" with lines lt -1

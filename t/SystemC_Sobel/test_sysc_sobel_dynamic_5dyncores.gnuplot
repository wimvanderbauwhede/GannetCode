set terminal svg size 800 560 enhanced fname "Luxi Sans" fsize 11
set output "test_sysc_sobel_dynamic_5dyncores.svg.tmp"

set nologscale xy


set title "Gannet DRI SystemC Model: Service Core Status" font "Luxi Sans,14"
set xlabel "Elapsed time (us)" font "Luxi Sans,12"
unset ytics
set ylabel "Service Core Status" font "Luxi Sans,12" offset -8,0
  
set label 2 "LET" at -1,8 right
set label 6 "IMG" at -1,16 right
set label 7 "CONFIG" at -1,24 right
set label 9 "S5" at -1,32 right
set label 11 "PROCIMG" at -1,40 right
set label 12 "S4" at -1,48 right
set label 13 "S3" at -1,56 right
set label 14 "S1" at -1,64 right
set label 15 "S2" at -1,72 right
plot [0:300] [2:80] 'core_status_1.out' using ($1/1000):($2+8) title "" with lines lt -1,\
'core_status_5.out' using ($1/1000):($2+16) title "" with lines lt -1,\
'core_status_6.out' using ($1/1000):($2+24) title "" with lines lt -1,\
'core_status_8.out' using ($1/1000):($2+32) title "" with lines lt -1,\
'core_status_10.out' using ($1/1000):($2+40) title "" with lines lt -1,\
'core_status_11.out' using ($1/1000):($2+48) title "" with lines lt -1,\
'core_status_12.out' using ($1/1000):($2+56) title "" with lines lt -1,\
'core_status_13.out' using ($1/1000):($2+64) title "" with lines lt -1,\
'core_status_14.out' using ($1/1000):($2+72) title "" with lines lt -1

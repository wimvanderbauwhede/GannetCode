set terminal svg size 800 560 enhanced fname "Luxi Sans" fsize 11
set output "test_sysc_sobel_dynamic_1dyncore.svg.tmp"

set nologscale xy


set title "Gannet DRI SystemC Model: Service Core Status" font "Luxi Sans,14"
set xlabel "Elapsed time (us)" font "Luxi Sans,12"
unset ytics
set ylabel "Service Core Status" font "Luxi Sans,12" offset -8,0
  
set label 2 "LET" at -1,13 right
set label 6 "IMG" at -1,26 right
set label 7 "CONFIG" at -1,40 right
set label 11 "PROCIMG" at -1,53 right
set label 14 "S1" at -1,66 right
plot [0:500] [2:80] 'core_status_1.out' using ($1/1000):($2+13.3333333333333) title "" with lines lt -1,\
'core_status_5.out' using ($1/1000):($2+26.6666666666667) title "" with lines lt -1,\
'core_status_6.out' using ($1/1000):($2+40) title "" with lines lt -1,\
'core_status_10.out' using ($1/1000):($2+53.3333333333333) title "" with lines lt -1,\
'core_status_13.out' using ($1/1000):($2+66.6666666666667) title "" with lines lt -1

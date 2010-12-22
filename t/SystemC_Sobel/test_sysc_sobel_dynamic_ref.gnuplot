set terminal svg size 800 560 enhanced fname "Luxi Sans" fsize 11
set output "test_sysc_sobel_dynamic_ref.svg.tmp"

set nologscale xy


set title "Gannet DRI SystemC Model: Service Core Status" font "Luxi Sans,14"
set xlabel "Elapsed time (us)" font "Luxi Sans,12"
unset ytics
set ylabel "Service Core Status" font "Luxi Sans,12" offset -8,0
  
set label 1 "CONFIG" at -1,5 right
set label 2 "LET" at -1,10 right
set label 3 "MIR2" at -1,15 right
set label 4 "MIR1" at -1,20 right
set label 5 "SOBV1" at -1,25 right
set label 6 "IMG" at -1,30 right
set label 7 "MADD1" at -1,35 right
set label 8 "MADD2" at -1,40 right
set label 9 "MADD3" at -1,45 right
set label 10 "SOBH1" at -1,50 right
set label 11 "PROCIMG" at -1,55 right
set label 12 "R2Y" at -1,60 right
set label 13 "Y2R" at -1,65 right
set label 14 "SOBH2" at -1,70 right
set label 15 "SOBV2" at -1,75 right
plot [0:250] [2:80] 'core_status_0.out' using ($1/1000):($2+5) title "" with lines lt -1,\
'core_status_1.out' using ($1/1000):($2+10) title "" with lines lt -1,\
'core_status_2.out' using ($1/1000):($2+15) title "" with lines lt -1,\
'core_status_3.out' using ($1/1000):($2+20) title "" with lines lt -1,\
'core_status_4.out' using ($1/1000):($2+25) title "" with lines lt -1,\
'core_status_5.out' using ($1/1000):($2+30) title "" with lines lt -1,\
'core_status_6.out' using ($1/1000):($2+35) title "" with lines lt -1,\
'core_status_7.out' using ($1/1000):($2+40) title "" with lines lt -1,\
'core_status_8.out' using ($1/1000):($2+45) title "" with lines lt -1,\
'core_status_9.out' using ($1/1000):($2+50) title "" with lines lt -1,\
'core_status_10.out' using ($1/1000):($2+55) title "" with lines lt -1,\
'core_status_11.out' using ($1/1000):($2+60) title "" with lines lt -1,\
'core_status_12.out' using ($1/1000):($2+65) title "" with lines lt -1,\
'core_status_13.out' using ($1/1000):($2+70) title "" with lines lt -1,\
'core_status_14.out' using ($1/1000):($2+75) title "" with lines lt -1

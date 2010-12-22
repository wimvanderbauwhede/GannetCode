#!/usr/bin/perl 
use warnings;
use strict;
# Generate Quarc NoC connections for Gannet SystemC model

die "Please provide the number of nodes in the Quarc NoC (must be a multiple of 4).\n" if not @ARGV;
my $N=$ARGV[0];

for my $i (0..$N-1) {
    my $cim1=($i-1<0)?$N-1:$i-1;
    my $cip1=($i==$N-1)?0:$i+1;
    my $xi=($i<$N/2)?$N/2+$i:$i-$N/2; 

    my $inst_i = instsel($i);
    my $inst_q0=instsel($cip1);
    my $inst_q1= instsel($xi);
    my $inst_q2= instsel($xi);
    my $inst_q3= instsel($cim1);
    print "
        $inst_i.network_rx_fifo0 .bind ($inst_q0.xpwr_rxfifo0);
    $inst_i.network_rx_fifo1 .bind ($inst_q1.xpwr_rxfifo1);
    $inst_i.network_rx_fifo2 .bind ($inst_q2.xpwr_rxfifo2);
    $inst_i.network_rx_fifo3 .bind ($inst_q3.xpwr_rxfifo3);
    ";
}
# Gateway instance is special
sub instsel { my $i=shift;
	if ($i==$N-1) { 
		return 'gwinstance';
	} else {
		return "(*instances[ $i ])";
	}
}

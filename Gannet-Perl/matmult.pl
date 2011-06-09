#!/usr/bin/perl 
use warnings;
use strict;
my $w=64;
my $aref = [];
my $href={};
my @a = ();
my @b= (1,2,3);
#my @m=();
$a[$i][$j];# =  rand();

foreach my $i (0 .. $w-1) {
    foreach my $j (0 .. $w-1) {
        $w=$i+$j;
#       $a[$i][$j] = rand();
#       $b[$i][$j] = rand();
    }
}

foreach my $i (0 .. $w-1) {
    foreach my $j (0 .. $w-1) {
#        $m=[$i][$j]=0;
        foreach my $k (0 .. $w-1) {
            $w++;
#           $m[$i][$j]+=$a[$i][$k]*$b[$k][$j];
        }
    }
}


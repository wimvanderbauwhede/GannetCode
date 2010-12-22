#!/usr/bin/perl
use warnings;
use strict;

my $rgbfile = $ARGV[0];
my $os=`uname -s`; chomp $os;
my $aqua='';
my $pause='pause -1';
if ($os ne 'Linux') {
    $aqua='set terminal aqua';
    $pause='';
}

my $gnuplotcmds=<<"ENDC";
$aqua
set nokey
set noxtics
set noytics
set noborder
set size square 1,1

plot '$rgbfile' with rgbimage
$pause

ENDC

open my $OUT,'>','plot_rgbimage';
print $OUT $gnuplotcmds;
close $OUT;
system("gnuplot plot_rgbimage");

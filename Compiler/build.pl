#!/usr/bin/perl
use warnings;
use strict;
use Getopt::Std;
use Cwd;
my $wd=cwd();
my %opts=();
if (!@ARGV) {$opts{'h'}=1}
getopts( 'hvrNbcCW:', \%opts );

if ( $opts{'h'} ) {
die "$0 options:
    -c: configure
    -b: build
    -C: clean, then configure
    -W [32|64]: word size (default 64-bit) 
    -N: NEW, for testing new features
";
}
my $v=$opts{'v'}?'--verbose=2':'';
my $build=($opts{'b'} || $opts{'c'} || $opts{'C'})  ?1:0;
my $c=$opts{'c'}?1:0;
my $C=$opts{'C'}?1:0;
$c||=$C;

#if ($opts{'W'} ) {
#    $c=1;$C=1;
#}

my $NEWflag=$opts{'N'}?'-fNEW':'';
my $W=64;
if ($opts{'W'}) {
    $W=$opts{'W'}*1;
}
my $wflag = '-fW'.$W; # ($W!=64)?'-fW'.$W:'';

if (-e "./dist/build/gannetc/gannetc"){
    unlink "./dist/build/gannetc/gannetc";
}

if ($C) {
    system("cabal clean");       
}	
if ($c) {
    print "* Cabal configure, $W-bit, flags: $NEWflag\n";
    system("cabal configure $wflag $NEWflag");
}
if ($build) {    
    print "* Cabal build\n";
    system("cabal build $v");
    print "* Cabal install\n";
    system("cabal install --bindir=../bin");
    system("cp ../bin/gannetc ../bin/gannetc$W");
}

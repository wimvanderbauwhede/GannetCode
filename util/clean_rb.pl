#!/usr/bin/perl 
use warnings;
use strict;
open my $IN,'<',$ARGV[0];
my $skip=0; my $cskip=0;my $skipl=0;
while (my $line=<$IN>) {

#if ($line=~/=begin/) {
#    $cskip=1;
#}
#if ($line=~/=end/) {
#    $cskip=0;
#}
if ($line=~/^\s*\#iv\s*$/) {
    $skip=1;
}
if ($line =~/^\s*\#ev\s*$/) { $skip=0;
}

if ($line=~/^\s*[^\s]+.*\s+if\s+\@v\s+/) {
    $skipl=1;
}
$line=~s/\s*#\s+.*$//;
$line=~s/^\s*#\s*$//;
$line=~s/#\w+.*$//;
#$line=~s/^=.*$//;
if ($skipl==0 and $skip==0 and $cskip==0 and $line!~/^\s*$/) {
print $line;
}
$skipl=0;
}
close $IN;

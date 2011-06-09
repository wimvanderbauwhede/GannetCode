#!/usr/bin/perl 
use warnings; 
use strict;

if (!@ARGV) {
    die "Please specify the input image file (color, jpeg or png).\n";
}
my $in=$ARGV[0];
$xpm=$in;
$xpm=~s/\..*$/.xpm/;
my $convert=`which convert`;
chomp $convert;
if ($convert ne '') {
    system("convert -colorspace Gray $in $xpm");
} else {
    die "Can't find convert, please make sure you have ImageMagick and the path is set correctly.\n";
}

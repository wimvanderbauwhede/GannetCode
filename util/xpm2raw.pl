#!/usr/bin/perl 
use warnings;
use strict;
@ARGV or die "syntax: $0 xpm-name\n";
my $xpmname=$ARGV[0];
$xpmname=~s/\.xpm//;
unlink "$xpmname.raw";
my @xpm=();


my $pixmap=$xpmname;
$pixmap=~s/\-/_/g;
open(XPM,"<$xpmname.xpm") or die "$!";
while(<XPM>) {
	/^\s*$/ && next;
	/static/ && next;
	/^\s*\/\*/ && next;
	chomp;
    s/None/#FFFFFF/;
	s/^\s*\"//;
	s/\"\s*\,*\s*$//;
	s/\"\s*}\s*;\s*$//;
	push @xpm,$_;
}
close XPM;

my @xpmname=@xpm;

my($ncols,$nrows,$ncolors,$nchars)=split(' ',$xpm[0]);
open RAW,">$xpmname.raw";
print RAW sprintf("0x%x",$nrows),"\n";
print RAW sprintf("0x%x",$ncols),"\n";
# the "raw" format is simply a list of 32-bit integers
# Word 1: nrows
# Word 2: ncols
# Word 3..nrows*ncols+2: 0x00RRGGBB


my %colortable=();
foreach my $i (1..$ncolors){
	my ($charcode,$color)=split(/[\s\t]c\ \#/,$xpmname[$i]);
#	print "$i: <$charcode>:\t$color\n";
#	if($color!~/\#/){
#		next;
#	}
	$color='0x00'.$color;
#	$color.='UL';
	$charcode=~s/\s+$//;
	$colortable{$charcode}=$color;
}
#my $rr=0;
foreach my $i (0..$nrows-1) { # loop over all pixel rows  
	my $ci=$i+$ncolors+1;
	my $pixstring=$xpmname[$ci];
	my $nunits=$nchars;
	my $templ="a$nunits "x$ncols;
	my @pixvals= unpack($templ,$pixstring);
		for my $pixval (@pixvals) {
#print '<'.$pixval.'>',"\n";
$pixval=~s/\s+$//;
if (not defined  $colortable{$pixval}) {
print RAW "0x00000000UL\n";
} else {
print RAW $colortable{$pixval},"\n";
}
		}
#		print $rr,';',scalar(@pixvals),"\n";
#$rr++;
}
close RAW;

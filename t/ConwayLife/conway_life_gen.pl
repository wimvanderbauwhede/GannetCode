#!/usr/bin/perl
use warnings;
use strict;

my $nrows=3;
my $ncols=4;

print << 'ENDH';
; Gannet version of Conway's game of life

(begin
 ; get the initial values. We don't really need the coordinates here, we can just generate random data
 ; but I guess I'd like nice pictures
'(img.in )
'(begin

ENDH

for my $r (0..$nrows-1) {
for my $c (0..$ncols-1) {
print "	(b$r$c.init (img.block '$r '$c) )\n";
}}
print << 'ENDS';
)
; calculate, loop forever
; later, detect that all is quiet and rerun the outer loop forever
(label 'L 
	'(begin
ENDS


for my $r (0..2) {
my $rp=($r==2)?0:$r+1;
my $rm=($r==0)?2:$r-1;
for my $c (0..3) {
my $cp=($c==3)?0:$c+1;
my $cm=($c==0)?3:$c-1;

my $tlc= $rm.$cm;
my $trow =$rm.$c;
my $trc=$rm. $cp;
my $rcol=$r. $cp;
my $brc=$rp. $cp;
my $brow=$rp. $c;
my $blc=$rp. $cm;
my $lcol=$r. $cm;

print "	
		(img.draw '$r '$c
		(b$r$c.return '(b$r$c.block) (b$r$c.core)
		(b$r$c.edges  (b$tlc.brc) (b$trow.brow) (b$trc.blc) (b$rcol.lcol) (b$brc.tlc) (b$brow.trow) (b$blc.trc) (b$lcol.rcol) )))
";
}
}		
		
print "
		'(return L)
	)
)
";




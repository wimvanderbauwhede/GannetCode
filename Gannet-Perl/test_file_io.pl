# Testing file IO

use Gannet;

open my $FH, "<", "./test.txt";
open my $OUT, ">", "./out.txt";
# (IO.open '2 "./test.txt" "r")
#my $line="";
while (my $line = <$FH> ) { # FIXME: while pred must be part of while block scope!
	print $OUT (chomp $line),"\n"; # (IO.print line)
}

 close $FH;
 close $OUT;
# (close '2)

# Testing file IO

use Gannet;

open my $FH, "<", "./test.txt";
open my $OUT, ">", "./out.txt";
# (IO.open '2 "./test.txt" "r")
#my $line="";
while (my $line = <$FH> ) { # FIXME: while pred must be part of while block scope!
    chomp $line;
#	(assign 'line (IO.readln '2))
# not quite: we should declare my $line outside the while, then test the read 
# In fact, I need to test for EOF as predicate, and read the line in the let-block
	print $OUT $line,"\n"; # (IO.print line)
}

 close $FH;
 close $OUT;
# (close '2)

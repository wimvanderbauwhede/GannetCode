use warnings;
use strict;

  
  open my $SVG,'<','sim_core_status.svg.tmp';
  open my $SVGFIX,'>','sim_core_status.svg';
  while (my $line=<$SVG>) {
    $line=~s/(color:)(.*?)(;\s* stroke:)currentColor/$1$2$3$2/;
    print $SVGFIX $line;
}
close $SVG;
close $SVGFIX;


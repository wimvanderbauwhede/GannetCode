package Petrel::Services;
use vars qw( $VERSION );
$VERSION = "1.0.0";

use warnings;
use strict;
use Exporter;
@Petrel::Services::ISA = qw(Exporter);
@Petrel::Services::EXPORT = qw(
            &s1
        );

sub s1 {
    my $res=1;
    for my $v ( @_) {
        $res*=$v;
    }
    return $res;
}
1;

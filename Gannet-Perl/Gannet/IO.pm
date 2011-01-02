package IO;
use Gannet::Service;
@ISA = ("Gannet::Service");
sub new {
    my $class = shift;
    my $self = {};
    bless  ($self, $class);
    return $self;
}

sub say {
    my $self=shift;
    my $arg=shift;
    print $arg,"\n";
}

1;
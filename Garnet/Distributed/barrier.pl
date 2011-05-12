#!/usr/bin/perl
use warnings;
use strict;

use IO::Socket::INET;
use IO::Select;
die "Please specify number of clients.\n" unless @ARGV;
my $verbose=0;
my $nclients=shift @ARGV;

my $ppid=shift @ARGV;#getppid();

# Create the receiving socket
my $s = new IO::Socket::INET (
        LocalHost => 'localhost',
        LocalPort => 7188,
        Proto => 'tcp',
        Listen => 16,
        Reuse => 1,
        );
die "Could not create socket: $!\n" unless $s;

my $read_set = new IO::Select(); # create handle set for reading
$read_set->add($s);           # add the main socket to the set
# Tell parent we're ready to accept connections
print "kill('USR1',$ppid);\n" if $verbose;

kill('USR1',$ppid);

while ($nclients) { # forever
# get a set of readable handles (blocks until at least one handle is ready)
    my ($rh_set) = IO::Select->select($read_set, undef, undef, undef);
# take all readable handles in turn
    foreach my $rh (@$rh_set) {
# if it is the main socket then we have an incoming connection and
# we should accept() it and then add the new socket to the $read_set
        if ($rh == $s) {
            my $ns = $rh->accept();
            $read_set->add($ns);
        } else {
            my $buf = <$rh>;
            if($buf) { # we get normal input
                print "Node ping received!\n" if $verbose;
                $nclients--;
				print "Remaining: $nclients\n" if $verbose;
            } else { # the client has closed the socket
                # remove the socket from the $read_set and close it
                $read_set->remove($rh);
                close($rh);
            }
        }
    }
}
# Tell parent we're finished
kill('USR1',$ppid);


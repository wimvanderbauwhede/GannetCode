#!/usr/bin/perl
use warnings;
use strict;
use IO::Socket::INET;

my $blocked=1;
$SIG{"USR1"} = sub {$blocked=0};
my $pid=$$;
my $id=system("barrier.pl 1 $pid &");
print "PID:$id<>$pid\n";

while($blocked) {};
print "Got SIGUSR1!\n";

$blocked=1;
# Now open a client socket and write to the barrier
# Create the receiving socket
my $s = new IO::Socket::INET (
        PeerAddr => 'localhost',
        PeerPort => 7188,
        Proto => 'tcp',
        Reuse => 1,
        );
die "Could not create socket: $!\n" unless $s;
$s->send('1');
$s->close();

while($blocked) {};
print "Got SIGUSR1!\n";





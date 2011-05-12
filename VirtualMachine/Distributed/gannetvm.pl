#!/usr/bin/perl
use warnings;
use strict;

=pod
The aim of this script is to start the gateway and the services as different processes.

The gateway runs on, well, the gateway, and uses the base port number
Every process takes as argument its port number. For "truly" distributed operation every
process will have a different IP address, obtained using the appropriate socket call.
But we must know in advance which process to run on which core.

So we have a flag $multi_ip

	if ($multi_ip) {} else {}

If there's only a single IP, the Node ID is added to the base port number
If there are multiple IPs, the Node ID is added to the base IP address
Consequently, we use the Node ID as an argument. And we always increment the base port number with the node id.

For multiple IPs, we will have to use ssh -f (or -n) host command args
For single IPs we simply have system('command args')

To know how many processes to start, and what the identifier would be, we need to parse the SystemConfiguration.



=cut


use Data::Dumper;
my $nl="\n";
use Getopt::Std;

use YAML::Syck;
use Socket;

my %opts;
getopts( 'dhmvI:P:Y:', \%opts );

if ( $opts{'h'} ) {
    die "
    Distributed GannnetVM launcher
    Outputs to STDOUT.
    $0 [opts]
	-m: multiple IP addresses, for clusters. Default is single-IP.
    -v: verbose (for debugging)
	-d: dry-run (just print the commands);
	-I: Base IP address
	-P: Base port address
    -Y: YAML config file for Gannet (defaults to SBA.yml)
    \n";
}

my $verbose = $opts{'v'} ? 1 : 0;
my $ymlfile = $opts{'Y'} || 'SBA.yml';
my $multi_ip= $opts{'m'}?1:0;
my $run = $opts{'d'}?0:1;

my $base_ip=$opts{'I'}||'127.0.0.1';
my $base_port=$opts{'P'}||7188;

my %service_nodes=();

if (-e $ymlfile) {
    my $config_href = YAML::Syck::LoadFile($ymlfile);
    my %config = %{$config_href};
    %service_nodes=%{ $config{'System'}{'ServiceNodes'} };
}

for my $k (keys %service_nodes) {
	my $v=$service_nodes{$k};
	my $node_id=$v->[0];
	launch_servicenode($node_id,$multi_ip);
}

launch_gateway($multi_ip);

sub launch_servicenode {
	my $node_id=shift;
	my $multi_ip=shift;
	my $res=0;
	my $command='';
	if ($multi_ip) {
		my $node_ip=get_node_ip($node_id);
		$command="ssh -f $node_ip gannetnode $node_id $multi_ip";
	} else {
		$command="gannetnode $node_id $multi_ip";
	}
	print $command,$nl if $verbose;
	if ($run) {
		$res=system($command);
	}
	return $res;
}

sub launch_gateway {
	my $multi_ip=shift;
	my $res=0;
	my $command='';
#if ($multi_ip) {
#		$command="ssh -f $base_ip gannetgw $multi_ip";
#	} else {
		$command="gannetgw $multi_ip";
#	}
	print $command,$nl if $verbose;
	if ($run) {
		$res=system($command);
	}
	return $res;
}

sub get_node_ip {
	my $node_id=shift;
	my @bytes=split(/\./,$base_ip);
	$bytes[-1]+=$node_id;
	return join('.',@bytes);
}
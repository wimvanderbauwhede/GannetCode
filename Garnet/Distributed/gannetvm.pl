#!/usr/bin/perl
use warnings;
use strict;

=pod
The aim of this script is to start the gateway and the services as different processes.

The gateway runs on, well, the gateway, and uses the base port number
Every process takes as argument its port number. For "truly" distributed operation every
process will have a different IP address, obtained using the appropriate socket call.
But we must know in advance which process to run on which core.

So we have a field $base_ip_int

	if ($base_ip_int) {} else {}

If there's only a single IP, the Node ID is added to the base port number
If there are multiple IPs, the Node ID is added to the base IP address
Consequently, we use the Node ID as an argument. And we always increment the base port number with the node id.

For multiple IPs, we will have to use ssh -f (or -n) host command args
For single IPs we simply have system('command args')

To know how many processes to start, and what the identifier would be, we need to parse the SystemConfiguration.

NOTE: on large multicore machines like the ldc, we can run each process on a different core ("set processor affinity") with the taskset command. What I should do is calculate the mask based on the number of nodes and number of cores

=cut


use Data::Dumper;
my $nl="\n";
use Getopt::Std;
use YAML::Syck;
my $gannet_dir='.';
if (exists $ENV{'GANNET_DIR'}) {
    $gannet_dir=$ENV{'GANNET_DIR'};
}

my $blocked=1;
$SIG{"USR1"} = sub {$blocked=0};

my %opts;
getopts( 'dhmRvI:N:P:Y:', \%opts );

if ( $opts{'h'} or !@ARGV ) {
    die "
    Distributed GannnetVM launcher
    Outputs to STDOUT.
    $0 [opts] .tdc-file
    -v: verbose (for debugging)
	-d: dry-run (just print the commands);
	-I: Base IP address, i.e. address the head node for non-shared-mem machines.
	-P: Base port address
    -Y: YAML config file for Gannet (defaults to SBA.yml)    
    -R: Ruby processes
    -N: Number of cores (for shared-mem or NUMA multicore machines)
    \n";
}

my $verbose = $opts{'v'} ? 1 : 0;
my $ymlfile = $opts{'Y'} || 'SBA.yml';
my $run = $opts{'d'}?0:1;
my $rb=$opts{'R'}?'.rb':'';
my $ncores=$opts{'N'}||1;
my $taskset='';

my $base_ip_str='127.0.0.1';
my $base_ip_int= 0;
if ($opts{'I'}) {
 $base_ip_str=$opts{'I'};
 $base_ip_int= ip_address_str_to_int($base_ip_str);
}

my $base_port=$opts{'P'}||7188;

my $tdc_file=shift @ARGV;
$ymlfile=find_configpath($tdc_file);

my %service_nodes=();

if (-e $ymlfile) {
    my $config_href = YAML::Syck::LoadFile($ymlfile);
    my %config = %{$config_href};
    %service_nodes=%{ $config{'System'}{'ServiceNodes'} };
}
my $nnodes=scalar keys %service_nodes;
if ($ncores>1) {
# use taskset. The gateway goes on processor 1
    
}
# Here we launch the barrier and wait for its signal
if ($run) {
    
    my $pid=$$;
    print "barrier.pl $nnodes $pid &\n" if $verbose;
    system("barrier.pl $nnodes $pid &");
#my $pid=`ps | grep barrier`;
    while($blocked) {};
}
print "Got SIGUSR1 from barrier, launch nodes\n" if $verbose;;
$blocked=1;

# Then we launch all the client nodes

for my $k (keys %service_nodes) {
	my $v=$service_nodes{$k};
	my $node_id=$v->[0];
	launch_servicenode($node_id,$base_ip_int);
}

# Here we wait untill all client nodes have talked back to the barrier 
if ($run) {
while($blocked) {};
}
print "Got SIGUSR1 from barrier, launch gateway\n" if $verbose;;

launch_gateway($base_ip_int);
# this only works for single host
system("killall gannetnode$rb");
################################################################################

sub launch_servicenode {
	my $node_id=shift;
	my $base_ip_int=shift;
	my $res=0;
	my $command='';
	if ($base_ip_int) {
		my $node_ip=get_node_ip($node_id);
		# First need to scp YAML file to target node I guess? 
		$command="ssh -f $node_ip gannetnode$rb $node_id $base_ip_int $verbose $ymlfile";
	} else {
    	$taskset=compute_taskset($node_id,$nnodes,$ncores);
		$command=$taskset."gannetnode$rb $node_id $base_ip_int $verbose $ymlfile &";
	}
	print $command,$nl if $verbose;
	if ($run) {
		$res=system($command);
	}
	return $res;
}

sub launch_gateway {
	my $base_ip_int=shift;
	my $res=0;
	my $command='';
	$command="gannetgw$rb base_ip_int $verbose $tdc_file";
	print $command,$nl if $verbose;
	if ($run) {
		$res=system($command);
	}
	return $res;
}

sub get_node_ip {
	my $node_id=shift;
#	my @bytes=split(/\./,$base_ip);
#	$bytes[-1]+=$node_id;
#	return join('.',@bytes);
    return $base_ip_int+$node_id;
}
sub ip_address_str_to_int {
(my $ip_str)=@_;
    my @bytes=split(/\./,$ip_str);
    my $ip_int=($bytes[0] << 24)+($bytes[1] << 16)+($bytes[2] << 8)+$bytes[3]; 
    return $ip_int;
}

sub compute_taskset {
    (my $node_id, my $nnodes, my $ncores)=@_;
    if ($ncores==1) {
        return '';
    } elsif ($ncores>$nnodes) {
        my $mask=sprintf('%x',2**$node_id); 
        return "taskset 0x$mask "; 
    } elsif ($ncores==1) {
        if ($node_id==0) {
            return 'taskset 0x1 ';
        } else {
            return 'taskset 0x2 ';
        }
    } else {
            my $nodes_per_core=int(($nnodes+1)/$ncores); # e.g. if nnodes+1 is 13 and ncores is 4, we have 13/4 = 4, 
            if ($nodes_per_core<$nnodes/$ncores) {
                $nodes_per_core++;
            }
            my $taskset='';
            for my $i (0..$ncores-1) {
                if ($node_id-$nodes_per_core*$i<$nodes_per_core and $node_id-$nodes_per_core*$i>=0) {
                    $taskset= 'taskset 0x'.(2**$i).' '; # so 0,1,2,3 go on core 1;     
                    last;                    
                } else {
                    $taskset= 'taskset 0x'.(2**$ncores).' ';
                }
            }
            return $taskset;    
    }
}

sub find_configpath { 
    (my $tdc_file)=@_;
    my $NBYTES=4;
    my $F_Length=0x01ff0000; 
    my $FS_Length=16;
    if ($tdc_file=~/\.tdc64/) {
        $NBYTES=8;
        $F_Length=0x0000ffff00000000;
        $FS_Length=32;
    }
    open my $TDCH, '<',$tdc_file;
    binmode($TDCH); 
    # Find number of packets
    my $npackets_str='';
    read($TDCH, $npackets_str, $NBYTES, 0);
    my @npackets_bytes=unpack('W'x$NBYTES,$npackets_str);
    my $npackets=$npackets_bytes[0]*8+$npackets_bytes[1];
    print "NPackets: $npackets\n" if $verbose;;
	my $offset=0;
    # For each packet
    for my $np (1..$npackets) {
        # get the length from the header
        my $header_word_str=0; 
        read($TDCH,$header_word_str,$NBYTES,0);
		my @header_word_bytes=unpack('W' x $NBYTES,$header_word_str);
        print "Header: ".join(':',@header_word_bytes)."\n" if $verbose;;
		my $length=0;#($header_word & $F_Length) >> $FS_Length;
		if ($NBYTES==8) {
			$length=$header_word_bytes[2]*256+$header_word_bytes[3];
		} elsif ($NBYTES==4) {
			$length=($header_word_bytes[0]&1)*256+$header_word_bytes[2];
		}
        print "Packet $np length: $length\n" if $verbose;;
        $offset=($length+3)*$NBYTES;
        seek($TDCH,$offset-$NBYTES,1);
    }

    # If the bytecode contains the path to the YAML file, it should be in the
# remaining Words
    my $ymlfile="";
	my $byte='';
    while($ymlfile!~/\.yml/) {
        my $word='';
        read($TDCH,$word,8,0);
#        $offset+=$NBYTES;
        my @t=unpack('a'x8,$word);
		$ymlfile.=join('',@t);
    }
    if (-e $ymlfile) {
    return $ymlfile
    } else {
    $ymlfile=~s/^.*\///;
    $ymlfile="$gannet_dir/SystemConfigurations/$ymlfile";
    if (-e $ymlfile) {
        return $ymlfile;
    } else {
        die "Could not find $ymlfile.\n";
    }
    }
}


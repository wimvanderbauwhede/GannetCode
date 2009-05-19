#!/usr/bin/perl
use warnings;
use strict;
use Carp;


if (not @ARGV) {
	die "Please specify a grammar file name.\n";
}
use Parse::RandGen;
use Parse::RandGen::ExtendPick;

my %ops=();

my $grammar= Parse::RandGen::Grammar->new('Gannet');
# a better way would be to parse the grammar file ...

my $grammar_file=shift @ARGV;
#die "<$grammar_file>";
open(my $GRAMMAR,"<$grammar_file");
my $code='';
while(<$GRAMMAR>) {
	next if /^\s*\#/;
	next if /^\s*$/;
	s/\#.*//;
	# maybe I should get matrix size from the grammar as well?
	# though that is not grammar but config of course
	# get operators and execution times
	/^\s*\'([^\']+)\'\s*=>\s*(\d+)/ && do {
    	$ops{$1}=$2;
	};
	/:/ && do {
		#chomp;
		my $rule_begin='$grammar->defineRule("';
		my $rule_end='")->set(prod=>[cond=>"';
		s/^\s*/$rule_begin/;
        s/:\s*/$rule_end/;
        my $or='"],prod=>[cond=>"';
        s/\s+\'\s+\'\s+/ __WS__ /g;
		s/\s*\|\s*/$or/g;
		my $and='",cond=>"';
		my $prod_end='"]);';
		s/\s*$/$prod_end/;	
		s/\s+/$and/g;
		# fix whitespace
		s/__WS__/' '/g;
		# fix regexes
		s/\"qr\//qr\//g;
		s/\/\"/\//g;
		$_.="\n";
	$code.=$_;	
	};
}
close $GRAMMAR;

eval $code;

my $noc_link_delay=1; #5; 
my $payload_size=64;
my $header_size=0; #3;
my $packet_size=$payload_size+$header_size;

my $nruns=100;
    open my $REFS, '>', 'ref_results.txt';
for my $run (1..$nruns) {
    my $tdfile="test_matrixops_$run.td";

    open my $TD, '>',$tdfile;
    
my @tokens=$grammar->rule("expression")->extPick();

my $run_time=0;
my $nops=0;

my $transfer_time=$packet_size*$noc_link_delay; # FIXME: only OK for single hop

my $expression='';
for my $token (@tokens) {

    if (exists $ops{$token}) {
        $nops++;
        $run_time += $transfer_time + $ops{$token};
        $token=~s/\_\d+//;
    }
$expression.= lc $token;    
}

    #print ";Expression #ops: $nops\n;Expression run time: $run_time\n$expression\n"; 
    print $REFS "$run\t$nops\t$run_time\n"; 
    print $TD ";Expression #ops: $nops\n;Expression run time: $run_time\n$expression\n";
    close $TD;
 
    system("./guillemot -Y ../../Garnet/MatrixOps.SBA.yml $tdfile");
}    
close $REFS;

# end

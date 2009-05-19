#!/usr/bin/perl
use warnings;
use strict;

use Getopt::Std;

#my $gannet_dir = $ENV{GANNET_DIR}.'/Garnet';
my $platform = 'macosx';
my $os=`uname`;
if ($os=~/Linux/) {
    $platform='linux';
    }
    
my %opts;
getopts( 'hv', \%opts );

if ( $opts{'h'}) {
	die "
	Gannnet build script for SystemC
	$0 [opts] 
	-Y YAML-file.yml: SBA config file to use
	-v: verbose (for debugging)
	\n";
}

my $verbose=$opts{'v'}?1:0;

my @sc_module_list =qw(
activate_subtask
core_control
dispatch_data_packets
parse_subtask
parse_task_description
prepare_subtask
receive_packets
store_data
store_result_packets
store_subtask_code
transmit_packets
);


my @sc_helper_list =qw(
activate_subtask_helper
build_value_address_list
demux_packets_by_type
restart_subtask
send_ack
clean_up
);

my %sc_modules=();

for my $name (@sc_module_list) {
    $sc_modules{$name}=$name;
}

my %sc_helpers=();

for my $name (@sc_helper_list) {
    $sc_helpers{$name}=$name;
}


my $SCM;
my $in_module=0;
my $module='';
my $class='';
my $skip=0;
my $helper=0;

while(my $line=<>) {
    print $line if $verbose;
    $line=~/(void|\w+)\s+(\w+)::(\w+)(\(.*?\))\s+\{/ && do {
        $class=$2;
        my $name=$3;
		print "CLASS: $class; NAME: $name\n" if $verbose;
        my $helper=(exists $sc_helpers{$name});
        if (not -d $class) {
            mkdir $class;
        }
        if (exists $sc_modules{$name} or $helper) {
            open $SCM,'>',"$class/$name.cc";    
            $module=$name;
            $in_module=1;
        }
#        if ($helper) {
       # print $SCM $line;
#        }
        next;
    };
    
    $line=~/\/\/\s+of\s+$module/ && do {
        if ($in_module) {
#                if ($helper) {
      #  print $SCM $line;
#        }
            close $SCM;
            $in_module=0;
            $module='';   
        }
    };
    
    if ($in_module) {
        ($line=~/\#ifdef\s+CYCLES/) && do {
            $skip=1;
            next;
        };

        ($line=~/\#endif/) && ($skip==1) && do {
            $skip=0;
            next;
        };
#        print $line unless $skip;
        print $SCM $line unless $skip;
    }
    
};
# to allow build system to check for change
system("date > $class/generated_on");



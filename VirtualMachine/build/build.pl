#!/usr/bin/perl
use warnings;
use strict;

# build script for VirtualMachine GannetVM
# should be run inside VirtualMachine/build

use Cwd;
use Getopt::Std;

my $gannet_dir = $ENV{GANNET_DIR};
my $platform = 'macosx';
my $os=`uname`;
if ($os=~/Linux/) {
    $platform='linux';
    }
    
my %opts;
getopts( 'hvNsVdbgcY:', \%opts );

if ( $opts{'h'} or not scalar %opts) {
	die "
    Gannet build script for VirtualMachine GannetVM
    $0 [opts] 
    -Y YAML-file.yml: SBA config file to use
    -v: verbose (for debugging)
    -N: Don't generate C++ sources from Ruby code
    -s: static allocation
    -V: VM (default is HW)
	-X: cross-compile for Linux on PPC
    -g: generate SystemConfiguration.h from YAML-file, don't build
    -b: build only, don't generate
    -c: clean
    -d: debug
    \n";
}


my $verbose= $opts{'v'}?1:0;
my $generate= $opts{'b'}?($opts{'g'}?1:0):1;
my $build= $opts{'g'}?($opts{'b'}?1:0):1;
my $clean=$opts{'c'}?1:0;
my $debug=$opts{'d'}?1:0;

my $scons_nogen=$opts{'N'}?'nogen=1':'';
my $scons_v=$verbose?'v=1':'';
my $scons_d=$debug?'dbg=1':'';
my $scons_dyn=$opts{'s'}?'':'dyn=1';
my $scons_vm=$opts{'V'}?'vm=1':'';
my $scons_xc=$opts{'X'}?'xc=1':'';

my $ymlfile = $opts{'Y'}||"$gannet_dir/SystemConfigurations/SBA.yml";

my $wd=cwd();

my $ymlpath=$ymlfile;
if ($ymlfile!~/^\//) {
	$ymlpath="$wd/$ymlfile";
}

my $cxx_source_path="$gannet_dir/VirtualMachine/SBA";
my $cxx_testbench_path="$gannet_dir/VirtualMachine/build";

my $run_scons_str="GANNET_YML_CONFIG=$ymlpath scons $scons_v $scons_d $scons_dyn $scons_vm $scons_nogen -f SConstruct";

$wd="$gannet_dir/VirtualMachine/build";
if ($clean) {
# 3. Clean VirtualMachine build 
print "chdir $cxx_testbench_path\n";
chdir "$cxx_testbench_path";
print "$run_scons_str -c\n";
system("$run_scons_str -c");
} else {
if ($generate) {
chdir "$gannet_dir/Garnet";
# 1. Generate SystemConfiguration from YAML file 
# TODO: this should go into the GannetBuilder

#my $gen_sysconfig_str="ruby ./create_Cxx_SystemConfiguration.rb -Y $ymlpath -D $cxx_source_path";
#print $gen_sysconfig_str,"\n";
#system($gen_sysconfig_str);

my $create_config_scons_str="scons -f SConstruct.SystemConfiguration.py Y='$ymlpath' D='$cxx_source_path' gen";
        print "$create_config_scons_str\n";
        system($create_config_scons_str);
}
if ($build) {
# 3. Build VirtualMachine code 
print "chdir $cxx_testbench_path\n";
chdir "$cxx_testbench_path";

print $run_scons_str,"\n";
system($run_scons_str);
# Install binary
print $run_scons_str," install\n";
system("$run_scons_str install");
}
}

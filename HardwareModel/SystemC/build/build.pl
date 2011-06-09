#!/usr/bin/perl
use warnings;
use strict;

# build script for SystemC Gannet
# should be run inside SystemC/bench

use Cwd;
use Getopt::Std;

my $gannet_dir = $ENV{GANNET_DIR};
my $platform = 'macosx';
my $os=`uname`;
if ($os=~/Linux/) {
    $platform='linux';
    }
    
my %opts;
getopts( 'hvdbgNcQY:', \%opts );

if ( $opts{'h'} or not scalar %opts) {
	die "
	Gannet build script for SystemC
	$0 [opts] 
	-Y YAML-file.yml: SBA config file to use
    -Q: use the Quarc NoC integration
	-v: verbose (for debugging)
	-g: generate SystemC files from YAML-file and ServiceCoreLibrary.rb, don't build
	-N: Don't generate SystemC files from Ruby
	-b: build only, don't generate
	-c: clean
	-d: debug
	\n";
}

my $quarc= $opts{'Q'}?1:0;
my $verbose= $opts{'v'}?1:0;
my $generate= $opts{'b'}?($opts{'g'}?1:0):1;
my $build= $opts{'g'}?($opts{'b'}?1:0):1;
my $clean=$opts{'c'}?1:0;
my $debug=$opts{'d'}?1:0;
my $gen_sc=$opts{'N'}?0:1;

my $scons_q=$quarc?'quarc=1':'';
my $scons_v=$verbose?'v=1':'';
my $scons_gen=$gen_sc?'scgen=1':'';
my $scons_d=$debug?'dbg=1':'';

my $ymlfile = $opts{'Y'}||"$gannet_dir/SystemConfigurations/SBA.yml";

my $wd=cwd();

my $ymlpath=$ymlfile;
if ($ymlfile!~/^\//) {
	$ymlpath="$wd/$ymlfile";
}

my $sysc_lib_path="$gannet_dir/HardwareModel/SystemC/src";
if ($gen_sc) {
    $sysc_lib_path="$gannet_dir/HardwareModel/SystemC/scsrc";
}
my $sysc_genlib_path="$gannet_dir/HardwareModel/SystemC/gensrc";

#my $sysc_obj_path='SystemC/test';
my $sysc_obj_path="$gannet_dir/HardwareModel/SystemC/scsrc/SBA";
my $cxx_source_path="$gannet_dir/VirtualMachine/SBA";
my $cxx_testbench_path="$gannet_dir/VirtualMachine/build";

my $run_scons_str="scons sysc=1 $scons_v $scons_d $scons_gen -f SConstruct.gannet_sc.py";
my $run_scons_sysc_str="scons $scons_q $scons_v $scons_d $scons_gen";

my $create_config_scons_str="scons -f SConstruct.SystemConfiguration.py sysc=1 Y='$ymlpath' D='$sysc_lib_path' gen";
my $create_corelib_scons_str="scons -f SConstruct.ServiceCoreLibray.py sysc=1 Y='$ymlpath' D='$sysc_lib_path' gen";
my $create_gen_sysc_scons_str="scons -f SConstruct.Generate_SC_SBA.py Y='$ymlpath' D='$sysc_genlib_path' gen";

#Types.cc
my @symlinked_cxx_sources=qw(
Base
Types.h
ServiceManagerObjects.h
LookupTable.cc
LookupTable.h
);

$wd="$gannet_dir/HardwareModel/SystemC/build";
if ($clean) {
    if ($generate) {
        chdir "$gannet_dir/Garnet";
        # 1. Clean generated SC_SystemConfiguration from YAML file
        print "$create_config_scons_str -c\n";
        system("$create_config_scons_str -c");
	unlink "$sysc_lib_path/SC_SystemConfiguration.h";
	unlink "$sysc_lib_path/SC_SystemConfigurationConsts.h";
        # 2. Clean SC_ServiceCoreLibrary from Ruby code        
        print "$create_corelib_scons_str -c\n";
        system("$create_corelib_scons_str -c");
        if ($gen_sc) {
            # Generate source files for inclusion is SystemC
            system($create_gen_sysc_scons_str.' -c');
            chdir "$sysc_genlib_path";
            system("scons -f SConstruct".' -c');             
        }                           
    }
    
    # 3. Clean C++ build 
    print "chdir $cxx_testbench_path\n";
    chdir "$cxx_testbench_path";
    print "$run_scons_str -c\n";
    system("$run_scons_str -c");
    # 4. Clean SystemC build
    chdir $wd;
    print("$run_scons_sysc_str -c\n");    
    system("$run_scons_sysc_str -c"); 
    
} else {
    if ($generate) {
        chdir "$gannet_dir/Garnet";
        # 1. Generate SC_SystemConfiguration from YAML file
        print "$create_config_scons_str\n";
        system($create_config_scons_str);
        # 2. Generate SC_ServiceCoreLibrary from Ruby code        
        print "$create_corelib_scons_str\n";
        system($create_corelib_scons_str);
        if ($gen_sc) {
            # Generate source files for inclusion is SystemC
            print "$create_gen_sysc_scons_str\n";
			system($create_gen_sysc_scons_str);
            print "gensrc:$sysc_genlib_path\n";         
            chdir "$sysc_genlib_path";
            system("scons -f SConstruct");             
        }                           
    }
            
    if ($build) {
        chdir $wd;
		# Check if SBA dir exists
		if (not -d  "$sysc_obj_path/") {
			mkdir  "$sysc_obj_path/";
		}
		# Symlink C++ sources
        for my $file (@symlinked_cxx_sources) {
            if (not -l "$sysc_obj_path/$file" or not -e "$sysc_obj_path/$file") {
                system("ln -f -s $cxx_source_path/$file $sysc_obj_path/$file");
            }
        }        
        # Build C++ code 
        print "chdir $cxx_testbench_path\n";
        chdir "$cxx_testbench_path";
        print "$run_scons_str\n";
        system("$run_scons_str");
        system("$run_scons_str movelib");

        # Build SystemC code
        chdir $wd;
        print("$run_scons_sysc_str\n");    
        system("$run_scons_sysc_str"); 
        
        # Install SystemC code
        chdir $wd;
        print("$run_scons_sysc_str install\n");    
        system("$run_scons_sysc_str install"); 
        
    }
}

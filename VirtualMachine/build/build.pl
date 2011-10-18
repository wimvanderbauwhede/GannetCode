#!/usr/bin/perl
use warnings;
use strict;

# build script for VirtualMachine GannetVM
# should be run inside VirtualMachine/build
use Config;
use Cwd;
use Getopt::Std;
use YAML::Syck;
use Data::Dumper;

my $gannet_dir = $ENV{GANNET_DIR};
my $sba_dir=cwd();
my $platform = 'macosx';
my $os=`uname`;
if ($os=~/Linux/) {
    $platform='linux';
    }
    
my %opts;
getopts( 'hvNZnsHSTPW:dbgcCY:', \%opts );

if ( $opts{'h'} or scalar( keys %opts)==0) {
	die "
    Gannet build script for VirtualMachine GannetVM
    $0 [opts] 
    -Y YAML-file.yml: SBA config file to use
    -v: verbose (for debugging)
    -N: NEW, uses SConstruct.New and implements 'New' features
    -Z: NEWER, uses SConstruct.Newer and implements 'Newer' features
    -n: Don't generate C++ sources from Ruby code
    -s: static allocation
    -H: model HW (default is VM)
    -S: use POSIX sockets (no longer supported)
    -T: use POSIX threads
    -P: use communicating processes
    -W 32|64: specify WORDSZ (either 32 or 64) (defaults to the host width!)
    -X: cross-compile for Linux on PPC
    -g: generate SystemConfiguration.h from YAML-file, don't build
    -b: build only, don't generate
    -c: clean
    -C: count cycles
    -d: debug
    \n";
}


my $verbose= $opts{'v'}?1:0;
my $generate= $opts{'b'}?($opts{'g'}?1:0):1;
my $build= $opts{'g'}?($opts{'b'}?1:0):1;
my $clean=$opts{'c'}?1:0;
my $scons_c=$clean?'-c':'';
my $debug=$opts{'d'}?1:0;

my $scons_ext=$opts{'N'}?'.New':'';
$scons_ext=$opts{'Z'}?'.Newer':'';

my $scons_new=($opts{'N'} || $opts{'Z'})?'new=1':'';
my $scons_nogen=$opts{'n'}?'nogen=1':'';
my $scons_v=$verbose?'v=1':'';
my $scons_d=$debug?'dbg=1':'';
my $scons_dyn=$opts{'s'}?'':'dyn=1';
my $scons_vm=$opts{'H'}?'':'vm=1';
#my $scons_svm=$opts{'S'}?'svm=1':'';
my $scons_svm='';
my $scons_sock=$opts{'S'}?'sock=1':'sock=0';
my $scons_pthreads=$opts{'T'}?'pthreads=1':'';
my $scons_distr=$opts{'P'}?'distr=1':'';
my $scons_xc=$opts{'X'}?'xc=1':'';
my $scons_cycles=$opts{'C'}?'cycles=1':'';
my $ymlfile = $opts{'Y'}||"$gannet_dir/SystemConfigurations/SBA.yml";
my $wordsz = (exists $opts{'W'})?1*$opts{'W'}: (exists $opts{'H'}?32:$Config{longsize}*8);
my $scons_wordsz=($wordsz ==32)?'':'wordsz='.$wordsz;

my $wd=cwd();

my $ymlpath=$ymlfile;
if ($ymlfile!~/^\//) {
	$ymlpath="$wd/$ymlfile";
}
my $scons_sclib='';
my $sclib='';
if($opts{'Z'}) {
	my $config_href = YAML::Syck::LoadFile($ymlpath);
	my %config = %{$config_href};
	$sclib= shift @{ $config{'System'}{'Libraries'} }; #FIXME: need to process all libs! AWFULL HACK!!!
#	@{$scons_sclib}=map {'sclib='.$_ } @{$sclib};
	$scons_sclib='sclib='.$sclib;
}    

my $cxx_source_path="$gannet_dir/VirtualMachine/SBA";
my $cxx_testbench_path="$gannet_dir/VirtualMachine/build";

my $run_scons_str="GANNET_YML_CONFIG=$ymlpath scons $scons_c $scons_new $scons_sclib $scons_v $scons_d $scons_cycles $scons_dyn $scons_vm $scons_sock $scons_pthreads $scons_wordsz $scons_nogen
-f SConstruct$scons_ext";
$run_scons_str=~s/\s+/ /g;

# for SEQVM
# if ($svm) {
# $run_scons_str="GANNET_YML_CONFIG=$ymlpath scons $scons_v $scons_d $scons_cycles $scons_dyn $scons_svm $scons_sock $scons_pthreads $scons_nogen -f SConstruct.svm";
# }

$wd="$gannet_dir/VirtualMachine/build";
if ($clean) {
# 3. Clean VirtualMachine build 
	print "chdir $cxx_testbench_path\n";
	chdir "$cxx_testbench_path";
	print "$run_scons_str\n";
	system("$run_scons_str");
	print "rm ../SBA/SystemConfiguration.h\n";
	unlink "../SBA/SystemConfiguration.h";
} else {
	if ($generate or $clean) {
		my $c=($clean)?'-c':'';
		chdir "$gannet_dir/Garnet";
# 1. Generate SystemConfiguration from YAML file 
#TODO: this should go into the GannetBuilder
		my $create_config_scons_str="scons $c -f SConstruct.SystemConfiguration.py Y='$ymlpath' D='$cxx_source_path' WD='$sba_dir' $scons_distr $scons_wordsz gen";
        $create_config_scons_str=~s/\s+/ /g;
		print "$create_config_scons_str\n";
		system($create_config_scons_str);
	}
	if ($build) {
# 2. Build VirtualMachine code 
		print "chdir $cxx_testbench_path\n";
		chdir "$cxx_testbench_path";
		#FIXME: somehow SCons refuses to generate this
#FIXME: the generated C++ files should not be placed in $gannet_dir/VirtualMachine/SBA/ServiceCoreLibraries/ but in a local build directory!
		if($opts{'Z'}) {
			my $r2nh_sclib='';
			my $r2ncc_sclib='';
			if (-e "$sba_dir/Gannet/$sclib.rb") {
#			my $r2nh_sclib= "perl -I../../util ../../util/r2n.pl -Y $ymlpath -H $sba_dir/Gannet/$sclib.rb > $gannet_dir/VirtualMachine/SBA/ServiceCoreLibraries/$sclib.h";
#			my $r2ncc_sclib="perl -I../../util ../../util/r2n.pl -Y $ymlpath -CC $sba_dir/Gannet/$sclib.rb > $gannet_dir/VirtualMachine/SBA/ServiceCoreLibraries/$sclib.cc";
			$r2nh_sclib= "perl -I../../util ../../util/r2n.pl -Y $ymlpath -H $sba_dir/Gannet/$sclib.rb > $sba_dir/Gannet/$sclib.h";
			$r2ncc_sclib="perl -I../../util ../../util/r2n.pl -Y $ymlpath -CC $sba_dir/Gannet/$sclib.rb > $sba_dir/Gannet/$sclib.cc";
			} else {
			$r2nh_sclib= "perl -I../../util ../../util/r2n.pl -Y $ymlpath -H $gannet_dir/Garnet/SBA/ServiceCoreLibraries/$sclib.rb > $gannet_dir/VirtualMachine/SBA/ServiceCoreLibraries/$sclib.h";
			$r2ncc_sclib="perl -I../../util ../../util/r2n.pl -Y $ymlpath -CC $gannet_dir/Garnet/SBA/ServiceCoreLibraries/$sclib.rb > $gannet_dir/VirtualMachine/SBA/ServiceCoreLibraries/$sclib.cc";
			}
			print "$r2nh_sclib\n";
			system($r2nh_sclib);
			print "$r2ncc_sclib\n";
			system($r2ncc_sclib);
		}
		print $run_scons_str,"\n";
		system($run_scons_str);
# 3. Install binary
		print $run_scons_str," install\n";
		system("$run_scons_str install");
	}
}

#!/usr/bin/perl -w
use strict;
use Cwd;
my $wd=cwd();

my $lutil='';
my $path=$ENV{'GAMBIT_PATH'};
if (`uname -s`=~/Linux/) {
	$lutil='-lutil';
}

my $f=$ARGV[0];
chomp $f;
$f=~s/\.\w+$//;
if (-e $f) {
	unlink $f;
	unlink "$f.c";
	unlink "${f}_.c";
}
#if (not -e "$f.c") {
#system("gsc -c $f.scm");
#}
if (not -e "${f}_.c") {
	system("gsc -link $f.scm");
}
print("gcc -I$path/include/ -L$path/lib $f.c ${f}_.c -lgambc -lm -ldl $lutil -o $f\n");
system("gcc -I$path/include/ -L$path/lib $f.c ${f}_.c -lgambc -lm -ldl $lutil -o $f");


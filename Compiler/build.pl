#!/usr/bin/perl
use Cwd;
my $wd=cwd();
die "$0 options:
    -c: configure
    -C: clean, then configure
" if ($ARGV[0] eq '-h');
my $s=$ARGV[0] || "";
my $infile='../infile.td';
if ($s!~/^\-/ and $s ne "") {
    $infile=$s;
    print "input file: $infile\n";
}
if (-e "./dist/build/gannet/gannetc" and $s ne '-r'){
    unlink "./dist/build/gannet/gannetc";
}
if (-e "./dist/build/Gannet/gannetc" and $s ne '-r'){
    unlink "./dist/build/Gannet/gannetc";
}
#chdir "$wd/src";
if ($s eq '-c') {
#system("runhaskell Setup.hs configure --user ");
    system("cabal configure");
}
if ($s ne '-r') {
    if ($s eq '-C') {
    system("cabal clean");
#    system("runhaskell Setup.hs clean");
    system("cabal configure");
#       system("runhaskell Setup.hs configure ");
    }	
#system("runhaskell Setup.hs build");
    system("cabal build");
    system("cabal install --bindir=../bin");
}

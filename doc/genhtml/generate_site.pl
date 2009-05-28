#!/usr/bin/perl -w
use strict;
use Cwd;
use File::Copy;
use English;
=info
WV 09/01/2005
Script to make it easier to create/maintain a site with server-side includes
WV 26/12/2007
Modified version to generate includes from Markdown text files
using hsmarkdown
=cut
#die cwd(),"\n",$0;
#my $path=$0;
#$path=~s/[\w\.]+$//;
#chdir $path;
my $gen_dir="../html";
my $cssdir='stylesheet';
my @stylesheets=qw(
site.css
sidebar.css
);
my $ext='shtml';
my $gen_from_markdown=1;
my $markdown_ext='txt';
my $ssi_ext='html';
my $cfmext='cfm';
my $gen_ext='html';
################################################################################
##
## No changes required below this line
##
################################################################################

my $basedir=cwd();

if (not -d $gen_dir) {
mkdir "$basedir/$gen_dir",0777;
}
if (not -d "$basedir/$gen_dir/$cssdir") {
mkdir "$basedir/$gen_dir/$cssdir",0777;
}

for my $stylesheet (@stylesheets) {
copy("$basedir/$cssdir/$stylesheet","$basedir/$gen_dir/$cssdir/$stylesheet");
}

if (not -d "$basedir/$gen_dir/images") {
mkdir "$basedir/$gen_dir/images",0777;
}
system("rsync -ua --exclude \"*.svg\" $basedir/images/ $basedir/$gen_dir/images/");

my @pages=glob("*.$ext");
my @cfmpages=glob("*.$cfmext");
push @pages,@cfmpages;
foreach my $page (@pages) {
if ( $page=~/\.$cfmext$/) {
	$gen_ext=$cfmext
} else {
	$gen_ext=$ssi_ext
};
my $gen_page=$page;
$gen_page=~s/\.($ext|$cfmext)$//;
open(HTML,">$basedir/$gen_dir/$gen_page.$gen_ext") or die($!);

open(MAIN,"<$page");
while(<MAIN>) {
#print $_;
	/include\s+\w+=\"(\w+\.$ssi_ext)\"/ && do {
		my $include=$1;
		if ($gen_from_markdown) {
			my $mdfile=$include;
			$mdfile=~s/$ssi_ext/$markdown_ext/;
			if (-e $include) {
				unlink $include;
			}
			print "$mdfile\n";
			open (my $MD,'<',"../$mdfile") or die "Can\'t open $mdfile";
			while (my $line=<$MD>) {
				$line=~/include\s+\w+=\"(\w+\.$ssi_ext)\"/ && do {
					my $m_include=$1;
					my $m_mdfile=$m_include;
					$m_mdfile=~s/$ssi_ext/$markdown_ext/;
					if (-e $m_include) {
						unlink $m_include;
					}
					print "\#include $m_mdfile\n";
					system("hsmarkdown ../$m_mdfile > include/$m_include");
				};
			}
			close $MD;
			system("hsmarkdown ../$mdfile > include/$include");
		}
		open(my $INCL,'<',"include/$include") or die;
		my $inclcode='';
		while(my $line=<$INCL>) {
			if ($line=~/include\s+file\s*=\"(\w+\.$ssi_ext)\"/ ) {
				my $m_include=$1;
				open(my $MINCL,'<',"include/$m_include") or die $!;
				my $m_inclcode='';
				while(my $mline=<$MINCL>) {
					$m_inclcode.=$mline;
				}
				close $MINCL;
				my $patt='<!-- #include file="'.$m_include.'.'.$ssi_ext.'" -->';
				$line=~s/\<\!\-\-.*?\-\-\>//;	
				$inclcode.=$line.$m_inclcode;
			} else {
				$inclcode.=$line;
			}
		}
		close $INCL;

		print HTML $inclcode;
		next;
	};

	print HTML $_;
}
close MAIN;
close HTML;
}



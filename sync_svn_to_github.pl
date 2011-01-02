#!/usr/bin/perl
use warnings;
use strict;
if (!@ARGV) {
	die "Please specify a commit message (in double quotes) or -a for a full, interactive transfer.";
}

my $commit_message=shift @ARGV;

my $gannet_dir=$ENV{'GANNET_DIR'}; 
my $github_dir=$gannet_dir; $github_dir=~s/Gannet/GitHub/;

my $all=$commit_message eq '-a'?1:0;
if ($all) {
system("rm -Rf $github_dir/GannetCode-SVN-export");
system("svn export . $github_dir/GannetCode-SVN-export");
system("rsync -uva $github_dir/GannetCode-SVN-export/ $github_dir/GannetCode");

# A better way is to find files that have changed in svn and copy these to GitHub
# Problem is, if svn is down, I can't really tell if a file has changed many times.
# But if svn works, after a commit there are no files with status AMDR ...
exit;
}
my @svn_status_lines=`svn status`;
open my $SH, '>', "$github_dir/git-GannetCode.sh";
for my $line (@svn_status_lines) {
   if ( $line=~/^([AMDR])/) {
	   my $status=$1;
       $line=~s/^\w\s+//;
       chomp $line;
	   if ($status ne 'D') {
		   print $SH "cp $gannet_dir/$line .\n";
		   print $SH "git add $line\n";
	   } else {
		   print $SH "git rm $line\n";
	   }
   }
}
print $SH "git commit -a -m \"$commit_message\"\n";
print $SH "git push origin master\n";
close $SH;
chdir "$github_dir/GannetCode";
system('pwd');
#system('chmod +x git-GannetCode.sh');
system('cat ../git-GannetCode.sh');
#system('bash ../git-GannetCode.sh');

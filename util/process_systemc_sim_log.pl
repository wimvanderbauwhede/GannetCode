#!/usr/bin/perl
use warnings;
use strict;

use Getopt::Std;

my $gannet_dir = $ENV{GANNET_DIR};

my %opts;
getopts( 'hpY:', \%opts );

if ( $opts{'h'} or !@ARGV) {
	die "usage: $0 -Y YAML-file < systemc-log\nIf not _Y is used, it is assumed that the systemc log file is named after the .td file, e.g. td_file.sysc.log\n";
}
my $show_traffic=$opt{'p'}?1:0;

my $sysc_log_file=$ARGV[0];
if (not -e $sysc_log_file) {
	die "The file $sysc_log_file is not present in the current directory.\n";
}

my $td_file=$sysc_log_file;
$td_file=~s/\..+$//;
my $ymlfile = $opts{'Y'}||"$gannet_dir/SystemConfigurations/SBA.yml";

open(my $TD, '<', "$td_file.td");
while (my $line=<$TD>) {
    if ($line=~/^\s*\;\s*[Ss]ys(?:tem)*\s*[cC]onfig(?:uration)*\s*[\:\=\/]?\s*([\w\.\/]+)/) {
        my $maybe_yml_file=$1;
        if ($maybe_yml_file!~/\.yml/) {
            $maybe_yml_file.='.yml';
        }
        if (-e "$gannet_dir/SystemConfigurations/$maybe_yml_file") {
            print "Found YAML-file $maybe_yml_file\n";
            $ymlfile="$gannet_dir/SystemConfigurations/$maybe_yml_file";
        }
    last;
    }
}
close $TD;


my @servicecore_names= get_servicecore_names($ymlfile);
my @waves=();

#SC_Register Runtime.sba.instance3.service_manager.core_status value_changed_event() called, value is now 3#
#    10980 ns: SC_Register Runtime.sba.instance2.service_manager.core_status write() called, value is now 0
my $t_fin=0;
my $done=0;

open my $SYSCLOG,'<',$sysc_log_file;
while (<$SYSCLOG>) {
/DONE:\ / && do {
		  chomp;
		  $t_fin=$_;
		  $t_fin=~s/DONE:.//;
		  $t_fin=~s/\s+.*$//;        
			  $t_fin/=1000;
		  my $t_fin_round=int($t_fin/50)*50;
		  if ($t_fin_round<$t_fin) {
			  $t_fin=$t_fin_round+50;
		  } else {
			  $t_fin=$t_fin_round;
		  }
		  $done=1;
		  last;
	  };
	  next unless /core_status/;

	  s/^\s+//;
	  s/^\s*0\s+s:/0/;
	  s/\s+us:/000/;
	  s/\s+ns://;
#5672670 ns: SC_Register Runtime.sba.instance13.service_manager.core_status value_changed_event() called, value is now 2
	  s/SC_Register\ Runtime\.sba\.// or next;
	  s/\.service_manager\.core_status\s+(value_changed_event|write)..\s+called\,\s+value\s+is\s+now//; 

		  /instance(\d+)/ && do {
			  my $service=$1;
			  chomp;
			  (my $time, my $inst, my $value)=split(/\s+/,$_);    
			  $t_fin=$time;    
			  push @{$waves[$service]},[$time,$value];
		  };

}
close $SYSCLOG;

if (not $done)  {        
	$t_fin/=1000;
	my $t_fin_round=int($t_fin/50)*50;
	if ($t_fin_round<$t_fin) {
		$t_fin=$t_fin_round+50;
	} else {
		$t_fin=$t_fin_round;
	}
	$done=1;
}

open my $PLOT,'>',"$td_file.gnuplot";
my $gnuplot_header=<< "ENDH";
set terminal svg size 800 560 enhanced fname "Luxi Sans" fsize 11
set output "$td_file.svg.tmp"

set nologscale xy


set title "Gannet DRI SystemC Model: Service Core Status" font "Luxi Sans,14"
set xlabel "Elapsed time (us)" font "Luxi Sans,12"
unset ytics
set ylabel "Service Core Status" font "Luxi Sans,12" offset -8,0
  
ENDH
# looks like the graph area is 80 a.u. high
my $voff=0;
my $vsep=5;
my $ncores=15; # the GW is not a core; we should get ncores from the YAML file

print $PLOT $gnuplot_header;
my @changing_waves=();
my $nwaves=0;

for my $i (0..($ncores-1)) {
	my $j=$i+1;
	my $label=$servicecore_names[$i];
}
for my $i (0..($ncores-1)) {    
	my $changes=0;
	my $j=$i+1;
	my $label=$servicecore_names[$i];
	open my $OUT,'>',"core_status_$i.out";
	my $prev_val=0;
	my $prev_time=0;
	for my $entry (@{$waves[$i]}) {
		my $time=$entry->[0];
		my $val=$entry->[1];        
		if($prev_val<$val) {
			print $OUT "$time\t$prev_val\n";  
			$changes=1;  
		}
		if($prev_val>$val) {
			print $OUT "$prev_time\t$val\n";    
			$changes=1;
		}            
		print $OUT "$time\t$val\n";    
		$prev_time=$time;
		$prev_val=$entry->[1];
	}
	if ($prev_val!=0) {
		print $OUT $t_fin*1000,"\t$prev_val\n";
	}
	print $OUT $t_fin*1000,"\t0\n";    
	close $OUT;

	my $plotline="'core_status_$i.out' using (\$1/1000):(\$2+_YY_". ') title "" with lines lt -1';
	$plotline.=',\\'; 
	if ($changes) {
	    $nwaves++;
		$changing_waves[$i][0]= "set label $j \"$label\" at -1,_YY_ right\n";
		$changing_waves[$i][1]= $plotline."\n"; 
	}
}

for my $ii (0..($ncores-1)) {
	my $i=($ncores-1)-$ii;
	if (defined $changing_waves[$i][0]) {
		$changing_waves[$i][1]=~s/\,\\//;
		last;
	}
	pop @changing_waves;
}

$vsep=80/($nwaves+1);

my $yy=0;
my $y=0;
for my $i (0..($ncores-1)) {
	if (defined $changing_waves[$i][0]) {
		$y++;
		$yy=int($voff+$vsep*$y);
		$changing_waves[$i][0]=~s/_YY_/$yy/;
		print $PLOT $changing_waves[$i][0];
	}
}

$yy=0;
$y=0;
my $first=1;
for my $i (0..($ncores-1)) {
	if (defined $changing_waves[$i][1]) {
		$y++;
		$yy=$voff+$vsep*$y;
		if ($first==1) {
			$first=0;
			print $PLOT 'plot [0:'.$t_fin.'] [2:80] ';
		}
		$changing_waves[$i][1]=~s/_YY_/$yy/;
		print $PLOT $changing_waves[$i][1];
	}
}

close $PLOT;

system("gnuplot $td_file.gnuplot"); 
system("rm -f core_status_*.out"); 
open my $SVG,'<',"$td_file.svg.tmp";
open my $SVGFIX,'>',"$td_file.svg";
while (my $line=<$SVG>) {
	$line=~s/(color:)(.*?)(;\s* stroke:)currentColor/$1$2$3$2/;
	print $SVGFIX $line;
}
close $SVG;
close $SVGFIX;
unlink "$td_file.svg.tmp";
if (`uname -s`=~/Linux/) {

	system("xdg-open $td_file.svg");
} else {
	system("open $td_file.svg");
}

sub get_servicecore_names {
	my $ymlfile=shift;
	my @servicecore_names=();
	if (-e $ymlfile) {
		open my $YMLF,'<',$ymlfile;
		my $services=0;
		my $aliases=0;
		my $stop=0;
		while(<$YMLF>) {
			/^\s*\#/ && next;
			if(/^\s+ServiceInstances:/) {
				$services=1;
				next;
			}
			if(/^\s+Aliases:/) {
				return @servicecore_names;
				$services=0;
				$aliases=1;
				next;
			}
			if (/^\s+ALU_Names:/) {
				$stop=1;
			}

			if ($services) {
				next if /^\s*$/;
#15: { BEGIN: [0, ls_BEGIN, 1], Addr: 0 }
				my $name=$_;
				chomp $name;
				$name=~s/^\s+\d+:\s+\{\s+//; #\}
				my $scname=$name;    
				my $addr=$name;
				$name=~s/:.*$//;
				$scname=~s/^.*?\[\d+\s*\,\s*//;
				$scname=~s/\,.*$//;
				$addr=~s/.*Addr:\s+//;
				$addr=~s/\s+.*$//;            
					$servicecore_names[$addr]=$name;
			}        
			last if $stop;
		}
		close $YMLF;      
	} else {
		warn "WARNING: $ymlfile does not exist!\n";
	}
} 


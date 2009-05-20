package R2N::ServiceConfiguration;

# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  

# $Id: ServiceConfiguration.pm 2534 2009-04-23 11:21:04Z socgroup $
use vars qw( $VERSION );
$VERSION = "1.0.0";

#
#$Id: ServiceConfiguration.pm 2534 2009-04-23 11:21:04Z socgroup $
#

use warnings;
use strict;
#use Perl6::Say;
use Exporter;

@R2N::ServiceConfiguration::ISA = qw(Exporter);

@R2N::ServiceConfiguration::EXPORT = qw(
	&r2n
);


my $wordsz=0;
my $skip=0;
my $enum='';
my $in_enum=0;
my %enums=(
'Symbol_Kinds' => 'K', 
'Symbol_Types' => 'T',
'Packet_Types' => 'P',    
'Core_Status' => 'CS',
'Subtask_Status' => 'STS',
'Data_Status' => 'DS',
'Code_Status' => 'CS',
'RegData_Status' => 'RDS',
'Mode' => 'M',
);

sub r2n {

my $rbfile=shift;
open(IN,"<$rbfile");
for my $line (<IN>) {
	$line=~/\s+\#skip/ && next;
	chomp $line;
	$line=~/^\s*module.*$/ and do {
		print '
#ifndef SERVICE_MGR_CONF_H_
#define SERVICE_MGR_CONF_H_

#include "Base/Types.h" //
#include "./Base/Types.h" // WHY?

using namespace SBA;

namespace SBA {

';	
next;
};
	$line=~/require/ && next;
	$line=~/\=end/ && do {$skip=0;next};
	$line=~/\=begin/ && do {$skip=1;next};
	$line=~/include/ && do {$skip=0;next};

if ($line=~/^\#endskip/) { $skip=0;next}

	next if $skip==1;
	
($line=~/^\#skip/) && ($skip=1) && next;
	$line=~s/^\s*(\w+)\s*=\s*\{/enum $1 {/ && do {
		$enum=$1;
		$in_enum=1;
		$line=~s/Kinds/Kind/;
		$line=~s/Types/Type/;
	};
		if ($in_enum==1) {
		$line=~s/\'(\w+)\'\s*=>\s*/$enums{$enum}_$1=/;
		$line=~s/\}/};/ && ($in_enum=0);
	}
	$line=~s/\#/\/\//;
	$line=~/^\s*[A-Z][A-Z][A-Z0-9_]*\s*=\s*([0-9]+|0x[a-fA-F0-9]+)/ && do {
		if ($line=~/0x/) {
		(my $var, my $val)=split(/\s*=\s*/,$line);
            $val=&hex_r2c($val);
            $var=~s/^\s*/#define /;
            $line="$var $val";
		} else {
	
		$line=~s/^\s*/#define /;
		$line=~s/\=/ /;
		}		  
	};	
	$line=~s/raise/\#error/;
	$line=~/if\s+[A-Z0-9_]+\s*==\s*(\d+)/ && do {		
		$wordsz=$1;
		$line=~s/elsif/\#elif/ or $line=~s/if/\#if/;
	};
	$line=~/(else|end)\s+\/\/\s+[A-Z][A-Z][A-Z0-9_]*/ and do {
	$line=~s/end/\#endif\ \/\// or $line=~s/else/\#else\ \/\// ;
	}; 

if ($line=~/(.+?)\s*\=\s*\[(.*)\](.*$)/) {
my $before=$1;
my $inside=$2;
my $after=$3;
my @nelts=();
if ($inside=~/,/) {
	my @elts=split(/\s*,\s*/,$inside);
	
	for my $elt (@elts) {
		push @nelts,&hex_r2c($elt);
	}
}
	$before=~s/^F/const\ Word\ F/;
	$line=$before.'['.scalar(@nelts).']={'.join(',',@nelts).'};';
} else {
$line=~/0x/ && $line=~/=/ && do {
    (my $var, my $val)=split(/\s*=\s*/,$line);
    $val=&hex_r2c($val);
  $line=$var.'='.$val;
};

$line=~s/^F/const\ Word\ F/;
$line=~/^const\ Word/ && do {
	$line=~s/$/;/;
	$line=~s/\/\//;\/\//;
};
}
print $line,"\n";
} # loop
 print "
typedef Data_Status Argument_Status;
/// The core must return a Gannet data type
typedef Symbol_Type Core_Type;
typedef Symbol_Type Data_Type;
\n";

print "
}
#endif /*SERVICE_MGR_CONF_H_*/
\n"; 
}
 
sub hex_r2c {
	my $val=shift;
	$val=~s/_//g;
    $val=uc($val);
    $val=~s/0X/0x/g;
    $val=~s/\s+$//;
    $val=$val.'UL';
    ($wordsz==64) && ($val=$val.'L');
return $val;
}

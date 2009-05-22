#!/usr/bin/perl

# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  

# $Id: r2n.pl 2547 2009-05-08 11:28:47Z socgroup $
use warnings;
use strict;
use Getopt::Std;
use lib '.', '../../';
use R2N::ServiceConfiguration;
#use Perl6::Say;

my %opts;
getopts( 'HCsShvY:', \%opts );

if ( $opts{'h'} ) {
	die "
	Gannnet Ruby to C++/SystemC translator.
	Outputs to STDOUT.
	$0 [opts] [file.rb]
	-s: static allocation
	-H: create header file
	-CC: create cc file (default)
	-S: generate SystemC code 
	-v: verbose (for debugging)
	-Y: YAML config file 
	\n";
}

my $STATIC_ALLOC=$opts{'s'} ? 1 : 0;
my $H = $opts{'H'} ? 1 : 0;
my $CC= 1-$H;
my $v= $opts{'v'}?1:0;
my $sclib=0;
my $SYSC_SCLIB = $opts{'S'}? 1:0;
my $SYSC = $opts{'S'}? 1:0;
my $sysc_while_fifo='while\s+\(([\.\w]+)fifo\.(size|length)';
my $sysc_rx_fifo='\@sba_tile\.transceiver\.rx_fifo';
my $sysc_tx_fifo='\@sba_tile\.transceiver\.tx_fifo';
my $sysc_ostream="OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<".'": " << ';
my $ymlfile=$opts{'Y'}||'SBA.yml';
my %servicecore_names= get_servicecore_names($ymlfile);

my $switches='NUML|NUMK|NUM|SYM|BYC|REDIR|QUOTEFLD';
my %methods = (
	'addresses'           => 1,
	'aliases'             => 1,
	'alunames'            => 1,
	'as_is'               => 1,
	'by_status'           => 1,
	'check_status'        => 1,
	'collect_lambda_refs' => 1,
	'copy'                => 1,
	'corestatus'          => 1,
	'datastatus'          => 1,
	'get_storage_usage'   => 1,
	'kinds'               => 1,
	'labels'              => 1,
	'occ'                 => 1,
	'packettypes'         => 1,
	'pop'                 => 1,
	'prepare_subtask'     => 1,
	'services'            => 1,
	'show_status '        => 1,
	'size'                => 1,
	'mode'                => 1,
	'nargs'               => 1,
	'subtasks'            => 1,
	'subtaskstatus'       => 1,
	'to_bytecode'         => 1,
	'to_bytes'            => 1,
	'to_float'            => 1,
	'to_float_list'       => 1,
	'to_l'                => 1,
	'to_l_C'              => 1,
	'to_num'              => 1,
	'to_s'                => 1,
	'to_signed_int'       => 1,
	'to_signed_int_list'  => 1,
	'to_symbol'           => 1,
	'to_symbol_list'      => 1,
	'to_word_list'        => 1,
	'to_wordlist'         => 1,
	'to_w_C'              => 1,
	'types'               => 1,
	'utilised'            => 1,
	'utilized'            => 1,
	'shift'               => 1,
	'length'              => 1,
	'Header'              => 1,
	'Payload'             => 1,
	'Type'                => 1,
	'Length'              => 1,
	'Priority'            => 1,
	'To'                  => 1,
	'Send_to'           => 1,
	'Redir'           => 1,
	'Return_to'           => 1,
	'Return_as'           => 1,
	'Kind'                => 1,
	'Datatype'            => 1,
	'Ext'                 => 1,
	'Quoted'              => 1,
	'Task'                => 1,
	'Subtask'             => 1,
	'Name'                => 1,
	'Count'               => 1,

	#	'dup'                 => 1, # not supported!
	#	'new'                 => 1 # new is transformed anyway
);

my %types = ();

my %rubytypes = (
	'Symbol'        => 'SBA_Symbol',
	'Packet_Header' => 'SBA_Packet_Header',
	'Packet'        => 'SBA_Packet',
	'Packet_Label'  => 'SBA_Packet_Label',
	'Packet_Fifo'   => 'SBA_Packet_Fifo',
	'Subtask'       => 'Fixnum',
	'uint'          => 'Fixnum',
	'uint8'         => 'Fixnum',
	'int'           => 'Fixnum',
	'string'        => 'String',
	'uint64'        => 'Integer'              # was Bignum
);

my %listitemtypes = (                         #First we try without '&'
	'Packet_Fifo'  => 'Packet_t',
	'Subtasks'     => 'Subtask',
	'Addresses'    => 'Address',
	'MemAddresses' => 'MemAddress',
	'Word_List'    => 'Word',
	'Symbol_List'  => 'Symbol',
	'Arguments'    => 'Argument',
	'Labels'       => 'Label',
	'Double_List'  => 'double',
	'Packet_List'  => 'Packet_t',
	'Services'     => 'Service',
	'SubtaskRefs'  => 'Subtask',
	'Requests'	   => 'Word',
);

my %snippets = (
	'tile' => "\t" . 'Tile& sba_tile=*(sba_system.instances[address]);' . "\n",
	'system' => "\t" . 'System& sba_system=*((System*)sba_system_ptr);' . "\n",
	'gwtile' => "\t"
	  . 'GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);' . "\n",
	'core' => "\t// Set up context
\tServiceCore* servicecore_ptr=(ServiceCore*)parent_ptr;
\tServiceCore& parent=*servicecore_ptr;
\tSystem* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
\tSystem& sba_system=*sba_system_ptr;
\tTile& sba_tile=*(sba_system.instances[servicecore_ptr->address]);
"."\n"
);

if ($SYSC_SCLIB) {
$snippets{'core'}='    // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    
'."\n";	
}

my $infile = $ARGV[0] or die "Please specifiy input file\n";
if ($infile=~/ServiceConfiguration\.rb/) {
	R2N::ServiceConfiguration::r2n($infile) if $H;
	exit;
}

# I don't think I have to change any file apart from ServiceCoreLibray
# Later we can maybe generate a few more (SystemConfiguration, e.g.)
if ($infile!~/ServiceCoreLibrary\.rb/) {
	$SYSC_SCLIB=0;	
	$sclib=0;
} else {
	$SYSC=0;
    $sclib=1;
}

open( CC, "<$infile" ) or die "Couldn't open $infile for reading\n";

my $hshield = uc($infile);
$hshield =~ s/.*\///;
$hshield =~ s/\.rb/_H_/i;
if ($SYSC_SCLIB) {
$hshield ='SC_'.$hshield;
}

my $class = $infile;
$class =~ s/\..*$//;
$class =~ s/.*\///;
my $module=0;

my $skip             = 0;
my $skipb			= 0;
my $skiph            = 0;
my $skipf=0;
my $skipcc           = 0;
my $skipsc           = 0;
my $skipv            = 0;
my $skip_comments    = 0;
my $block            = 0;
my $skipnum          = 0;
my $skipnuml         = 0;
my %skips            = ();
my $ifdef            = 'NONE';
my %ifdefs=();
my $in_method        = 0;
my $in_method_stored = 0;
my $class_counter    = 0;
my $first            = 1;
my $lastline         = '';
my $switch           = 0;

# -----------------------------------------------------------------------------
#
# Main loop
#
# -----------------------------------------------------------------------------
while (<CC>) {
	my $line = $_;

# -----------------------------------------------------------------------------
# Comments, ifdefs and other special cases
# -----------------------------------------------------------------------------
	print "LINE: skip:$skip,cc:$skipcc,h:$skiph;b:$block;meth:$in_method $line" if $v;
	if ( $line =~ /^\s*$/ and $first==1) {    # first blank line after header
		print "\n// ****** Code generated from $infile by $0 ******\n// ****** DO NOT EDIT (unless you know what you're doing) ******\n\n";
		$skip_comments = 1;
		if ( $H and $first ) {
			print "\n#ifndef $hshield\n#define $hshield\n\n";
		}
		$first = 0;
		next;
	}

	next if $line =~ /\#(\+\+|\-\-|\=\=)/; # for Rubydoc
	
	$lastline = $line; # memory
	
	if (
		not(   $skip == 1
			or ( $H and $skiph == 1 )
			or ( $CC and $skipcc == 1 ) )
	  )
	{    #otherwise go straight to special comments at start of line
	# in here we deal with switches and #ifdefs
	# and also C++ code blocks

		print "HERE:  $skip,$skipcc,$skiph;$block;$in_method $line" if $v;
		if ($line!~/^\s*\#/) {
			# remove blocks with NUM==0 or NUML==0. Tricky!
			if ( $line =~ /($switches)==0/ ) { $skips{$1} = 1; next }
			if ( $line =~ /($switches)==1/ ) { $skips{$1} = 0; next }
        }
        
        # FP handling. Find a better way! Why is FP not a switch?
		next if ( $line =~ /FP==0/ );    
		if ( $line =~ /FP==1/ ) { $skips{'FP'} = 1; next }
		if ( $line =~ /\#\s+FP\s*$/ ) {
			delete $skips{'FP'};
			next;
		}

		my $next = 0;
		for my $key ( keys %skips ) {
			$next = $skips{$key};
			last if $next == 1;
		}
		next if $next == 1;
		if ( $line =~ /\#\s+\b($switches)\b/ ) {
			$skips{$1} = 0;
			next;
		}
		
		print "HERE2:  $skip,$skipcc,$skiph;$block;$in_method $line" if $v;		
		
		# convert if CONST... to #if
		if ( $line !~/^\s*\#if/ and $line =~ /\bif\s+([A-Z_]+)\s*==/ ) {
			$ifdef = $1;
			$ifdefs{$ifdef}=1;
			$line =~ s/^\s*/\#/;
			print $line; # if CONST
			next;
		} elsif ( $line =~ /\b(end|else)\s+\#\s+([A-Z_]+)/ and exists $ifdefs{$2} ) {
			if ( $1 eq 'end' ) {
				$line =~ s/end/endif/;
				$ifdef = 'NONE';
			}
			$line =~ s/\#/\/\//;
			$line =~ s/^\s*/\#/;
			print $line; # else // CONST
			next;
		} 

		# Special rdoc blocks used as C++ code blocks
		if ( $line =~ /^\=end\s+\#/ ) {
			$block     = 0;
			$in_method = $in_method_stored;
			next;
		}
#		$line=~s/\#\=(begin|end)/\=$1/; # Stupid hack to account for bug in Eclipse RDT on Ubuntu
		if ( $line =~ /^\=begin\s+\#/ ) {
			$block            = 1;
			$in_method_stored = $in_method;
			next;
		}

		if ( $block == 1 ) {
			next if $skip == 1;		
			next if ( $H and $skiph == 1 );
			next if ( not $H and $skipcc == 1 );
			if ($H) {
				next if $in_method;
				if ( $line =~ /\/\/H/ ) {
					$line =~ s/\/\/H//;
					$in_method = 1;
					$line =~ s/$class\:\://;
					$line =~ s/\s*\{\s*$/;\n/; #} keep EPIC happy
				}
				$line =~ s/\/\/skipcc//;
				print $line unless $line =~ /\/\/skiph/;
			} else {
				$line =~ s/\/\/skiph//;
				print $line unless $line =~ /\/\/skipcc/;
			}
			next;
		}
	}

 #	print "HERE3:  $skip,$skipcc,$skiph;$block;$in_method $line";
 # -----------------------------------------------------------------------------
 # Special comments at start of line
 # currently, we have:
 # shield/endshield
 # skip/endskip
 # skipcc/endskipcc
 # skiph/endskiph
 # C++
 # H
 # last
 # Ruby #-- and #++

	if ( $line =~ /^(\s*)\#(\w+\S*)/ ) {
		my $wsb    = $1;
		my $marker = $2;
		if ( $marker eq '++' or $marker eq '--' ) {
			next;
		} elsif ( $marker eq 'skip' ) {
			$skip = 1;
		} elsif ( $marker eq 'endskip' ) {
			$skip = 0;
			next;
		} elsif ( $marker eq 'skipcc' ) {
			if ($CC) { $skipcc = 1 }
			else { next }
		} elsif ( $marker eq 'endskipcc' ) {
			if ( $CC ) { $skipcc = 0; next }
			else { next; }
		} elsif ( $marker eq 'skiph' ) {
			if ($H) { $skiph = 1 }
			else { next; }
		} elsif ( $marker eq 'endskiph' ) {
			if ($H) { $skiph = 0; next }
			else { next; }			
		} elsif ( $marker eq 'C++' ) { # this line has C++ code
			if ( ( $skip == 1 ) or ( $skipv == 1 )) {
				next;
			}
			$line =~ s/\#C\+\+//;
			if ( $line =~ /\)\s*\{/ and $line !~ /\b(if|while|for|class)\b/ )
			{    # guess it's a function
				$in_method = 1;
				if ($H) {
					$line =~ s/$class\:\://;
					$line =~ s/\s*\{\s*$/;\n/;
					print $line ; # C++ function signature
				}
			}
			print $line unless ( $H and $in_method );
			next;
        } elsif ( $marker eq 'sysc') { # this line has SystemC code
            if ( !$SYSC or $H or ( $skip == 1 ) or ( $skipv == 1 )) {
                next;
            }
            $line =~ s/\#sysc//;
            print $line;
            next;			
		} elsif ( $marker eq 'shield' ) {
			if ($H and not ($skip  or $skiph)) {
				print "#ifndef $hshield\n#define $hshield\n\n";
			}
			next;
		} elsif ( $marker eq 'endshield' ) {
			if ($H and not ($skip  or $skiph)) {
				print "#endif // $hshield\n";
			}
			next;
		} elsif ( $marker eq 'tile' ) {
			if ( not $SYSC and $CC and not ($skip  or $skipcc)) {
				if ( $class ne 'Gateway' ) {
					print $wsb. $snippets{'system'};
					print $wsb. $snippets{'tile'};
				} else {
					print $wsb. $snippets{'gwtile'};
				}
			}
			next;
		} elsif ( $marker eq 'system' ) {
			if ( not $SYSC and $CC and not ($skip  or $skipcc)) {
				print $wsb. $snippets{'system'};
			}
			next;
		} elsif ( $marker eq 'core' ) {
			if ( not $SYSC and $CC and not ($skip  or $skipcc)) {
				print $wsb. $snippets{'core'};
			}
			next;
		} elsif ( $marker eq 'last' ) {
			last;
		} elsif ( $marker eq 'H' ) {

			#			print $skip,$in_method,$line;
			next if $skip == 1;
			next if ( $CC and $skipcc == 1 );
			next if ( $H and $skiph == 1 );

			$line =~ s/\#H//;
			if ($H) {
				print $line unless $in_method;
			}
			next;
		} elsif ( $marker =~/if|else|include|define/ ) { # cpp statements
			if (not ($skip or $skipcc or $skiph)) { 
				print $line; # preprocessor lines 	
			}
				next;
		}

	}

 # -----------------------------------------------------------------------------

	if ( $skip == 1 
		or ( ( $H and $skiph == 1 ) or ( not $H and $skipcc == 1 ) ) )
	{
			if ( $line =~ /^\=end/ ) {
		$skipb = 0;
			}
		next;
	}

	#print "SKIP: $skip: $line\n";



	next if $skip == 1;
	next if ( not $H and $skipcc == 1 );
	next if ( $H and $skiph == 1 );

# line-by-line skips
	next if $line =~ /^.+\#skip\b/;
#	next if $line =~ /^.+\#skipcc\b/ and not $H;
    if ($line =~ /^.+\#skipcc\b/) {
        if (not $H) {
            next;
        } else {
            $line=~s/\#skipcc.*//;
        }
    }	
	if ($line =~ /^.+\#skiph\b/) {
		if ($H) {
			next;
		} else {
			$line=~s/\#skiph.*//;
		}
	}
	if ($line =~ /^.+\#skipsysc\b/){
		if( $SYSC) {
			next;
		} else {
			$line=~s/\#skipsysc.*//;
		}
	}
	/^\s*\#+\s+/ && ( $skip_comments == 1 ) && next;
	next if $line =~ /^\s*include\s/;

 # -----------------------------------------------------------------------------
 # #C++
	if ( $line =~ s/^(\s*).*?\#C\+\+/$1/ ) { # replace Ruby code by this C++ code
		if ($H) {			
#			next if $in_method;
			if ( $line =~ /\)\s*\{/ and $line !~ /\b(if|while|for|class)\b/ )
			{    # guess it's a function
				$in_method = 1;
				$line =~ s/$class\:\://;
				$line =~ s/\s*\{\s*$/;\n/;
				print $line;
			} elsif ($line =~ /\b(if|while|for|class)\b/ ) {
				if ($in_method==0) {
					$in_method = 1;
				} else {
					$in_method++;
				}			
			}
		}
		if ($SYSC) {
			# ad hoc for SystemC
			if ($line=~/\.front.*?pop_front.*$/) {
				$line=~s/\.front.*$/.shift();/;				 
			}		
			$line=~s/sba_tile\.//g;
		}
		print $line unless ( ( $skipv == 1 ) or ( $H and $in_method ) );
		next;
	}

 # -----------------------------------------------------------------------------
 # #sysc
 if ($line =~ /^(\s*).*?\#sysc/ and $line!~/puts|print|cout/) {
    if ( $SYSC and $line =~ s/^(\s*).*?\#sysc/$1/ ) { # replace Ruby code by this systemc code
        print $line unless ( $skipv == 1 );
        next;
    } else {
    	$line =~ s/\#sysc.*//;
    }
 }

 # -----------------------------------------------------------------------------
 # #H
	if ( $line =~ s/^(\s*).*?\#H/$1/ and $H ) {
		print $line unless $in_method;
		next;
	}

 # -----------------------------------------------------------------------------
 # Special comment for substitution
	if ( $line =~ /\#s(?!ysc)/ ) {
		chomp $line;
		my @parts     = split( /\#/, $line );   # blabla |s/blob|t blab |s/blib|
		my @substs    = ();
		my @lineparts = ();
		
		for my $part (@parts) {
#			print "PART: $part\n";
			if ( $part =~ /^s\// ) {
				$part =~ s/\s*$//;
#				print "SUBST:<$part>\n";
				push @substs, $part;
			} else {
				push @lineparts, $part;
			}
		}
		$line = shift @lineparts;

   # a line can contain '#' if it's #{...}
   # In principle of course it could be '#' inside a string, but we ignore that.
		for my $linepart (@lineparts) {
			if ( $linepart =~ /^\{/ ) {
				$line .= "#$linepart";
				shift @lineparts;
			}
		}
#		print "LB:$line\n";
		my $rest = join( '#', @lineparts );    # anything else
#		print "REST:$rest\n";
		for my $subst (@substs) {
			$subst =~ s/\s*$//;
			( my $s, my $old, my $new, my $g ) = split( /\//, $subst );
			if ($g) {
				$line =~ s/$old/$new/g;
			} else {
				$line =~ s/$old/$new/;
			}
		}
#		print "LA:$line\n";
		$line .= " #$rest\n";
#		print "LF: $line\n";
	}

 # -----------------------------------------------------------------------------
 # VERBOSE
	if ( $line =~ /^\s*#iv/ ) {

		print "#ifdef VERBOSE\n" unless $H;
		if ($H ){ $skipv = 1;}
		next;
	} elsif ( $line =~ /^(\s*)#ev/ ) {

		print "#endif // VERBOSE\n" unless $H;
		$skipv = 0;
		next;
	}
#	next if $skipv == 1;

 # -----------------------------------------------------------------------------
	$line !~ /^\s*(\/\/|\#)/ and $line =~ /\b(if|for|while|case)\b/ and $in_method++;

 # -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Line-by-line Processing
# -----------------------------------------------------------------------------

 
 # string transformations
	if ( $line =~ /\#\{/ ) {
		my $cmd='';
		if($line=~/(puts|print)/) {
			$cmd=$1;
		}
		$line =~ s/\#\{/\" <</g; # do this first to avoid problems with comments
		$line =~ s/\}/<< \"/g;
		$line =~s/\}\s*\,/} COMMA/g;		
		if ($cmd eq 'puts') {
			#warn "1:$line\n" if $line=~/Mode:\ CACHE:\ reg.*mode/;
		$line =~ s/([^\<]{2}\s*)\"\s*\,/$1\" << \"\\n\" << /g;
			#warn "2:$line\n" if $line=~/Mode:\ CACHE:\ reg.*mode/;
		$line =~ s/\,\s*\"/ << \"\\n/g;
			#warn "3:$line\n" if $line=~/Mode:\ CACHE:\ reg.*mode/;
		} else {
		$line =~ s/\"\s*\,/\" << /g;
		$line =~ s/\,\s*\"/ << \"/g;
		}
		$line =~s/COMMA/,/g;		
	}

#		print "BEFORE TYPES: $line\n";
 # -----------------------------------------------------------------------------
 # Type annotations
	if ( $line !~ /^\s*(for|while)\b/ ) {   # for and while are treated separately
		my $marker  = 'NONE';
		my $argtype = 'NONE';
		my $def     = 0;
		if (    $line !~ /\bdef\b/
			and $line =~ /\#t\s+([\w\<\,\>\s\&\*\.]+)\s+.*$/ )
		{
			$marker = $1;
		} elsif ( $line !~ /^\s*\#/ and $line =~ /[\s\,]\:\w+/ )
		{                                 #attribute accessor lines
			next unless $H;
			$line =~ s/\s+attr_\w+\s+//;
			
			$line =~ s/\,\s+\#/\#/;
			( my $varstr, my $typestr ) = split( /\#t\s+/, $line );
			my @vars = split( /\s*\,\s*/, $varstr );
			chomp $typestr;
			$typestr =~ s/;\s*$//;
			my @types = split( /;/, $typestr );

			for my $var (@vars) {
				$var =~ s/\://;
				my $type = shift @types;
				print "\t$type $var;\n";
			}
			next;
		} elsif ( $line =~ /\bdef\b/
			and $line =~
/\)\s+\#t\s+([\w\<\>\s\&\*\.\,]+)*\s*(\(([\w\<\>\s\&\*\.\,\;]+)\))*.*$/
		  )
		{
			#			print "HERE NO DAMAGE : $line\n";
			$def     = 1;
			$marker  = $1 || '';
			$argtype = $3;
			my $argline = $line;
	            if ($sclib and $line=~/MemAddresses/) {	            	
	            	my $name=$line;
	            	chomp $name;
	            	$name=~s/^\s*def\s+SBA_SCLib\.//;
	            	$name=~s/\s*\(.*$//;
	            	if ( exists $servicecore_names{$name}) {
	            		if ($skipf==1) {
	            			$skipf=0;
	            		print "#endif // 0\n";
	            		}	
	            		#print "//NAME:<$name>\n";	            			            		
	            	} else {
	            		if ($skipf==1) {
                        print "#endif // 0\n";
                        } else {
                        	$skipf=1;
                        }   
	            		print "#if 0 // SKIP $name\n";
	            			            		
	            	}			            	
            }
			
			$argline =~ s/^.*?\(//;
			$argline =~ s/[\)\(]//g;
			( my $varstr, my $typestr ) = split( /\s+\#t\s+/, $argline );

#print "varstr: $varstr, typestr: $typestr, marker: $marker, argtype: $argtype\n";
			my @vars = split( /\s*\,\s*/, $varstr );
			if ( $varstr !~ /\,/ and $varstr =~ /\w+/ ) {
				$vars[0] = $varstr;
			}

			chomp $typestr;
			$typestr =~ s/^\s*//;
			$typestr =~ s/\s*$//;
			$typestr =~ s/.*?\s//;

			#			print "typestr: $typestr\n";
			my @types  = split( /;/, $typestr );
			my $argstr = '(';

			#print join(';',@vars),"\n",join(';',@types),"\n";
			for my $var (@vars) {
				my $type = shift @types;
				if ($SYSC_SCLIB) {
					if($type eq 'Base::ServiceCore*') {
						$type='void*';
					}						
				}
				if ( $H && ( $line !~ /initialize/ ) ) {
					$var =~ /sba_(tile|system)/ && next;
					$argstr .= "$type,";
				} else {
					if ( $var =~ /sba_(tile|system)/ ) {
						$argstr .= "$var,";
					} else {
						$argstr .= "$type $var,";
					}
				}
			}

			#			print "argstr:$argstr\n";
			$argstr =~ s/\,$/\)/;
			if ( $argstr eq '(' ) { $argstr = '()' }
			my $defline = $line;
			$defline =~ s/\(.*$//;

			#$defline=~s/\bdef\s+//;
			chomp $defline;

			#			$line="$marker $defline$argstr;";
			$line = "$defline$argstr";

			#			print "HERE DAMAGE? : $line\n";
			if ($H) { $line .= ';'; }
			$argtype = 'NONE';
		} elsif ( $line =~ /\bdef\b/
			and $line =~
			/\)\s+\#t\s+([\w\<\>\s\&\*\.]+)\s+(\(([\w\<\>\s\&\*\.]+)\))*.*$/ )
		{
			$def     = 1;
			$marker  = $1;
			$argtype = $3;
		} elsif ( $line =~ /\bdef\b/
			and $line =~ /\#t\s+([\w\<\>\s\&\*\.]+)\s+.*$/ )
		{
			$def    = 1;
			$marker = $1;
		}

		#		print "DAMAGE DONE?: $line\n";
		chomp $marker;

		#	$marker=~s/\&/_amp/;
		if ( $marker ne 'NONE' ) {
			$types{$marker} = $marker;
			$line =~ s/\#t\s+$marker//;

			#	$line=~s/\=\s*\[\s*\]//;
			#	$line=~s/\=\s*\{\s*\}//;
			$line =~ s/\b/$marker\ /;
		}

		if ( $def == 1 ) {

			if ( not $H ) {

				#				$line =~ s/$marker/$marker\ $class\:\:/;
				$line =~ s/\:\:\s+/::/;
			}
			if ( $argtype ne 'NONE' ) {
				$line =~ s/\(/\($argtype\ /;
				$line =~ s/\s*\($argtype\)//;
			}
		}

	}    # end of type annotations

 #		print "AFTER TYPES: $line\n";

 # Handle trailing comments   
	my $line_without_comments = $line;
	my $tmp_comments          = '';
	if ( $line =~ /(\#[^\{]*)$/ ) {
		$tmp_comments = $1;
		$line_without_comments =~ s/(\#[^\{]*)$//;
	}
 #ÊIf the line contains a method call, add (). Uses %methods to decide	
	if ( $line_without_comments =~ /[^\.]\.\w+/ ) {    # might be method calls
		my @maybe_method_calls       = split( /\./, $line_without_comments );
		my $before                   = shift @maybe_method_calls;
		my @method_calls_with_parens = ();
		for my $maybe_method_call (@maybe_method_calls) {
			if ( $maybe_method_call !~ /^\w+\(/ ) {
				my $maybe_method_call_cleaned = '';
				if ( $maybe_method_call =~ /(\w+)/ ) {
					$maybe_method_call_cleaned = $1;
				}

				#			print "CALL? <$maybe_method_call_cleaned>\n";
				my $rest = $maybe_method_call;
				$rest =~ s/^$maybe_method_call_cleaned//;
				if ( exists $methods{$maybe_method_call_cleaned} ) {
					$maybe_method_call =
					  $maybe_method_call_cleaned . '()' . $rest;
				}
			}
			push @method_calls_with_parens, $maybe_method_call;
		}
		my $line_with_parens = join( '.', @method_calls_with_parens );
		chomp $line_with_parens;
		$line = "$before\.$line_with_parens$tmp_comments";
		if ( $line !~ /\n/ ) { $line = $line . "\n" }
		# Special case for SystemC
		if ($SYSC) {
            if ($line=~/$sysc_rx_fifo/) {
                $line=~s/$sysc_rx_fifo/transceiver_rx_fifo/;
            }
            if ($line=~/$sysc_tx_fifo/) {
                $line=~s/$sysc_tx_fifo/tx_fifo/;
            }
            
		}
		
	} 

 # -----------------------------------------------------------------------------
 # transform object constructor calls
 # use class name as type (obviously)
	$line =~ s/^(\s*)(.*?)\s*=\s*(\w+)\.new\b/$1$3 $2/;    #a=C.new() => C a()

	$line =~ s/^(\s*)(.*?)\s*=\s*mk(\w+)/$1${3}_t $2 = mk$3/;    #a=mkC() => C_t a=mkC()
 # -----------------------------------------------------------------------------
 # check for end-of-class comment
	next if $line =~ /^\s*end\s+\#.*$class/;

 # -----------------------------------------------------------------------------
 #line-comments
 #full line
 	$line =~ s/^\s*\#([\/\*\sWTF])/\/\/$1/g;
	if ( $skip_comments == 1 and $line!~/end\s+\#\s+of\s+/) {
		$line =~ s/\#([\s\*\/WTF]).*$// unless $line =~ /cout/;
	} else {
		if ($line!~/end\s+\#\s+of\s+/) {
		$line =~ s/\#([\s\*\/WTF])/\/\/$1/;
		} else {
		$line =~ s/\#/\/\//;
		}
	}
	if ( $line =~ /^\s*\/\// ) {
		print $line unless $skip_comments == 1;
		next;
	}
 # -----------------------------------------------------------------------------
 # C-style block comments

 # -----------------------------------------------------------------------------
 #block-comments
	if ( $line =~ /^\=begin\s*$/ ) {
		$skipb = 1;
	}
		if ( $line =~ /^\=end\s*$/ ) {
		$skipb = 0;
		next;
	}
	next if $skipb == 1;

 # -----------------------------------------------------------------------------
 # skip "require" and "include" statements
	next if $line =~ /^\s*(require|include)\s+/;

 # -----------------------------------------------------------------------------
 #	$line =~ s/require/\#include/;
 #	if ( $line =~ /include/ ) {
 #		$line =~ s/\.rb/.h/;
 #		$line =~ s/SBA\///;    #AD HOC!
 #	}
 # -----------------------------------------------------------------------------
 # @
 #	$line=~s/\@/attr_/g;
	$line =~ s/\@//g;
	if ($SYSC) {
		$line=~s/sba_tile\.//g;
		$line=~s/gw_tile\.//g;
		# for ServiceCore
		$line=~s/service_mananger\.//g;		
	}

 # -----------------------------------------------------------------------------
 # ? in method names
	$line =~ s/(\w)\?\(/$1\(/g;

 # -----------------------------------------------------------------------------
 # end
	my $end = 0;
	if ( $line =~ /\bend\b/ and $line!~/print|puts/) {		
		$line =~ s/\bend\b/}/g;
		$end = 1;
		$in_method--;
	}
	$in_method < 0 and $in_method = 0;

	#	print "LINE:$line";
	next if $end == 1 and $in_method == 0 and $H;

 # -----------------------------------------------------------------------------
	$line =~ s/\#\;/;/;    # in case we need an extra ;

 # -----------------------------------------------------------------------------
 # if
	$line =~ s/^(\s*(els)*if)\ /$1\ \(/;

	if ( $line =~ /if\s*\(/ ) {
		/^\s*\/\// && next;
		if ( not $line =~ /if.*?\/\// ) {
			$line =~ s/$/\)\{/;
		} else {
			$line =~ s/\/\//\)\{\/\//;
		}
	}

	$line =~ s/else/\}\ else\ \{/;
	$line =~ s/elsif/\}\ else\ if/;

 # -----------------------------------------------------------------------------
 # case/switch
	if ( $line =~ /^(\s*)case\s+(.*)/ ) {
		my $ws        = $1;
		my $predicate = $2;
		$line   = "$ws switch ($predicate) {\n";
		$switch = 1;
	}
	if ( $line =~ /^(\s*)when\s+(.*)/ ) {
		my $ws      = $1;
		my $predval = $2;
		$line = "$ws case $predval :\n$ws {\n";
		if ( $switch != 1 ) {
			$line = "$ws  break;\n$ws }\n" . $line;
		}
		$switch = 0;
	}

 # -----------------------------------------------------------------------------
 #while
	if ( $line =~ s/^(\s*while)\ /$1 \(/ ) {		
		$line =~ /^\s*\/\// and next;		
		if ($SYSC and $line=~/^(\s+)$sysc_while_fifo/) {			
			$line=$1.'while (true) {'."\n";		
		} else {		
			if ( not $line =~ /while.*?\/\// ) {
				$line =~ s/$/\)\{/;
			} else {
				$line =~ s/\/\//\)\{\/\//;
			}
		}
	}

 # -----------------------------------------------------------------------------
 # replace for ... in loops
	if ( $line =~ /^(\s*)for\s+(\w+)\s+in\s+/ ) {

		#$line=~s/\#.*$//;
		my $ws   = $1;
		my $item = $2; # this can be a range! e.g. 0..length()-1
		my $type = 'TYPE';

		# Type annotations
		if ( $line =~ /\#t\s+([\w\<\,\>\s\&\*\.]+)\s+.*$/ ) {
			$type = $1;
			$type =~ s/\s+$//;
			chomp $type;

			#	$type=~s/\&/_amp/;
			$types{$type} = $type;
			$line =~ s/\#.*//;
		}
		my $list = '';
		if ( $line !~ /\.\./ ) {
			$line =~ /in\s+([\w\]\[]+)(\.keys)*\s*$/ && ( $list = $1 );
			my $itemtype = 'TYPE';
			if ( exists $listitemtypes{$type} ) {
				$itemtype = $listitemtypes{$type};
			} elsif ( $type =~ /\<(\S+)\>/ ) {
				$itemtype = $1;
			}
			if ( $type !~ /[Mm]ap/ ) {
				if (!$STATIC_ALLOC) {
				$line =
"${ws}for($type\::iterator iter_=$list.begin();iter_!=$list.end();iter_++) {\n${ws}\t$itemtype $item=*iter_;\n";
				} else {
# ${ws}unsigned int size_$unique=$list.size()\n;${ws}for(unsigned int iter_=0;iter_<size_$unique;iter_++) {\n${ws}\t$itemtype $item=$list.back();\n${ws}\t$list.pop_back();\n";

$line =
"${ws}for(unsigned int iter_=0;iter_<$list.size();iter_++) {\n${ws}\t$itemtype $item=${list}[iter_]\n"; #.back();\n${ws}\t$list.pop_back();\n";
				}
			} else {
				my $keytype = $itemtype;
				$keytype =~ s/\,.*//;
				$line =
"${ws}for($type\::iterator iter_=$list.begin();iter_!=$list.end();iter_++) {\n${ws}\t$keytype $item=iter_->first;\n";

			}
			if ( $list eq 'self' ) {
				$line =~ s/$list\./this->/g;
			}
		} else { # range
			my $from = 'START';
			my $to   = 'STOP';
			chomp $line;

			$line =~ /in\s+(.+)\s*\.\.(.+)\s*$/ and do {
			  $from = $1;
			  $to   = $2;			  
			};
			$line = "${ws}for($type $item=$from;$item<=$to;$item++) {\n";
			
		}
	}

 # -----------------------------------------------------------------------------
 #class
	if ( $line =~ /^\s*(class|module)\s+(\w+)/ ) {  
		my $prevclass       = $class;
		my $class_or_module = $1;
		$class = $2;
		$class =~ s/SBA_//;

		#inheritance. use < or 'p' (parent)
		if ( $line =~ /\#[\<p]\s+/ ) {
			$line =~ s/\#./:/;
		}
		$line =~ s/$/ \{/;
		if ( $class_or_module eq 'module' ) {
			$line="\n";
			$line = "namespace $class {\n" unless $class eq 'SBA'; # a hack really
			$module=1;
			 }
		if ( $class_counter > 0 ) {
			$line = "}; // $prevclass\n\n" . $line;
			$class_counter++;
		} else {

			if ($H) {
				# first class, add namespace
				if (not $SYSC_SCLIB) {
				$line = "using namespace std;\n\nnamespace SBA {\n" . $line;
				} else {
				$line = <<ENDLH; 
#include "SC_sba.h"
			
using namespace std;

namespace SC_SBA {
#include "SC_ServiceCore_signature.h"
$line

ENDLH
				}
			} else {
				# first class, add using namespace
				if (not $SYSC_SCLIB) {				
				$line = "using namespace std;\nusing namespace SBA;\n";
				} else {
				$line = <<ENDLCC;				
using namespace std;
using namespace SC_SBA;
				
#define	FIXME_SERV_MGR 0
				
ENDLCC
				}
			}
		}
	}

 # -----------------------------------------------------------------------------
 #self
 # self. maps to this->
	$line =~ s/\bself\./this->/g;

 # -----------------------------------------------------------------------------
 #def
	if ( $line =~ /^\s*(.+)*\s*def\s+/ ) {

		$in_method = 1 unless $line =~ /initialize/;
		my $type = $1 || 'NONE';

		# def can be qualified using module name. Remove qualifier
		# we add the $class:: anyway.
		$line =~ s/def\s+\w+?\./def /;
		if ( $line =~ /\bdef\s+(\w+)(\s|$)/ ) {
			my $f = $1;
			$line =~ s/$f/$f()/;
		} elsif ( $H and $line =~ /initialize/ ) {			
			$line =~ s/\S*\s+def\s+initialize/ $class/;
			print $line, "\n";
			next;
		}

		if ( not $H ) {
			if ( $type ne 'NONE' ) {
				$line =~ s/\bdef\s+/\ $class\:\:/;
			} else {
				$line =~ s/\bdef\b/void\ $class\:\:/;
			}
		} else {
			if ( $type ne 'NONE' ) {
				$line =~ s/\bdef\s+/ /;
			} else {
				$line =~ s/\bdef\b/void\ /;
			}
		}
		$line =~ s/\:\:\s+/::/;
		$line =~ /^\s*\/\// && next;
		$line =~ s/\s+/\ /g;           # stupid beautifier!
		if ( not $H ) {
			if ( not $line =~ /(void|$type).*?\/\// ) {
				$line =~ s/$/ \{\n/;
				$line = "\n" . $line;
			} else {
				$line =~ s/\/\// \{\ \/\//;
			}
		}

		# ad-hocs

		if ($H) {
			$line =~ s/sba_system\,*//;
			$line =~ s/sba_tile\,*//;
			$line =~ s/\(\s*\,\s*\)/()/;
			$line =~ s/\)\s*$/);\n/;
			print $line, "\n";
			next;
		}
	}

 # -----------------------------------------------------------------------------
 # ad-hocs
 if ($line!~/SBA_[A-Z][A-Z_]+/) { 
	$line =~ s/SBA_//g; # Assumes only a single macro on a line 
 }
	# C++ has no exponetiatin operator. Use << for integer powers of 2
	$line =~ s/2\s*\*\*\s*(\d+)/\(1<<$1\)/g;

	# STL always uses size
	$line =~ s/\.length/\.size/g;
	# STL always uses push_back -- NOT! This introduces a segfault in List<>
	# $line =~ s/\.push/\.push_back/g;
	
	# C++ copies by value by default
	$line =~ s/\.dup\b//;
	
	# Ruby requires that lists are initialised to []; C++ doesn't
	$line=~s/\=\s*\[\]//;
	# References to system&tile	are replaced by pointers to base classes in C++
	if ( $line =~ /sba_(system|tile)/ ) {

		#		my $both    = 0;
		#		my $pointer = $1;
		#		if ( $line =~ /sba_system/ and $line =~ /sba_tile/ ) {
		#			$both = 1;
		#		} elsif ( $line =~ /sba_system/ ) {
		#			$both    = 0;
		#			$pointer = 'system';
		#		} else {
		#			$both    = 2;
		#			$pointer = 'tile';
		#		}
		#
		#		if ( $class =~ /Gateway\b/ and $pointer eq 'tile' ) {
		#			$pointer = 'gwtile';
		#		}

		$line =~ s/sba_system\s*,//;
		$line =~ s/sba_tile\s*,//;
		$line =~ s/sba_system\s*\)/)/;
		$line =~ s/sba_tile\s*\)/)/;

	}
 
 # -----------------------------------------------------------------------------
 # substitution in puts/print
	if ( $line =~ /^\s*(puts|print|raise)\s+/ and $line !~ /^\s*\#\s/ ) {
		
		my $cmd=$1;
		$line =~ s/puts\s/cout << /
		  && ( $line =~ s/;*\s*\#\s+.*$/<<endl/ # any normal trailing comment is replaced by << endl
		  or $line =~ s/;*(\#\w+)\s*$/<<endl; $1/ # any special trailing comment is replaced by << endl #comment
		  or $line =~ s/;*\s*$/<<endl/); # any en -of-line is replaced by << endl
		$line =~ s/print\s/cout << /;
		$line =~ s/raise\s/cerr << /;
		$line =~ s/\#\{/\" <</g; # do this first to avoid problems with comments
		$line =~ s/\}/<< \"/g;
		$line =~s/\}\s*\,/} COMMA/g;			
		if ($cmd eq 'print') {
			$line =~ s/\"\s*\,/\" << /g;
			$line =~ s/\,\s*\"/ << \"/g;
		} elsif ($cmd eq 'puts') {		
			$line =~ s/([^\<]{2}\s*)\"\s*\,/$1\" << \"\\n\" << /g;
			$line =~ s/\,\s*\"/ << \"\\n/g; 	
		}							
		$line =~ s/\s+\#\s+.*$//;
		$line =~ s/\s*$//;
		$line =~ s/$/;\n/;
		$line =~ s/;+$/;/;
		$line =~ s/\@//g;
		$line =~s/COMMA/,/g;
		if ($line=~/#sysc/) {
			$line=~s/\#sysc.*$//;
			if ($SYSC) {
			$line=~s/cout\s+\<\<\s+/$sysc_ostream/;
			}
		}
#	} elsif (	$line =~ /^\s*\#\s+.+$/ ) {
#		$line=~s/\#/\/\/ COMMENT/;
	} else {

 # -----------------------------------------------------------------------------
 # ; at end of statement
 # remove any trailing comments
		my $trail_comment = '';
		if ( $line =~ /(\s*\/\/.*)$/ ) {
			$trail_comment = $1;			
			$line =~ s/\s*\/\/.*//;
			chomp $trail_comment;
		}
	
		if (
			not(

				#	line is blank or comment-only
				$line =~ /^\s*($|\/\/)/ or

				#	line ends with ; or }
				$line =~ /[\;\}]\s*$/ or

				#	line ends with [\{,\:]
				$line =~ /[\{,\:]\s*$/ or

				#	line contains a keyword
				$line =~
/\b(else|switch|case|default|public|private|protected|template|namespace|catch|try)\b/
				or (    $line =~ /\b(union|enum|struct|class)\b/
					and $line !~ /\}\s*$/ )
				or ( $line =~ /\bwhile\b/ and $line =~ /\{\s*$/ )
				or

				#	line starts with #
				$line =~ /^\s*\#/
			)
		  )
		{

			$line =~ s/\s*$/\;/;

			$line = $line . $trail_comment . "\n";			
		}
		
		if ($trail_comment=~/\/\/\s+of\s+/) {
			chomp $line;
			$line.=$trail_comment . "\n";	
		}

	}

 # -----------------------------------------------------------------------------
 #	print $in_method, ": $line";
	if ($SYSC_SCLIB) { 
		
		$line=~s/sba_tile\.(transceiver)\./parent.$1\_/;
		if ($line=~/sba_tile\.service_manager/) {
			if ($line=~/sba_tile\.service_manager\.(subtask_list|symbol_table|subtask_reference_fifo|data_address_stack)/) {
				$line=~s/sba_tile\.service_manager\.(subtask_list|symbol_table|subtask_reference_fifo|data_address_stack)/parent.$1/;
			} else {
				if ($line !~/^\s*if/) {
					if ($line=~/^(.*?)\s*\=\s*(sba_tile\.service_manager.+)$/) {
						$line	= "$1 = 0; // FIXME_SERV_MGR // $2\n";					
					} else {
						$line = '// FIXME_SERV_MGR // '.$line;
					}
				} else {
					$line=~s/if\s*\((.+)\)/if ( FIXME_SERV_MGR )/;
						$line.='//'.$1."\n";
				}
			}
		}			
	} 
	print $line unless ( $H and $in_method );

	#more ad-hocs
	$line =~ /^(\s*)cerr/ && !$H && print "${1}exit(1);\n";
	$line =~ /\bclass\b/ && print "\tpublic:\n";

}

# -----------------------------------------------------------------------------
# end of last class, namespace, shield
if ($H) {
	if ( $lastline =~ /^\s*end/ ) {
		print "}; // $class\n" unless $class eq 'SBA'; # a hack, again
		if (not $SYSC_SCLIB) {
			print "} // namespace SBA\n";
		} else {
			print "} // namespace SC_SBA\n#include \"SC_ServiceCoreLibrary.cc\"";
			
		}
	}
	print "
#endif // $hshield
";
}

sub get_servicecore_names {
	my $ymlfile=shift;
    my %servicenames=();
      $servicenames{'sba_GATEWAY'}=['GATEWAY'];
    $servicenames{'none'}=['none']; 
	if (-e $ymlfile) {
    open my $YMLF,'<',$ymlfile;

#	my %aliasnames=();
#	my %servicecorenames=();
#	$servicecorenames{'sba_GATEWAY'}='sba_GATEWAY';
#	$servicecorenames{'none'}='none';
	my $services=0;
	my $aliases=0;
	
	while(<$YMLF>) {
		/^\s*\#/ && next;
	    if(/^\s+Services:/) {
	        $services=1;
	        next;
	    }
	    if(/^\s+Aliases:/) {
	    	return %servicenames;
	        $services=0;
	        $aliases=1;
	        next;
	    }
	    if (/^\s+ALU_Names:/) {
	    	#
	    }
	    
	    if ($services) {
#15: { BEGIN: [0, ls_BEGIN, 1], Addr: 0 } 
            my $name=$_;            
            chomp $name;            
            $name=~s/^\s+\d+:\s+\{\s+//; #\} 
            my $scname=$name;
            $name=~s/:.*$//;
            $scname=~s/^.*?\[\d+\s*\,\s*//;
            $scname=~s/\,.*$//;
            push @{$servicenames{$scname}},$name; 	    	
	    }
	}  
	close $YMLF;  	
	} else {
		warn "WARNING: $ymlfile does not exist!\n";
	}
}



=head1 Name
r2n.pl is the Ruby to C++/SystemC translator for the Gannet project.

=head1 Synopsis
	./r2n.pl -H [Ruby source file] > [C++/SystemC header file]
	./r2n.pl -CC [Ruby source file] > [C++/SystemC source file]
	
=head1 Description
This is a translator, not a code generator. It translates the Ruby code line by line, using the hints provided by special tags and blocks.
The approach is simple:
	-Annotate Ruby where necessary
	-Allow use of C++ snippets in Ruby comments

=head1 Marking up the Ruby source

=head2 Restrictions
In C++ code, curly brackets should stay with the keyword:
	GOOD: 
	while (...) {
	BAD:
	while (...)
		{
Simularly for other block structures	.

=head2 Block tags
	#skip/#endskip
	#
- Add Type information in comment after code: #t Type
- Try to infer types based on names, with a lookup unless there is type information
- Initially we focus on converting the .rb files to .cc fiswles. Parts that should go in .h are omitted.
This means that there are no class declarations.

=Head1 Heuristics
* All functions are void! Just prefix void ServiceManager::
* sba_tile/sba_system as function arg => remove; add sba_tile/sba_system snippet on next line
* types of function arguments: currently, only proc_subtask has arguments: Subtask parent_subtask,Packet& subtaskpacket
So let's use the names in a consistent way: /subtask$/=> Subtask; /packet$/=>Packet. Easy
* Other types: It would be a pain to determine which one is a declaration and which one a re-assignment. So use #t to indicate type
* As we use objects, no need to init empty arrays/hashes. So simply 
	s/\=\s*\[\]//, s/\=\s*\{\}//
* All methods must have closing brackets (). Some are obvious: \.(length|size|to_(\w+)); others less so, make a list!	
=cut

#==================================================
#                HELP
#==================================================

#if
#elsif
#else
#unless
#< > == !=
#eq         : string comparison
#for
#while
#until
#say
#print

#substr()
#split()
#join()

#sub

#open $file1, $ARGV[0];

# ^ : begining of string
# $ : end of string 
# wildcard:     . (same as *)
#               + (match the immediately preceding character one or more times. The regular expression /ab+c/ will match "abc," "abbc," "abbbc", and so on.)
#               ? (will match the preceding character zero or one times. The regex /ab?c/ will match "ac" (zero occurences of b) and "abc" (one occurence of b). It won't match "abbc," "abbbc", and so on.
#               * (matches the immediately preceding character or metacharacter zero or more times. This is different from the + quantifier! /ab*c/ will match "abc," "abbc," and so on, just like /ab+c/ did, but it'll also match "ac," because there are zero occurences of b in that string.
# /^http:.+html$/. To understand what this does, read from left to right: This regex will match any string that starts with "http:" followed by one or more occurences of any character, and ends with "html".

#\d         matches a single digit, 
#\w         will match any single "word" character (a letter, digit or underscore), and 
#\s         matches a whitespace character (space and tab, as well as the \n and \r characters).
#\d{3}      means to match exactly three numbers,
# \D \S \W  inverts of \d, \s, \w

# [aeiuo]   say "This string contains at least two vowels in a row."
#               if $string =~ /[aeiou]{2}/;                          
#
# [^aeiou]  matches every character except the lowercase vowels]
# i         which makes a match case-insensitive: if $greet =~ /bob/i;

#/idiot|dope|twit|llama/        true if "idiot," "dope," "twit" or "llama" show up anywhere in the string.

# s///      operator --> s/regex/replacement string/
#

# x flag     allows you to use whitespace and comments within regexps, without it being significant to the pattern:
# g flag     match multiple times

# keys      hash
# values    hash

#==================================================
#                HELP END
#==================================================



##==============================================================================
##          SCRIPT FOR SYSTEMC MODULES' INTERCONNECT EXTRACTION
##==============================================================================

##------------------------------------------------------------------------------
##                              README
##------------------------------------------------------------------------------

# ABOUT: 
# Reads a SysstemC header file and extracts and documents interconnect infomration
#
# USAGE
#
#
# LIMITATION:
# Requires the use of systemC key word 'bind' to be used when making connections




##--------------------------- README END ---------------------------------------





open $src_file, $ARGV[0];  # read file passes as argument
open $dest_file, '>CONNECTIONS.log';

print "Reading File:\t", $ARGV[0], "\n";
print "Writing to:\t CONNECTIONS.log";

$nb = 0;    # lines with bind statement
@bindlines; # array containing lines with bind statement 

$nc = 0; # comment lines (// only)

$np = 0; # sc_port lines 
@portlines; # array containing lines with sc_port  

$nxp = 0; # sc_export lines
@exportlines; # array containing lines with sc_export  

$n = 0;  # total lines

while ($line = <$src_file>)
{
    $n++;
    
    # Check for lines with bind
    if ($line =~ /bind/)
    {
        @bindlines[$nb] = $line;        #store all bind containing lines in an array
        $nb++;                          #inc bindlines counter
        #print "line $n) has 'bind'\n";  #debug message    
    }
    
    # Check for lines with sc_port
    if ($line =~ /^\s*sc_port/ ) # if line begins with any white space and then sc_port
    {
        @portlines[$np] = $line;        #store all sc_port containing lines
        $np++;                          #inc bindlines counter
        #print "line $.) is sc_port\n";  #debug message    
    }
    
    # check for lines with sc_export
    if ($line =~ /^\s*sc_export/ ) # if line begins with any white space and then sc_export
    {
        @exportlines[$nxp] = $line;      #store all sc_export containing lines
        $nxp++;                          #inc bindlines counter
        #print "line $n) is sc_export\n"; #debug message    
    }
    
    # check for lines starting with // comment
    if ($line =~ /^\s*\/\// ) # if line begins with any white space and then //
    {
        $nc++;                          #inc bindlines counter
        #print "line $n) is a comment\n";  #debug message    
    }
    
}



print $dest_file "There are ($nb) lines in the file that contain a 'bind' statement.\n"; 
print $dest_file "There are (", $#bindlines + 1, ") lines in the file that contain a 'bind' statement.\n"; 
print $dest_file "There are ($np) sc_ports in the file.\n"; 
print $dest_file "There are ($nxp) sc_exports in the file.\n"; 
print $dest_file "There are ($nc) comment lines in the file.\n"; 

%connections;

#print "bindlines[0] = ", $bindlines[0];

# s/regex/replacement string/
 

#$bindlines[0] =~ s/\s+//g; #delete all white spaces (place \s with nothing)

#print "bindlines[0] without spaces = ", $bindlines[0], "\n";

#$source_module = $bindlines[0]; # extracting the source module
#$source_module =~   s/\..*//x;     # extract the source module name. Substitute the first occuring dot, and everything that occurs after a ., with nothing 
#print "source_module = ", $source_module, "\n";

#if ($bindlines[0] =~ / (bind) /x )
#{
#    print "Found a $1!!\n";
#}

#service_core.pwr_stlist_d.bind(arb_stlist.xpwr_master1); // connect to arbiter for access to subtask_list

# fill "hash of hash of array" with all the connections
for $i (0 .. $#bindlines)
{
    if( $bindlines[$i] =~ /  (\s*)   # $1 =
                            (\w+)   # $2 = srce_module name
                            (\s*)   # $3 =
                            (\.)    # $4 =
                            (\s*)   # $5 =
                            (\w+)   # $6 = source_module port name
                            (\s*)   # $7 =
                            (\.)    # $8 =
                            (\s*)   # $9 =
                            (bind)  # $10=
                            (\s*)   # $11=
                            (\()    # $12=
                            (\s*)   # $13=
                            (\w+)   # $14= dest_module name
                            (\s*)   # $15=
                            (\.)    # $16=
                            (\s*)   # $17=
                            (\w+)   # $18= dest_module port name
                                 /x )
    {
        $connections{$2}{$6} = [$14, $18]; #entry for source module
        $connections{$14}{$18} = [$2, $6]; #entry for destination module
    }#if
    
    elsif( $bindlines[$i] =~ /  (\s*)   # $1 =
                            (\w+)   # $2 = srce_module name
                            (\s*)   # $3 =
                            (\.)    # $4 =
                            (\s*)   # $5 =
                            (\w+)   # $6 = source_module port name
                            (\s*)   # $7 =
                            (\.)    # $8 =
                            (\s*)   # $9 =
                            (bind)  # $10=
                            (\s*)   # $11=
                            (\()    # $12=
                            (\s*)   # $13=
                            (\w+)   # $14= dest_module name
                                 /x )
    {
        $connections{$2}{$6} = [$14, "NULL"];   # port name of "NULL" means no port (direct connection to a channel or port)
                                                # no second entry just now since the dest is not a module.port but a port or channel
    }#elsif    
     
}# for


use Data::Dumper;
#print Dumper(%connections);

while ( ($modules, $ports) = each %connections )    # loop through each module
{  
    doc_module_connections($modules, $ports);
}


close $src_file;
close $dest_file;



##==============================================================================
##                                  SUB  ROUTINES
##==============================================================================

sub doc_module_connections 
# $_[0] = $module_name,
# $_[1] = %ports
{
   my $is_module;
   print $dest_file "===================================\n";
   print $dest_file  "|"; printf $dest_file "%25s", $_[0]; print $dest_file "        |\n";
   print $dest_file "===================================\n";
   while ( ($port, $destination) = each %{$_[1]} )  # loop through each port
    {
        $is_module = 1;
        print$dest_file "|                                 |\n";
        print $dest_file "|"; printf $dest_file "%33s", $port; print $dest_file "|";
        #print "\t$port = ";
        foreach (@$destination)                      # loop through a port's connections, arranged in (destination_module, destination_port) pair
        {
            if($is_module == 1)    # dest module entry
            {
                print $dest_file " ===> ", $_;
                $is_module = 0;
            }#if
            else                    # dest port entry
            {
                if($_ eq "NULL") # if no port name (direct connection), do nothing
                    {print $dest_file "\^\^";}
                else
                    {printf $dest_file "--->$_";}
                $is_module = 1;
                print $dest_file "\n";
            }#else                 
        }#foreach
        #if($is_module == 0) {print $dest_file "\n";} # newline here if the connection was not to a module.port pair.
    }#while
   print $dest_file "|                                 |\n";
   print $dest_file "===================================\n";
   print $dest_file "\n"; 
    #print "\n";    
}







#print "\n\n";
#print $2, "\n";
#print $6, "\n";    
#print $14, "\n";        
#print $18, "\n";
#print "\n\n";

#transmit_packets.pr_txfifo_d.bind   ( tx_fifo.xpr_fifo_d );
#transmit_packets.pw_txfifo_d.bind   ( arb_txf.xpw_master2 );
#service_core.pwr_stlist_d.bind(arb_stlist.xpwr_master1); // connect to arbiter for access to subtask_list


#$connections{"transmit_packets"}{"pr_txfifo_d"} = ["tx_fifo", "xpr_fifo_d"];
#$connections{"transmit_packets"}{"pw_txfifo_d"} = ["arb_txf", "xpw_master2"];

#$connections{"service_core"}{"pwr_stlist_d"} = ["arb_stlist", "xpwr_master1"];
#@coins = ("Quarter","Dime","Nickel");



#print "Here1\n";

#for $port ( keys %{ $connections{"transmit_packets"} }  ) {
#    print "Port: $port:\n";
#    for $i ( 0 .. $#{ $connections{"transmit_packets"}{$port} } ) {
#        print "\t$i = $connections{'transmit_packets'}{$port}[$i]\n";
#    }
#    print "\n";
#}



#print "Here2\n";

#print "\n\n";

#for $role ( keys %{ $HoH{$family} } )

#while ( ($port, $destination) = each %{ $connections{"transmit_packets"} } )
#{
#    print "\t$port=$destination\n";
#}

#print "Here3\n";

#for $module ( keys %connections ) {
#    print "$module:\n";
#    for $i ( 0 .. $#{ $connections{$module} } ) {
#        print "\t$i = $connections{$module}[$i]\n";
#    }
#    print "\n";




#($6);
#$connections{$2}[1] =  ($14);
#$connections{$2}[2] =  ($18);


#, $14, $18) ;    
#

#$i = 0;  
#$array = $connections{$2};                                     
#print "Here1: ",$connections{$2}[0], "\n";
#print "Here2: ",$array[0], "\n\n\n";
#
#
#for $module ( keys %connections ) {
#    print "$module:\n";
#    for $i ( 0 .. $#{ $connections{$module} } ) {
#        print "\t$i = $connections{$module}[$i]\n";
#    }
#    print "\n";
#}
#print 
#foreach ( @connections{$2} )
#foreach ( @array )
#foreach (0 .. 2)
#{
#    print "connections{$2}[$i]:", $_, "\n";
#    $i++;
#}


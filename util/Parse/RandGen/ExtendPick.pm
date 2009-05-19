# 

use warnings;
use strict;
use Carp;

use Parse::RandGen;

sub Parse::RandGen::Production::extPick {
    my $self = shift or confess("%Error:  Cannot call without a valid object!");
    my %args = ( match=>1,      # Default is to pick matching data
		 vals => { },   # Hash of values of various hard-coded sub-rules (by name)
		 @_ );
    my @conds = $self->conditions();
    my @tokens=();
    my $val='';
    for (my $i=0; $i <= $#conds; $i++) {
    	push @tokens, $conds[$i]->extPick(%args, match=>1);
    }

    return @tokens;
}

sub Parse::RandGen::Rule::extPick { 
    my $self = shift or confess("%Error:  Cannot call without a valid object!");
    my %args = ( match=>1,      # Default is to pick matching data
		 vals => { },   # Hash of values of various hard-coded sub-rules (by name)
		 @_ );

    # Return explicitly specified value (if specified by name or reference to $self)
    return $args{vals}{$self->name()} if (defined($self->name()) && defined($args{vals}{$self->name()}));
    return $args{vals}{$self} if defined($args{vals}{$self});

    my @prods;
    foreach my $prod ($self->productions()) {
	   push(@prods, $prod) if $prod->containsVals(%args);
    }
    @prods = $self->productions() unless(@prods);  # If {vals} does not specify any production of this rule, pick from all productions

    my $prodNum = int(rand($#prods+1));
    return( $prods[$prodNum]->extPick(%args) );
}


sub Parse::RandGen::Regexp::extPick {
    my $self = shift or confess ("%Error:  Cannot call without a valid object!");
    my %args = ( match=>1, # Default is to pick matching data
		 captures=>{ },  # Captures that are being explicitly specified
		 @_ );
    my $vals = { };
    foreach my $cap (keys %{$args{captures}}) {
    	my $ruleRef = $self->capture($cap)
	    or confess("%Error:  Regexp::extPick():  Unknown capture field ($cap)!\n");
	   $vals->{$ruleRef} = $args{captures}{$cap};
    }
    delete $args{captures};
    my @tokens = $self->{_rule}->extPick(%args, vals=>$vals);
    if (0) {
    	my $elem = $self->element();
	   print ("Parse::RandGen::Regexp($elem)::extPick(match=>$args{match}) with value of ", $self->dumpVal(@tokens), "\n");
    }
    return(@tokens);
}

sub Parse::RandGen::Subrule::extPick {
    my $self = shift or confess ("%Error:  Cannot call without a valid object!");
    my %args = ( match=>1, # Default is to pick matching data
		 @_ );
    my $rule = $self->subrule();
    my $ruleName = $self->element();
    defined($rule) or confess("Subrule::extPick():  $ruleName subrule cannot be found in the grammar!\n");

    my %result = $self->pickRepetitions(%args);
    my $matchCnt = $result{matchCnt};
    my $badOne = $result{badOne};

    my $val = "";
    my @tokens=();
    for (my $i=0; $i<$matchCnt; $i++) {
	    my $matchThis = (defined($badOne) && ($i==$badOne))?0:1;  # Only don't match for corrupted data
        my $specifiedVals = ($matchThis && ($i==($matchCnt-1))) ? $args{vals} : { }; # Only specify rules for last capture value
#	$val .= $rule->extPick(%args, match=>$matchThis, vals => $specifiedVals);
    	push @tokens,$rule->extPick(%args, match=>$matchThis, vals => $specifiedVals);
    }
    if ($Parse::RandGen::Debug) {
	print("Parse::RandGen::Subrule::extPick(match=>$args{match}, matchCnt=>$matchCnt, badOne=>".(defined($badOne)?$badOne:"undef")
	      .") on the rule \"".$rule->dumpHeir()."\" has a value of ".$self->dumpVal(@tokens)."\n");
    }
    return (@tokens);
}

sub Parse::RandGen::Literal::extPick {
    my $self = shift or confess("%Error:  Cannot call without a valid object!");
    my %args = ( match=>1, # Default is to pick matching data
		 @_ );
    my $val = $self->element();  # Reset to element before each attempt
    my $keepTrying = 10;
    my $length = length($self->element());
    confess "Literal length is 0!  This should never be!\n" unless ($length);

    my ($method, $char);
    while (!$args{match} && $keepTrying-- && ($val eq $self->element())) {
	$val = $self->element();  # Reset to element before each corruption attempt
	$method = int(rand(4));  # Method of corruption
	$char = int(rand($length));  # Which character

	if ($method == 0) {
	    # Try changing the case of first character
	    substr($val, $char, 1) = lc(substr($val, $char, 1));
	    substr($val, $char, 1) = uc(substr($val, $char, 1)) unless ($val ne $self->element());
	} elsif ($method == 1) {
	    # Randomly change the value of one of the characters
	    substr($val, $char, 1) = chr( (ord(substr($val, $char, 1)) + int(rand(256))) % 256 );
	} elsif ($method == 2) {
	    # Insert a random character into the literal
	    $char = int(rand($length+1));  # Where to insert character
	    substr($val, $char, 0) = int(rand(256)) # Insert random character
	} else {
	    # Remove a character
	    substr($val, $char, 1) = '';
	}
    }

    my $elem = $self->element();
    if ($Parse::RandGen::Debug) {
	if ($args{match}) {
	    print ("Parse::RandGen::Literal($elem)::extPick(match=>$args{match}) with value of ", $self->dumpVal($val), "\n");
	} else {
	    print ("Parse::RandGen::Literal($elem)::extPick(match=>$args{match}, method=>$method, char=>$char) with value of ", $self->dumpVal($val), "\n");
	}
    }
    return ($val);
}


sub Parse::RandGen::CharClass::extPick {
    my $self = shift or confess ("%Error:  Cannot call without a valid object!");
    my %args = ( match=>1, # Default is to pick matching data
		 @_ );

    my %result = $self->pickRepetitions(%args);
    my $matchCnt = $result{matchCnt};
    my $badOne = $result{badOne};

    my $min; my $max;
    my $val = "";
    for (my $i=0; $i < $matchCnt; $i++) {
	if (defined($badOne) && ($i==$badOne)) {
	    $min = $self->{_charsetEndOffset};
	    $max = 256;
	} else {
	    $min = 0;
	    $max = $self->{_charsetEndOffset};
	}
	my $chrOffset = $min + int(rand($max-$min));
	$val .= substr($self->{_charset}, $chrOffset, 1);
    }
    my $elem = $self->element();
    if ($Parse::RandGen::Debug) {
	print("Parse::RandGen::CharClass($elem)::extPick(match=>$args{match}, matchCnt=>$matchCnt, badOne=>".(defined($badOne)?$badOne:"undef")
	      ." with value of ".$self->dumpVal($val)."\n");
    }
    return ($val);
}

1;
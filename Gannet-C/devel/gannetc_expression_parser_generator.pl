#!/usr/bin/perl
use strict;
use warnings;

# Operator expression generator for SableCC

# Generates the Helpers, Tokens, Production rules and AST section for operators, 
# as well as the basic template code for dealing with them in the generated classes

my $nlevs=14;

my %optable=(
    1 => ['nonassoc',['++', '--']],
    2 => ['right',['**']],
    3 => ['right',['!','~']], 
    4 => ['left',['*','/','%']], 
    5 => ['left',['+','-']], 
    6 => ['left',['<<','>>']],
    7 => ['nonassoc',['<=','>=']], # '<','>' are anomalous because of <...>
    8 => ['nonassoc',['==','!=']], 
    9 => ['left',['&','bitand']],
    10 => ['left',['|','^','bitor','xor']],
    11 => ['left',['&&','and']],
    12 => ['left',['||','or']],
    13 => ['right',['?']], # this is obviously wrong, the operator is ternary cond ? iftrue : iffalse
    14 => ['right',['=']], # += -= *= etc. handled separately
);

my $expr='operator_pure_expr';
my $argexpr='operator_pure_expr';
# CamelCase
my $ccexpr=join('',map {ucfirst($_)} split(/_/,$expr) );
my $plus='add';

open my $TEMPL, '<','gannetc_expressions_templ.sablecc3';
open my $GRAM, '>','gannetc_expressions_gen.sablecc3';
while (my $line=<$TEMPL> ) {
	print $GRAM $line;
# Helpers
#print "\nHelpers\n\n";
$line=~/OP_EXPR_HELPERS/ && do { 
for my $lev (1..$nlevs) {
    my @ops = @{$optable{$lev}->[1]};    
    print $GRAM "l${lev}ops = '".join ("' | '",@ops)."';\n";
}
};

# Tokens
#print "\nTokens\n\n";
$line=~/OP_EXPR_TOKENS/ && do { 
for my $lev (1..$nlevs) {
    print $GRAM  "l${lev}op = l${lev}ops;\n";
}
};
# Productions

#print "\nProductions\n\n";
$line=~/OP_EXPR_PRODUCTIONS/ && do {
for my $j (1.. $nlevs-1) {
my $lev=$nlevs+1-$j;
    my $lm1=$lev-1;
    if ($lev==2) {
    	$lm1=0;
    }
my $assoc=$optable{$lev}->[0];


#    op_expr {-> operator_pure_expr} =  [left]:atomic_pure_expr [op]:operator [right]:atomic_pure_expr  
#    {-> New operator_pure_expr.opcall(op,[left.atomic_pure_expr,right.atomic_pure_expr])}; 
print $GRAM  "
    l${lev}term            {-> $expr} =
";
    if ($assoc eq 'left') {
print $GRAM  "               {l${lev}op}    [left]:l${lev}term [op]:l${lev}op [right]:l${lm1}term  {-> New $expr.l${lev} (op,[left.$argexpr,right.$argexpr] ) }\n";
    } elsif ($assoc eq 'right') {
    if ($lev!=13) {
print $GRAM  "               {l${lev}op}    [left]:l${lm1}term [op]:l${lev}op [right]:l${lev}term  {-> New $expr.l${lev} (op,[left.$argexpr,right.$argexpr] ) }\n";
    } else {
print $GRAM  "               {l13op}    [cond]:l12term [op]:l13op [t]:l13term colon [f]:l13term   {-> New $expr.l13 (op,[cond.$argexpr,t.$argexpr,f.$argexpr] ) }\n";    
    }    
    } else {
print $GRAM  "               {l${lev}op}    [left]:l${lm1}term [op]:l${lev}op [right]:l${lm1}term  {-> New $expr.l${lev} (op,[left.$argexpr,right.$argexpr]) }\n";    
    }
    if ($lev==5) {
print $GRAM  "              | {l5op_signed}    [left]:l5term [right]:l4term {-> New $expr.$plus ([left.$argexpr, right.$argexpr]) } // this is a bit of a hack, to deal with signed integers\n";
    } 
    
print $GRAM "             | {l${lm1}term}    l${lm1}term              {-> l${lm1}term.$argexpr };\n";
}
};


# AST

$line=~/OP_EXPR_AST/ && do {
#print $GRAM  '
#Abstract Syntax Tree
#
#expr               =
#';

    for my $lev (1..$nlevs) {
        print $GRAM "                 {l$lev}    [op]:l${lev}op [args]:${argexpr}+  |\n";
	}


#print $GRAM  '                 {number}  number 
#                      ;
#';

};

}
close $GRAM;
close $TEMPL;

# ============================================================================================================================================================

# Generate a cpde template for the visitors for the operator expressions 

open my $JTEMPL, '<','GannetCExpressionsVisitor_templ.java';
open my $JAVA, '>','GannetCExpressionsVisitor_gen.java';

while (my $line=<$JTEMPL>) {
	
	print $JAVA $line;
# Java operator table

$line=~/OP_EXPR_OPTABLE_ENTRIES/ && do {
#print $JAVA'
#  /* BEGIN Generated */
#  private Map<String, Integer> optable = new HashMap<String, Integer> ();
#  
#  public CLASSNAME () {
#';  
for my $lev (1..$nlevs) {
    my @ops = @{$optable{$lev}->[1]}; 
    my $i=0;
    for my $op (@ops) {
        print $JAVA "\t\toptable.put(\"$op\",$lev$i);\n";
        $i++;
    }
}
#print '  /* END Generated */
#  }
#';
};

# Visitors

$line=~/OP_EXPR_VISITORS/ && do {
#print "\n\n";
#print '// ','-' x 80, "\n";
for my $lev (1..$nlevs) {
print $JAVA "
    public void outAL${lev}$ccexpr(AL${lev}$ccexpr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {
";        
 my @ops = @{$optable{$lev}->[1]}; 
 my $i=0;
 for my $op (@ops) {
 if ($lev==13) { 
print $JAVA "
            case $lev$i: // '$op'
 	            //setNodeInt (node, (int)Math.pow((double)getNodeInt (node.getL()) , (double)getNodeInt (node.getR())));	
                break;
"; 	
 } elsif ($lev>1) {
 print $JAVA "
            case $lev$i: // '$op'
                //setNodeInt (node, getNodeInt (node.getL()) $op getNodeInt (node.getR()));
                break;
";
} else { # unary ++ and --
my $op1=($op eq '++')?'+':'-';
 print $JAVA "
            case $lev$i: // '$op'
                //setNodeInt (node, getNodeInt (node.getL()) $op1 1 );
                break;
";

}
$i++;                
}                
print $JAVA '                            
        }
    }
';

}
};

}
close $JAVA;
close $JTEMPL;

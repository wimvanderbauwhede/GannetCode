#!/usr/bin/perl
use strict;
use warnings;

# Operator expression generator for SableCC

# Generates the Helpers, Tokens, Production rules and AST section for operators, 
# as well as the basic template code for dealing with them in the generated classes

my $nlevs=13;
my $signop=5; # +/- are special 
my $fixity=''; 

my %optable=(
    1 => ['Prefix',['++', '--'],'AssocNone'],
    2 => ['Infix',['**'],'AssocRight'],
    3 => ['Prefix',['!','~'],'AssocRight'], 
    4 => ['Infix',['*','/','%'],'AssocLeft'], 
    5 => ['Infix',['+','-'],'AssocLeft'], 
    6 => ['Infix',['<<','>>'],'AssocLeft'],
    7 => ['Infix',['<=','>=','>','<'],'AssocNone'],
    8 => ['Infix',['==','!='],'AssocNone'], 
    9 => ['Infix',['&','bitand'],'AssocLeft'],
    10 => ['Infix',['|','^','bitor','xor'],'AssocLeft'],
    11 => ['Infix',['&&','and'],'AssocLeft'],
    12 => ['Infix',['||','or'],'AssocLeft'],
    13 => ['Infix',['='],'AssocRight'],
);
open my $TEMPL, '<','expressions_templ.sablecc3';
open my $GRAM, '>','expressions_gen.sablecc3';

while (my $line=<$TEMPL> ) {
    $line=~s/NLEVS/$nlevs/g;
    print $GRAM $line;
    
    # Helpers
        $line=~/OP_EXPR_HELPERS/ && do { 
            for my $lev (1..$nlevs) {
                my @ops = @{$optable{$lev}->[1]};    
                print $GRAM "ops_l$lev = '".join ("' | '",@ops)."';\n";
            }
        };

    # Tokens
        $line=~/OP_EXPR_TOKENS/ && do { 
            for my $lev (1..$nlevs) {
                my $opname=($lev==$signop)?'sign':"op_l$lev"; # special case: sign
                print $GRAM  "$opname = ops_l$lev;\n";
            }
        };
        
# Productions
    $line=~/OP_EXPR_PRODUCTIONS/ && do {
        for my $j (1.. $nlevs) {

            my $lev=$nlevs+1-$j;
            my $lm1=$lev-1;
            my $assoc=$optable{$lev}->[2];
            my $fixity=$optable{$lev}->[0];

            print $GRAM  "term_l$lev\t{-> expr} = ";
            if ($assoc eq 'AssocLeft') {
                if ($fixity eq 'Infix') {
                    my $opname=($lev==$signop)?'sign':"op_l$lev"; # special case: sign
                    print $GRAM  "{op_l$lev}    term_l$lev $opname term_l$lm1  
                    {-> New expr.l$lev ($opname,term_l$lev.expr,term_l$lm1.expr) }\n";
                } elsif ($fixity eq 'Postfix') {
                    print $GRAM  "{op_l$lev}     op_l$lev term_l$lm1  
                    {-> New expr.l$lev (op_l$lev,term_l$lm1.expr) }\n";
                } elsif ($fixity eq 'Prefix') {
                    print $GRAM  "{op_l$lev}    term_l$lev op_l$lev   
                    {-> New expr.l$lev (op_l$lev,term_l$lev.expr) }\n";
                } else {
                    die "Can't handle fixity $fixity!\n";
                }
            } elsif ($assoc eq 'AssocRight') {
                if ($fixity eq 'Infix') {
                    print $GRAM  "{op_l$lev}    term_l$lm1 op_l$lev term_l$lev  
                    {-> New expr.l$lev (op_l$lev,term_l$lm1.expr,term_l$lev.expr) }\n";
                } elsif ($fixity eq 'Postfix') {
                    print $GRAM  "{op_l$lev}    op_l$lev term_l$lev  
                    {-> New expr.l$lev (op_l$lev,term_l$lev.expr) }\n";               
                } elsif ($fixity eq 'Prefix') {
                    print $GRAM  "{op_l$lev}    term_l$lm1 op_l$lev   
                    {-> New expr.l$lev (op_l$lev,term_l$lm1.expr) }\n";

                } else {
                    die "Can't handle fixity $fixity!\n";
                }
            } else { # non-assoc
                if ($fixity eq 'Prefix') {# postfix
                    print $GRAM  "{op_l$lev}    term_l$lm1 op_l$lev 
                    {-> New expr.l$lev (op_l$lev,term_l$lm1.expr) }\n";
                } elsif ($fixity eq 'Postfix') { # prefix
                    print $GRAM  "{op_l$lev}    op_l$lev term_l$lm1  
                    {-> New expr.l$lev (op_l$lev,term_l$lm1.expr) }\n";
                } elsif ($fixity eq 'Infix') { # infix
                    print $GRAM  "{op_l$lev}  [l]:term_l$lm1 op_l$lev [r]:term_l$lm1  
                    {-> New expr.l$lev (op_l$lev,l.expr,r.expr) }\n";   
                } else {
                    die "Can't handle fixity $fixity for non-assoc operators!\n";
                }
            }

            print $GRAM "             | {term_l$lm1}    term_l$lm1              {-> term_l$lm1.expr };\n";
        }
    };

# AST    
    $line=~/OP_EXPR_AST/ && do {
        for my $lev (1..$nlevs) {
            my $opname=($lev==$signop)?'sign':"op_l$lev"; # special case: sign
            my $fixity=$optable{$lev}->[0];
            if ($fixity eq 'Prefix') {
                print $GRAM "                 {l$lev}     [op]:op_l$lev [l]:expr  |\n";
            } elsif ($fixity eq 'Postfix') {
                print $GRAM "                 {l$lev}     [op]:op_l$lev [r]:expr  |\n";
            } else  {
                print $GRAM "                 {l$lev}    [op]:$opname [l]:expr  [r]:expr |\n";
            }
        }
    };

}
close $GRAM;
close $TEMPL;

open my $JTEMPL, '<','ExpressionsVisitor_templ.java';
open my $JAVA, '>','ExpressionsVisitor_gen.java';

while (my $line=<$JTEMPL>) {

    print $JAVA $line;
# Java operator table

    $line=~/OP_EXPR_OPTABLE_ENTRIES/ && do {
        for my $lev (1..$nlevs) {
            my @ops = @{$optable{$lev}->[1]}; 
            my $i=0;
            for my $op (@ops) {
                print $JAVA "\t\toptable.put(\"$op\",$lev$i);\n";
                $i++;
            }
        }
    };

# Visitors

    $line=~/OP_EXPR_VISITORS/ && do {
        for my $lev (1..$nlevs) {
            my $opname=($lev==$signop)?'sign':"op_l$lev"; # special case: sign
            print $JAVA "
            public void outAL${lev}Expr(AL${lev}Expr node)
            {
            String opstr=node.getOp().toString().trim();
            switch (optable.get(opstr)) {
            ";        
            my @ops = @{$optable{$lev}->[1]}; 
            my $i=0;
            for my $op (@ops) {
                if ($lev==2) { # **
                    print $JAVA "
                    case $lev$i: // '$op'
                    setNodeInt (node, (int)Math.pow((double)getNodeInt (node.getL()) , (double)getNodeInt (node.getR())));	
                    break;
                    "; 	

                } elsif ($lev==1) { # unary ++ and --
                    my $op1=($op eq '++')?'+':'-';
                    print $JAVA "
                    case $lev$i: // '$op'
                    setNodeInt (node, getNodeInt (node.getL()) $op1 1 );
                    break;
                    ";

                } elsif ($lev==3) {# unary ! and ~
                    print $JAVA "
                    case $lev$i: // '$op'
                    setNodeInt (node, $op getNodeInt (node.getR()));
                    break;
                    ";
                } elsif ($lev==9 or $lev==10 or $lev==11 or $lev==12) {# bitwise and logic ops don't work in Java
                    print $JAVA "
                    case $lev$i: // '$op'
                    setNodeInt (node, getNodeInt (node.getL()) + getNodeInt (node.getR()));
                    break;
                    ";

                } else {
                    print $JAVA "
                    case $lev$i: // '$op'
                    setNodeInt (node, getNodeInt (node.getL()) $op getNodeInt (node.getR()));
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

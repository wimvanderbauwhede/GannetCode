#my $v=42;
use Gannet; # Mainly to provide buffer/stream support. Must be present. Call it "gannetDecl"
# Somehow we must have the class interface ...
use Gannet::IO qw( say ); # this line is parsed to determine all valid methods for a service. Maps to serviceDecl
# The line below should map to an instance declaration. 
my $io = new IO(1,2,3);
# However, the next lines are also instantiations
my $n_insts=32;
#my @ios = map { new IO($_) } (1..$n_insts);
# or
#foreach my $i (0..$n_insts-1) {
#    push @ios, new IO($i);
#}
# So the core is "new IO($i);" which means I'd need to know if this is a service or not, i.e. if it's in the Gannet namespace
# At first, we simply say any "new" is an instAlloc, which is a PureE

# Crucially, we need to be able to detect that the lists are compile-time


( 
    my $io = new IO(1,2,3),
    my $io2 = new IO,
    my $io3 = new IO() # IO->new(1,2,3) is NOT supported
);
 
sub ff {

        my $x=shift(@_);
#        (my $y)=@_; # TODO
         $x*($x+1);
};

my @a=(1,2,3);
my @b=@a;
my $bb=42;
my $cc=$b[0][$bb];
my $d=shift; # FIXME

{
      my $c=1;
    my $f= sub { 
        my $x=shift(@_);
         $x*($x+1);
	};
	
    if ($c==1) {
		use seq;
        $io->say( "Hello!");
        $io2->say( "Hello!");
        $io3->say( "Hello!");
    } else {
        $io->say( "Hi!");
        $f->(6);
        ff(6);
#        &{$f}(6); # TODO
    }
};

for (my $i=0;$i<10;$i++) {
	my $j=$i*$i;
};

for (my $i=0;$i<10;$i++) {
	my $j=$i*$i;	
};


while ($i<10) {
	use seq;
	my $j=$i*$i;
	$i++;
};

my $acc=0;
my $N=10;
foreach my $i (1 .. $N) { 
	use seq;
	$acc = $acc + $i * $i;
	$acc++;
#	$acc+= 2;  # FIXME: opupdate is not working!

};

foreach my $j (@b) {
print($j);
}

my @ar = 1 .. 10;

my $aref1 = [1 ,3, 3];
my $aref2 = [1 .. 3];
my $href0 = {};
my $href1 = { 1=> $aref1, "2" => $aref2};
my $href2 = { 1=> $aref1, "2" => $aref2};

my $url = "http://www.perl.org";
$url=~m/http:\/\//;

## Required conversions, we can do them in the Emitter or create a separate PerlASTConversions module
#(assign 'io (new '1 '2 '3)) => (io.new '1 '2 '3)
#So, if we have a PAssign which contains a PInstAlloc, we do a conversion
# to a DInstDecl

#'(label f (lambda  '(let (assign 'x (apply shift _) ) (* x (+ x '1))))) =>'(label f (lambda 'x  '(* x (+ x '1))))
#Here we need to scan the body of a lambda for an assign where the rhs is either an apply of a shift or an array index, or
#the case of (my $x)=@_, which atm would seem like a Begin containing an Assign inside a Bind with as rhs @_#
# Can't parse this atm as (my $y) is not a valid lhs expr.
#
#(-> io (apply say "Hi!")) => (io.say "Hi!")
# An operator -> with an apply inside it
# So OpCall => ServiceCall
#
#(-> f '6) => (apply f '6) => (apply f '6)
# An operator -> without apply inside simply becomes apply
# so OpCall -> FunAppl

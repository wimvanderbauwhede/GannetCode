package Puffin::Services {
	
	$Puffin::Services::v=0;
	sub evl($a) {
		my $c;
                if ($a.WHAT()~"" eq 'Block') {
                        $c=$a();
                } elsif ($a.WHAT()~"" eq "Thread") {
                        $c=$a.join();
                } else {
			$c=$a;
		} 
                $c;
	}
	sub gw {
		my $r=@_[0];
		while($r.WHAT()~"" eq 'Block' or $r.WHAT()~"" eq 'Thread') {
			say "\t"~$r.WHAT()~"" if $Puffin::Services::v;
			$r=evl($r);
		}
		say $r;	
	}

class Service {
	# unquote is tricky: if we unquote a thread, it starts immediately, not what we want.
	# so instead of unquoting we need something more like evl(): 
	# if it's a thread, join; else, just return. 
	sub ev_($a) {
        say 'ev_:<'~$a.WHAT()~'>' if $Puffin::Services::v;
        if ($a.WHAT()~"" eq "Sub") {# or $a.WHAT()~"" eq 'Block') {
            die "Sub is DEPRECATED";
            return $a();
        } elsif ($a.WHAT()~"" eq "Thread") {
            my $r=$a.join();
            if ($r.WHAT()~"" eq 'Thread') {
                &Service::ev_($r);
            } else {
                return $r;
            }
        } else {
            return $a;
        }
	}	
	
	method ev (@args) {
		my @evargs=();
		for @args->$a {	
			@evargs.push(ev_($a));
		}
		say 'ev:',@evargs.>>WHAT()  if $Puffin::Services::v;
		return @evargs;
	}
    
    coro crl ($cond,$task) {
        while ($cond) {
            yield $task();
        }
    }

    method coroloop($cond,$task) {
        crl($cond,$task);
    }
}
# ---------------- Actual services start here ----------------------
class ALU is Service {

    method plus {
	my @args= self.ev(@_);
	    say 'add:',@args.>>WHAT() if $Puffin::Services::v;
         return [+] @args;
    }
    
    method times {
	my @args= self.ev(@_);
	    say 'mult:',@args.>>WHAT() if $Puffin::Services::v;
        return [*] @args;
    }

    method minus {
	my @args= self.ev(@_);
	    say 'sub:',@args.>>WHAT() if $Puffin::Services::v;
        return [-] @args;
    }

    method over {
	my @args= self.ev(@_);
	    say 'div:',@args.>>WHAT() if $Puffin::Services::v;
        return [/] @args;
    }
    
    method lt {
	my @args= self.ev(@_);
    my $l=@args[0];
    my $r=@args[1];
	    say '<:',$l.WHAT(),',',$r.WHAT() if $Puffin::Services::v;
        if (@args[0] < @args[1]) {
            return 1;
        } else {
            return 0;
        }
    }

    method gt {
	my @args= self.ev(@_);
    my $l=@args[0];
    my $r=@args[1];
	    say '>:',$l.WHAT(),',',$r.WHAT() if $Puffin::Services::v;
        if (@args[0] > @args[1]) {
            return 1;
        } else {
            return 0;
        }
    }

    method eq {
	my @args= self.ev(@_);
    my $l=@args[0];
    my $r=@args[1];
	    say '==:',$l.WHAT(),',',$r.WHAT() if $Puffin::Services::v;
        if ($l == $r) {
            return 1;
        } else {
            return 0;
        }
    }
    
    method rnd {
        rand();
    }
}

class Cond is Service {
    method if {
	my @args= self.ev(@_);
	my ($c,$t,$f) =@args;
        if ($c) {
            if ($t.WHAT~"" eq 'Sub') {
                die "Sub is DEPRECATED";
                return &$t();
            } elsif ($t.WHAT()~"" eq 'Thread') {
                die "Thread in Thread should not happen!";
                return $t.join();
            } elsif ($t.WHAT()~"" eq 'Block') {
                my $r=$t();
	        	return &Service::ev_($r);
            } else {
                return $t;
            }
        } else {
            if ($f.WHAT()~"" eq 'Sub') {
                die "Sub is DEPRECATED";
                return &$f();
            } elsif ($f.WHAT()~"" eq 'Thread') {
                die "Thread in Thread should not happen!";
                return $f.join();
            } elsif ($f.WHAT()~"" eq 'Block') {
                my $r=$f();
        		return &Service::ev_($r);
            } else {
                return $f;
            }  
            } 
    };
    method return {
    	my @args= self.ev(@_);
	    my $a=@args[0];
        if ($a.WHAT~"" eq 'Sub') {
                die "Sub is DEPRECATED";
            &$a();
        } elsif ($a.WHAT()~"" eq 'Block') {
            my $r=$a();
        	return &Service::ev_($r);
        } elsif ($a.WHAT()~"" eq 'Thread') {
                die "Thread in Thread should not happen!";
                $a.join();
        } else {
                $a;
        }
    };
} # Cond

class Block is Service {
    has $.state;
    has %.store;
    
    method begin {
	my @args= self.ev(@_);
	return @args[-1];
    }

    method let {
	my @args= self.ev(@_);
#        my $a= @args.pop();
    my $r;
    my @t=();
    for @args->$a {
        if ($a.WHAT()~"" eq 'Sub') {
            @t.push($a());
            die "sub {} is DEPRECATED";
        } elsif ($a.WHAT()~"" eq 'Block') {
            my $b=$a(); 
    		@t.push(Service::ev_($b));
        } elsif ($a.WHAT()~"" eq 'Thread') {
            die "Thread in Thread should not happen";
            @t.push($a.join());    
        } else {
            @t.push($a);
        }
    }
    $r=@t.pop();
            map ->$_v {if ($_v.WHAT()~"" eq "Str") {say "DEL $_v" if
                    $Puffin::Services::v ;%.store.delete($_v)}},@args;
            say 'cleaned up LET:',%.store.>>WHAT() if $Puffin::Services::v;
            return $r;
    }

    method assign {
	my @args= self.ev(@_);
	say 'assign:',@args.>>WHAT() if $Puffin::Services::v;
        %.store{@args[0]}=@args[1];
        @args[0];
    }

    method update {
	my @args= self.ev(@_);
	say 'update:',@args.>>WHAT() if $Puffin::Services::v;
	my $start_wait=1;
    	while (not %.store.exists(@args[0])) {
	#wait
		say "UPDATE: waiting for '@args[0]'" if $start_wait and $Puffin::Services::v;
		$start_wait=0;
	}
    say "got @args[0]" if not $start_wait and $Puffin::Services::v;
        %.store{@args[0]}=@args[1];
        @args[1]; # returning @args[0] results in clean-up so can't do that!
    }

    method read {
	my @args= self.ev(@_);
	my $start_wait=1;
    	while (not %.store.exists(@args[0])) {
	#wait
		say "READ: waiting for '@args[0]'" if $start_wait and $Puffin::Services::v;
		$start_wait=0;
	}
	say "got @args[0]" if not $start_wait and $Puffin::Services::v;
        my $_v=%.store{@args[0]};
	say 'read:',@args.>>WHAT(),'=>',$_v.WHAT() if $Puffin::Services::v;
        $_v;
    }
   
    method list {
    	my @args= self.ev(@_);
        say @args.>>WHAT() if $Puffin::Services::v;
        my $l=\@args;
        $l;
    }

    method head {
    	my @args= self.ev(@_);
        my $l=@args[0];
        say @args.>>WHAT() if $Puffin::Services::v;;
        return @$l[0];
    }

    method tail {
    	my @args= self.ev(@_);
        my $l=@args[0];
        @$l.shift;
        return $l;
    }

    method length {
    	my @args= self.ev(@_);
        my $l=@args[0];
        return @$l.elems;
    }

    method cons {
    	my @args= self.ev(@_);
        my $l=@args[0];
        my $e=@args[1];
        @$l.push($e);
        return $l;
    }

    method append {
    	my @args= self.ev(@_);
        my $l1=@args[0];
        my $l2=@args[1];
        my @lj=(@$l1,@$l2);        
        return \@lj;
    }
} # Block


class Function is Service {
	has $.state=0;
	has %.store;
	has $.subst = sub {};
        method lambda {
	    #my @args= self.ev(@_);
            #my $l=@_;
	    #say @args.join(';');
            #return \@args;#$l;
		@_[0];
        };

       # This should really use soft refs etc 
       # But they are broken
       # so: (lambda 'x 'y '(+ x y)) 
       # $f.lambda('x','y',{$a.add({$f.var('x')},{$f.var'y')})}
        method apply {
	    my @args= self.ev(@_);
            say 'apply:',@args.>>WHAT() if $Puffin::Services::v;
		my %lambda_args_store=();
            my $l=@args.shift;
	    my @lambda_vars=@$l;
	    my $lambda_body=@lambda_vars.pop;
		say "LVS:",@lambda_vars.join(';') if $Puffin::Services::v;
	    #my @evaled_args=map ->$a {if ($a.WHAT()~"" eq 'Sub') {return $a()} else {return $a}},@args;
	my @evaled_args=();
        for @args->$a {
            if ($a.WHAT()~"" eq 'Sub') {
                @evaled_args.push( &$a());
            } elsif ($a.WHAT()~"" eq 'Block') {
                @evaled_args.push( $a());
            } elsif ($a.WHAT()~"" eq 'Thread') {
                @evaled_args.push( $a.join());            
            } else {
                @evaled_args.push( $a);
            }
        }
	    say 'A:',@args.>>WHAT() if $Puffin::Services::v;
	    say 'EA:',@evaled_args.>>WHAT() if $Puffin::Services::v;
            #print 'LV:', $lv~$.state;# if $Puffin::Services::v;
		#$::("*::$lv")=@evaled_args.shift;	
            #%.store{$lv}=@evaled_args.shift;
            #say '=>',%.store{$lv};#$::("*::$lv") ;#if $Puffin::Services::v;
            #say '=>',%lambda_args_store{$lv~$.state};#$::("*::$lv") ;#if $Puffin::Services::v;
        #    say 'LB:',$lambda_body.>>WHAT() if $Puffin::Services::v;
            my $r;
            if ($lambda_body.WHAT()~"" eq 'Sub') {
                 $r=&$lambda_body();
            } elsif ($lambda_body.WHAT()~"" eq 'Block') {
                 $r=$lambda_body();
            if ($r.WHAT()~"" eq 'Thread') {
                return $r.join();
            } else {
                my $r2=&$r(@evaled_args);
		return $r2.join();
                }
            } elsif ($lambda_body.WHAT()~"" eq 'Thread') {
                $r=$lambda_body.join();
            } else {
                $r=$lambda_body;
                }
        	$r;
        }

        method subst_ {
        my @args= self.ev(@_);
        while (not %.store.exists(@args[0])) {
        #wait
        }
         my $v=@args[0];
            	%.store{$v};
        }

}

class Buffer is Service {
	has @.store=();
    has %.refs={};	
	method fifo {
    	my @args= self.ev(@_);
        my $l=@args[1];
        @.store[@args[0]].push(@$l);

	}
	
	method get {
    	my @args= self.ev(@_);
        my $r=@.store[@args[0]].shift();
        $r;
	}
    
    # (buffer sid 'arg)
    # buffer pushes onto a list, so we can init
    # the pipe with multiple values
    method buffer {
		my @args= self.ev(@_);
        my $i=@args[0];
        my $s=@args[1];
        my $r=$s();
        %.refs{$i}=$s;
        @.store[$i].push($r);    
    }
    # stream returns the store and gets a new value for it
    # (stream sid 'arg)
    method stream {
		my @args= self.ev(@_);
        my $i=@args[0];
        my $s=%.refs{$i};
        my $r=$s();
        @.store[$i].push($r);            
        @.store[$i].shift();            
    }

    method peek {
		my @args= self.ev(@_);
        my $i=@args[0];
        @.store[$i][0];
    }
    # loop takes a task and a condition:
    # (loop cond 'task)
    # As long as the condition is true, it loops the task
    method loop {
		my @args= self.ev(@_);
        my $cond=@_[0];
        my $task=@args[1];
        my @res=();
        while ($cond()) {
            @res.push($task());
        }
        \@res;
        #self.coroloop($cond,$task);
    }

    method iter {
		my @args= self.ev(@_);
        my $niters=@_[0];
        my $task=@args[1];
        my @res=();
        for 1..$niters {
            @res.push($task());
        }
        \@res;
    }
    
    method eos {
            my @args = self.ev(@_);
            say 'eos:',@args.join(';') if $Puffin::Services::v;
            my $i= @args[0];
            my $r=(@.store[$i][0]==undef);
            $r;
    }

} # Buffer

class StreamIO is Service {
    has %.store={};
    has %.state={}; # for state of filehandles
    method ioread {
        my @args = self.ev(@_);
        say 'ioread:',@args.join(';') if $Puffin::Services::v;
        my $res=0;
        my $port= @args[0];
        my $in= undef;
        if (not %.store.exists($port)) {
        	$*ERR.print("$port: ");
            $in = =<>;
#            $in.chomp;
        } else {
            my $fd = %.store{$port};
            my $line="";
            if ($line = $fd.next()) {
                $in = $line;
            }
        }
        return $in;
    }

    method iowrite {
            my @args = self.ev(@_);
            say 'iowrite:',@args.join(';') if $Puffin::Services::v;
        my $res = 0;
         my $port = @args[0];
         my $data = @args[1];
         say "Got <$data>" if $Puffin::Services::v;
         if (not defined $data) {
             return 0;
         } else {
         if (not %.store.exists($port)) {
             say $port,':',$data;
         } else {
             my $fd = %.store{$port};
             $fd.say($port,':',$data);
         }
             return 1;
         } 
    }


# For convenience and to make the system more flexible,
# I add an (open '1 "filename") service, which binds a filename to a port
	method display {
            my @args= self.ev(@_);
	say ">>>",@args.join(";");
	1;
    }

    method fopen {
         my @args= self.ev(@_);
         my $fd;
         my $p=@args[0];
         if(@args[2] eq "r") {
         $fd=open(@args[1],:r);
     } elsif (@args[2] eq "w") {
         $fd=open(@args[1],:w);
     } else {
         $fd=open(@args[1]);
     }
     %.store{$p}=$fd;
     $p;
    }

    method fclose {
		my @args= self.ev(@_);
        my $p=@args[0];
        if (%.store.exists($p)) {
        	my $fd=%.store{$p};
        	$fd.close();
        }
    }
} # end of IO
} # Puffin::Services

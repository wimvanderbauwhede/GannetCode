use bytes;

my $nbytes=($WORDSZ==64)?8:4;
my $all1s= ($WORDSZ==64)? 18446744073709551616 : 4294967296;

#  [(56,48,40,32,)24,16,8,0];
my @bytes= ($WORDSZ==64)?(56,48,40,32,24,16,8,0):(24,16,8,0);

=pod;
32 bits:;
FB_Packet_type=3;
FB_Prio=3;
FB_Redir=2;
FB_Length=8;
FB_To=8;
FB_Return_to=8;
=cut;

# header is (\@hw1,\@hw2,\@hw3)
# @hw1 is 
sub bytecodize_header { my @gph =@_;
    my @hw1=@{$gph[0]};
        my @hw2=@{$gph[1]};
    my @hw3=@{$gph[2]};

        my $pt = $hw1[0];# ptype;
        my $rd = $hw1[1];#redir;
        my $pr = $hw1[2];#prio;
        my $pto = $hw1[4];#to;
        my $tl= lo($pto);
        my $rto=$hw1[5];#return_to;
        my $rtl=lo($rto);
        my @byteword1=();
if ($WORDSZ==64) {
        my $prrd = $pr*8+$rd;
        my $pl= $hw1[3];#plength;
        my $ll=lo($pl);
        my $lh= hi($pl,$ll);
        my $th= hi($pto,$tl);
        my $rth=hi($rto,$rtl);
         @byteword1=map sub {chr($_[0])}, ($pt,$prrd,$lh,$ll,$th,$tl,$rth,$rtl);
elsif ($WORDSZ==32) {
        my $pt_pr_rd = $pt*32+$pr*4+$rd;
        my $pl= lo($hw1[3]);# plength;
         @byteword1=map sub {chr($_[0])}, ($pt_pr_rd,$pl,$tl,$rtl);
}
        my @byteword2=bytecodize_gs (@hw2);
        my @byteword3=bytecodize_gs (@hw3) ;
    
        return (\@byteword1,\@byteword2,\@byteword3);
}

#----
sub bytecodize_hw1 { my @hw1 =@_;
        my $pt = $hw1[0];# ptype;
        my $rd = $hw1[1];#redir;
        my $pr = $hw1[2];#prio;
        my $pto = $hw1[4];#to;
        my $tl= lo($pto);
        my $rto=$hw1[5];#return_to;
        my $rtl=lo($rto);
        my @byteword1=();
if ($WORDSZ==64) {
        my $prrd = $pr*8+$rd;
        my $pl= $hw1[3];#plength;
        my $ll=lo($pl);
        my $lh= hi($pl,$ll);
        my $th= hi($pto,$tl);
        my $rth=hi($rto,$rtl);
         @byteword1=map sub {chr($_[0])}, ($pt,$prrd,$lh,$ll,$th,$tl,$rth,$rtl);
elsif ($WORDSZ==32) {
        my $pt_pr_rd = $pt*32+$pr*4+$rd;
        my $pl= lo($hw1[3]);# plength;
         @byteword1=map sub {chr($_[0])}, ($pt_pr_rd,$pl,$tl,$rtl);
}
    
        return @byteword1;
}
#----

sub setNSymbols {my @ppl = @_;

        my $operator=shift @ppl;
        my @operands = @ppl;
        my $nsymbols = countNSymbols(@operands);
        my $ssymbol=$operator;
        $ssymbol->[5]=$nsymbols;
    return ( $ssymbol,@operands);

}    
sub countNSymbols {my @operands = @_;
 length ( grep  { $_ != 'X'},  @operands);
}
   
#bytecodize_payload :: [GannetSymbol] -> [[Char]];
sub bytecodize_payload {my @ppl = @_;
map bytecodize_gs, setNSymbols(@ppl);
}

#bytecodize_gs :: GannetSymbol -> [Char];
sub bytecodize_gs {my @gs =@_;
	my @wordlist=();
#		case kind gs of;
    if ($gs[0] eq 'X') {
			 if ($gs[1]) eq 'i' or $gs[1] == $Symbol_types{'i'}) {
#								GannetTokenB (GannetBuiltinI i) =name gs;
								my $i=$gs[6];
								@wordlist=int_to_bytes($i);
				 } elsif ($gs[1]) eq 'f' or $gs[1] == $Symbol_types{'f'}) {
#								GannetTokenB (GannetBuiltinF f) =name gs;
								my $f=$gs[6];
								@wordlist=flt_to_bytes($f);
						
				 } elsif ($gs[1]) eq 's' or $gs[1] == $Symbol_types{'s'}) {
#								GannetTokenB (GannetBuiltinS s) =name gs;
								my $s=$gs[6];
								@wordlist=str_to_bytes($s);
                } else {
                    die "Unknown datatype $gs[1]"; 
				}
    } else {
			@wordlist=gs_to_bytes(@gs);
	}
	return	map chr, @wordlist;
}



=pod;
32 bits:;
FB_Kind=3 <<5;
FB_Datatype=1 <<4;
FB_Ext=1 <<3;
FB_Quoted=1 <<2;
FB_Task=2 << 0;
FB_Subtask=16;
FB_Name=8;

There is a problem: (fromEnum (datatype gs)) is more than 1 bit, so we must reduce this to 1 bit;
That means we can encode the T_i/T_f difference, but not T_b or, worse, T_s.;
What I can do is use K_Q|T_i to indicate K_B|T_s. The beauty is that we need not change anything to the code!;

=cut;
#gs_to_bytes :: GannetSymbol -> [Word8];
sub gs_to_bytes {$gs =$_[0];
#    let;
#        st :: Word16;
        my $st = $gs->[5];
#        stl :: Word8;
        my $stl=  $st % 256;
        my $sth=  ($st - $stl) / 256;
#        sname :: Integer;
        my $sname = $gs[6];
        if ($gs[6]!~/\d+/) {
             die "Name in " .join(':',@{$gs}). " not properly numerified\n";
             }
#        sn :: Word16;
        my $sn =  $sname;
#        nl :: Word8;
        my $nl= $sn % 256;
if ($WORDSZ==32) {
        my $k=$Symbol_Kind{$gs[0]};
        my $dt=$Symbol_Type{$gs[1]};
        my $kt = ($dt<4)? 2*$k+$dt : 2*$k+($dt & 1);
        my $q =$gs[2];
        my $kteqt=16*$kt + 8*$gs[2] + 4*$q + $gs[3];
        return ($kteqt,$sth,$stl,$nl);
} elsif ($WORDSZ==64) {
        my $k=$Symbol_Kind{$gs[0]};
        my $dt=$Symbol_Type{$gs[1]};
        my $kte= $k*16+$dt*2+$gs[3];
        my $t= $gs[4];
        my $q=$gs[2];
        my $qt= ( $q << 6)+$t;
#        nh :: Word8;
        my $nh= ($sn - $nl) / 256;
        #  FIXME: we don't use Count!;
#        ct :: Word16;
#        ct=fromIntegral $ count gs;
#        cl :: Word8;
#        cl=fromIntegral $ mod ct 256;
#        ch :: Word8;
#        ch=fromIntegral $ div (ct - fromIntegral cl) 256;
 return ($kte,$qt,$sth,$stl,$nh,$nl,0,0);
}
}

#  Neat, what?;
#  though in a more HW-ish language it would just be (n & (0xFF<<x))>>x);
#  and ((0xFFFFFFFF+n) & (0xFF << x))>>x;
#int_to_bytes :: Integer -> [Word8];
sub int_to_bytes {my $n = $_[0];
	if ( $n>0) {
	return map sub {($n & (255 << $_)) >> $_}, @bytes;
	} elsif ( $n==0) { 
	return (0,0,0,0); 	
	} else { 
	return map sub { (($all1s+$n) & (255 << $_)) >>$_}, @bytes;
	}
}

#  can of worms: the GannetBuiltinF type is Double, not Float, so I would need;
#   a conditional there as well.;

#flt_to_bytes :: Double -> [Word8];
sub flt_to_bytes {my $x=$_[0];
		my $fltw = 0;
		if ( $WORDSZ==64) {
		$fltw = encodeIEEE754( $x);
		} else {
				 $fltw = encodeIEEE754_32( $x);
				}
    return		reverse (int_to_bytes($fltw));
}

#str_to_bytes :: [Char] -> [Word8];
sub str_to_bytes {my $str=$_[0];
		my @bytes = map (fromIntegral . ord) str;
		my $nnulls = $nbytes - length(@bytes);

		if ($nnulls>0) {
			return  (@bytes,split('', 0 x $nnulls ));			
			} else {
			return @bytes;
			}
}
#lo :: Integer -> Word8;
sub lo { my $w16 = $_[0];
$w16 % 256;
}

#hi :: Integer -> Word8 -> Word8;
sub hi {(my $w16, my $lo)=@_;
   ($w16 -  $lo) / 256;
}

sub encodeIEEE754 { my $x=$_[0];
return $x;
}

sub encodeIEEE754_32 { my $x=$_[0];
return $x;
}

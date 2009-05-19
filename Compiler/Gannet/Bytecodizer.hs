{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}

-- |Turn Gannet Symbols and Packet headers into bytecode.
-- TODO: Going from Integer to Word8 and then back to Int or Integer for chr is inefficient. 
-- Work with Integer or Int all the way.
module Gannet.Bytecodizer(
	bytecodize,
	intToBytes
) where
import Gannet.SBA.Types
import Gannet.IEEE754

import Data.Bits
import Data.Word
import Data.Char

-- | Turn Gannet Packet into bytes				
bytecodize :: GannetPacket -> [[Char]]
bytecodize gp =
	let
		(gph,gppl)=gp
	in 
		(bytecodize_header gph)++(bytecodize_payload gppl)

{-
32 bits:
FB_Packet_type=3
FB_Prio=3
FB_Redir=2 
FB_Length=8
FB_To=8
FB_Return_to=8
-}

bytecodize_header :: GannetHeader -> [[Char]]
bytecodize_header gph =
    let
        pt = fromIntegral $ fromEnum (ptype gph)
        rd = fromIntegral $ redir gph
        pr = fromIntegral $ prio gph
        GannetLabelI pto = (to gph)
        tl= lo pto
        GannetLabelI rto=(return_to gph)
        rtl=lo rto
#if WORDSZ==64
        prrd = pr*8+rd
        pl= toInteger $ plength gph
        ll=lo pl
        lh= hi pl ll
        th= hi pto tl
        rth=hi rto rtl
        byteword1 :: [Char]
        byteword1=map (chr . fromIntegral) [pt,prrd,lh,ll,th,tl,rth,rtl]
#elif WORDSZ==32
        pt_pr_rd = pt*32+pr*4+rd
        pl= lo $ toInteger $ plength gph
        byteword1 :: [Char]
        byteword1=map (chr . fromIntegral) [pt_pr_rd,pl,tl,rtl]
#endif        
        byteword2 :: [Char]
        byteword2=bytecodize_gs (ack_to gph)
        byteword3 :: [Char]
        byteword3=bytecodize_gs (return_as gph)
    in  
        [byteword1,byteword2,byteword3]


setNSymbols  :: [GannetSymbol] -> [GannetSymbol]
setNSymbols ppl =
        ssymbol:operands
    where
        operator:operands = ppl    
        nsymbols = countNSymbols operands
        ssymbol=operator{subtask=nsymbols}
    
countNSymbols operands = toInteger $ length $ filter (\op -> kind op /= K_X)  operands
   
bytecodize_payload :: [GannetSymbol] -> [[Char]]
bytecodize_payload ppl = map bytecodize_gs (setNSymbols ppl)

bytecodize_gs :: GannetSymbol -> [Char]
bytecodize_gs gs = 
	let wordlist =
		case kind gs of
			K_X -> case datatype gs of
					T_i -> let 
								GannetTokenB (GannetBuiltinI i) =name gs
							in
								int_to_bytes i
					T_f -> let
								GannetTokenB (GannetBuiltinF f) =name gs
							in
								flt_to_bytes f
						
					T_s -> let
								GannetTokenB (GannetBuiltinS s) =name gs
							in
								str_to_bytes s
			otherwise -> gs_to_bytes gs
	in
		map (chr . fromIntegral) wordlist

#if WORDSZ==64
nbytes=8
all1s=18446744073709551616
#elif WORDSZ==32
nbytes=4
all1s=4294967296
#endif

-- [(56,48,40,32,)24,16,8,0]
bytes= take nbytes (iterate (\x->(x-8)) ((nbytes-1)*8) )

{-
32 bits:
FB_Kind=3 <<5
FB_Datatype=1 <<4
FB_Ext=1 <<3
FB_Quoted=1 <<2
FB_Task=2 << 0
FB_Subtask=16
FB_Name=8

There is a problem: (fromEnum (datatype gs)) is more than 1 bit, so we must reduce this to 1 bit
That means we can encode the T_i/T_f difference, but not T_b or, worse, T_s.
What I can do is use K_Q|T_i to indicate K_B|T_s. The beauty is that we need not change anything to the code!

WV15122008: to add Mode and Reg:
 For ACC/BUFFER/CACHE

F_Reg=7 (3 bits)
FS_Reg=10
F_Mode=3 # 0=normal,1=var/acc,2=buf/stream (2 bits)
FS_Mode=14

 We have Subtask == Mode|1Bit|Reg|DataAddress (2|1|3|10) == Mode|1Bit|Reg|2Bits|NArgs

 K_S:(Datatype):(Ext):(Quoted):Task:Mode|1Bit|Reg|2Bits|NArgs:SCId|Opcode
 K_D:(Datatype):(Ext):Quoted:Task:Mode|1Bit|Reg|10Bits:Name


-}
gs_to_bytes :: GannetSymbol -> [Word8]
gs_to_bytes gs =
    let
        reg_field=(reg gs)
        (regvar,regval)=reg_field
        modeval =fromIntegral $ fromEnum (mode gs)
        stval
            | (kind gs)==K_S || (kind gs)==K_R || (kind gs)==K_C || (kind gs)==K_D
                = (subtask gs) + modeval*16384+(fromIntegral regval)*1024
            | otherwise = subtask gs
        st :: Word16 -- FIXME: this is surely not correct for 64 bits?
        st = fromIntegral $ stval
--            | kind gs == K_B = 0
--            | otherwise = fromIntegral $ subtask gs
        stl :: Word8
        stl= fromIntegral $ mod st 256
        sth= fromIntegral $ div (st - fromIntegral stl) 256    
        sname :: Integer
        sname = case name gs of
            GannetTokenL (GannetLabelI li) -> li
            GannetTokenB (GannetBuiltinI bi) -> bi
            GannetTokenS (GannetLabelI sli) -> sli
            otherwise -> error $ "Name in " ++ (show gs) ++ " not properly numerified\n"            
        sn :: Word16
        sn = fromIntegral sname
        nl :: Word8
        nl=fromIntegral $ mod sn 256    
#if WORDSZ==32
        k=fromEnum (kind gs)
        dt=fromEnum (datatype gs)
        kt
            | dt<4 = 2*k+dt -- 12+0 = 110|0, 12+1 = 110|1, 12+2 = 111|0, 12+3 = 111|1       
            | otherwise = 2*k+(dt.&.1)
        q -- This is very late to quote strings, but better late than never :-)
--            | datatype gs == T_s = 1 -- somehow builtins got unquoted by moving from Name to Subtask. I have no time to figure out why
            | kind gs == K_B = 1
            | otherwise = quoted gs
        kteqt=fromIntegral $ 16*kt + 8*(ext gs) + 4*q + (task gs) 
#elif WORDSZ==64
        kte=fromIntegral $ (fromEnum (kind gs))*16+(fromEnum (datatype gs))*2+(ext gs) 
        t=task gs        
        q=quoted gs
        qt=fromIntegral $ (shiftL q 6)+t
        nh :: Word8
        nh=fromIntegral $ div (sn - fromIntegral nl) 256
        -- FIXME: we don't use Count!
        ct :: Word16
        ct=fromIntegral $ count gs
        cl :: Word8
        cl=fromIntegral $ mod ct 256
        ch :: Word8
        ch=fromIntegral $ div (ct - fromIntegral cl) 256
#endif
    in
#if WORDSZ==32
        [kteqt,sth,stl,nl]
#elif WORDSZ==64
        [kte,qt,sth,stl,nh,nl,ch,cl]
#endif		
-- Neat, what?
-- though in a more HW-ish language it would just be (n & (0xFF<<x))>>x)
-- and ((0xFFFFFFFF+n) & (0xFF << x))>>x
int_to_bytes :: Integer -> [Word8]
int_to_bytes n 
	| n>0 = map (fromIntegral . (\x->(shiftR (n .&. (shiftL 255 x)) x))) bytes
	| n==0 = replicate 4 (fromIntegral 0)
	| otherwise = map (fromIntegral . (\x->(shiftR ((all1s+n) .&. (shiftL 255 x)) x))) bytes


-- can of worms: the GannetBuiltinF type is Double, not Float, so I would need 
--  a conditional there as well.
#if WORDSZ==64
flt_to_bytes :: Double -> [Word8]		
flt_to_bytes x = 
	let
		fltw = encodeIEEE754 x
	in
		reverse (int_to_bytes fltw)
#elif WORDSZ==32
flt_to_bytes :: Float -> [Word8]		
flt_to_bytes x = 
	let
		fltw = encodeIEEE754_32 x     
	in
		reverse (int_to_bytes fltw)
#endif

str_to_bytes :: [Char] -> [Word8]
str_to_bytes str = 
	let
		bytes = map (fromIntegral . ord) str
		nnulls = nbytes - (length bytes)
	in
		if nnulls>0
			then bytes++(replicate nnulls 0)
			else bytes

lo :: Integer -> Word8
lo w16 = fromIntegral $ mod w16 256

hi :: Integer -> Word8 -> Word8			
hi w16 lo= fromIntegral $ div (w16 - fromIntegral lo) 256

-- | We assume that the Integer is actually Word16.
-- This is only used for the number of packets in a .tdc
-- just map (chr . fromIntegral) (int_to_bytes i) would do fine I think.
intToBytes :: Integer -> [Char]
intToBytes i =
    let
        il = lo i
        ih = hi i il
        nzeros=nbytes-2
    in
        map (chr . fromIntegral) ([ih,il]++(replicate nzeros 0))

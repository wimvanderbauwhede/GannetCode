{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -D_WORDSZ=32 #-}

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
-- for both 32- and 64-bit, Subtask is 16 bits
-- However, 32-bit K_B's use up the LSB!
-- So purely for consistency, for K_B I should set the Subtask to 0?         
        stval
            | (kind gs)==K_S || (kind gs)==K_R || (kind gs)==K_C || (kind gs)==K_D
                = (subtask gs) + modeval*16384+(fromIntegral regval)*1024
            | otherwise = subtask gs                        
        st :: Word16 -- FIXME: this is surely not correct for 64 bits?
        st = fromIntegral $ stval
        stl :: Word8
        stl
        	| kind gs /= K_B = fromIntegral $ mod st 256
            | kind gs == K_B && datatype gs == T_s = fromIntegral st -- used for padding so < 256 
        	| otherwise = 0 -- FIXME: ugly hack!!
        sth= fromIntegral $ div (st - fromIntegral stl) 256    
        sname :: Integer
        sname = case name gs of
            GannetTokenL (GannetLabelI li) -> li
            GannetTokenB (GannetBuiltinI bi) -> bi
            GannetTokenS (GannetLabelI sli) -> sli
            otherwise -> error $ "Name in " ++ (show gs) ++ " not properly numerified\n"
#if WORDSZ==32                        
        sn :: Word16
        sn = fromIntegral sname
        nl :: Word8
        nl=fromIntegral $ mod sn 256    
        -- FIXME: if the Symbol is a non-extended K_B and Name > 8 bits, we must
        -- use 2nd byte to store the rest of the number
        nh :: Word8
        nh=fromIntegral $ div (sn - fromIntegral nl) 256
#else
-- 64 bits
        sn :: Word32
        sn = fromIntegral sname 
        n3 :: Word8
        n3=fromIntegral $ (sn `shiftR` 24) .&. 255
        n2 :: Word8
        n2=fromIntegral $ (sn `shiftR` 16) .&. 255
        n1 :: Word8
        n1=fromIntegral $ (sn `shiftR` 8) .&. 255
        n0 :: Word8
        n0=fromIntegral $ sn .&. 255
        
#endif             
        e=(ext gs).&.1
        k
        	| kind gs /= K_Unknown =fromEnum (kind gs)
        	| otherwise = 0
        dt=fromEnum (datatype gs)   
        q -- This is very late to quote strings, but better late than never :-)
            | kind gs == K_B = 1
            | otherwise = (quoted gs).&.1
        t=task gs               
#if WORDSZ==32                
        kt = 2*k+(dt.&.1)
--            | dt<4 = 2*k+dt -- 12+0 = 110|0, 12+1 = 110|1, 12+2 = 111|0, 12+3 = 111|1       
--            | otherwise = 2*k+(dt.&.1)
        kteqt=fromIntegral $ 16*kt + 8*e + 4*q + t -- i.e. kt<<4 + e<<3 + q<<2 + t
#elif WORDSZ==64
        kte=fromIntegral $ (k.&.15)*16+(dt.&.0x7)*2+e -- kte is a byte 4|3|1      
        qt=fromIntegral $ (shiftL q 6)+t -- qt is a byte 2|6
#endif
    in
#if WORDSZ==32
		if k==(fromEnum K_B) && (e==0) -- for not-extended built-ins 
			then [kteqt,sth,nh,nl]
			else [kteqt,sth,stl,nl]        
#elif WORDSZ==64
        [kte,qt,sth,stl,n3,n2,n1,n0]
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
#elif WORDSZ==32
flt_to_bytes :: Float -> [Word8]
#endif		
flt_to_bytes x = 
	let
		fltw = encodeIEEE754 x
	in
		reverse (int_to_bytes fltw)

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

-- We assume that the Integer is actually Word16.
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

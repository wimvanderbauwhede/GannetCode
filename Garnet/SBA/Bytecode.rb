#/** \file Bytecode.rb
#   
# \brief Service-based SoC project - Service Configuration module
#
#*/
#
#/* ***** BEGIN LICENSE BLOCK *****
# * Version: AFL 2.1
# *
# * The contents of this file are subject to the Academic Free License Version
# * 2.1 (the "License") you may not use this file except in compliance with
# * the License. You may obtain a copy of the License at
# * http://opensource.org/licenses/afl-2.1.php
# *
# * Software distributed under the License is distributed on an "AS IS" basis,
# * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# * for the specific language governing rights and limitations under the
# * License.
# *
# *  (c) 2004-2008 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
# *
# * ***** END LICENSE BLOCK ***** */
#
#//==============================================================================
#//
#// Service-based SoC project - Auxiliary stuff for emitting bytecode
#//
#//==============================================================================
#
#// $Id: Bytecode.rb 2263 2009-02-19 11:48:28Z socgroup $

#skip
module SBA_Bytecode #skip
    include Math

        def nbytes    
            if WORDSZ==32
                return 4
            else
                return 8
            end
        end
    # pad list of bytes with 0's to get 8-byte word
    def byteword(num) #t Word_List (uint64)
        if num.is_a?(Integer) #skip
            bw=[] #t Word_List
            width=255 #t uint


            for i in 0..nbytes-1 #C++ for (uint i=0;i<nbytes;i++) {
                offset=(nbytes-1-i)*8 #t uint
                j=i # nbytes-1-i for other endianness
                bw[j]=(num>>offset) & width
            end            
            return bw
        else #skip
            raise "Only integers supported so far: #{num} ,#{num.inspect}" #skip
        end #skip        
    end      
    
    def byteword2num(bw) #t uint64 (Word_List)
        v=0 #t uint64
        i=nbytes-1 #t uint
        for b in bw #t Word_List
            v+=b*(256**i) #C++ v+=b*pow2(8*i);
            i-=1
        end
        return v
    end
    
    def bitslice(n,offset,width,*arg)
        if arg.length==0
            mask= (2**width-1)
            field= (n>>offset) & mask
            return field
        else
            old=n & ((2**width-1)<<offset)
            new=arg[0]<<offset
            return n-old+new
        end
    end

end #skip
#endskip

#skipcc
=begin #si1
#include <cmath>

#include "Types.h"

//using namespace std;
using namespace SBA;

namespace SBA {

//Word pow2(uint bits);
Word bitmask(uint8 n,uint8  m);
Word getrange(Word w,uint8 n,uint8  m);
Word field(uint8 n,uint8 m, uint f);
Word setrange(Word w, uint8 n, uint8 m,uint f);

=end #ei1
#endskipcc

#skiph
=begin #si2

#include "Bytecode.h"

//using namespace std;
//using namespace SBA;
/*
Word SBA::pow2(uint bits) {
if (bits>0) {
    return (Word)std::pow((double)2,(double)bits);
    } else {
    return (Word)1;
    }
}
*/
Word SBA::bitmask(uint8 n,uint8  m) {
/*
        Word bm=pow2(m);
        for (unsigned short i=n;i>m;i--) {
                bm+=SBA::pow2(i); // why not shifting?
        }
*/        
        Word bm=(((Word)1<<(1+n-m))-(Word)1)<<m;       
        return bm;
}

Word SBA::getrange(Word w,uint8 n,uint8  m) {
  if (m>0) {
        return (w & SBA::bitmask(n,m))>>m;
        } else {
        return w & SBA::bitmask(n,m);
        }
}

Word SBA::field(uint8 n,uint8 m, uint f) {
return ((Word)f<<m) & SBA::bitmask(n,m);
}

Word SBA::setrange(Word w, uint8 n, uint8 m,uint f) {
/* must do w - [current value of field] + field(n,m,f) */
    Word cf=SBA::getrange(w,n,m);
    Word nw=w-cf;
    nw=nw+SBA::field(n,m,f);
    return nw;
}
=end #ei2
#endskiph
#H }


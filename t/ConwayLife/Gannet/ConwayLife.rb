# ConwayLife.rb
#   
# :title: Gannet Service-based SoC project - Service Core Library for Conway's Game of Life 
#    
#--
#
# *
# *  (c) 2004-2012 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#++

# -----------------------------------------------------------------------------

=begin #inc
#include <fstream>
#include <sstream>
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/ServiceCore.h" //skipcc
#include "System.h" //skiph
#include "Tile.h" //skiph
#include "ServiceCore.h" //skiph
#include "SBACore.h"
=end #inc 

=begin
# This module contains the implementations of all SBA services. 
# Services starting with 'ls_' are language services
=end

require "SBACore.rb"

# module SBA_SCLib
# no more _SCLib

#C++ using namespace std;

#skipcc
#C++ namespace SBA {
#C++ namespace ConwayLife {
#endskipcc

#skiph
#C++ using namespace SBA; 
#endskiph
class ConwayLife < SBACore #skip
  
#ifndef NO_SERVICES  

    def cs_CONWAY_LIFE_CORE(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
         
        operation=parent.opcode #t uint    
        #C++ Word_List result_list;
        res_symbol=EXTSYM  #C++ const Word res_symbol = EXTSYM;
        res_data=0 #t Word
        
        bloksize=128
        # Instead of parent.init we could also have an init method for the service
        # in the Gannet-C code this might be the service constructor, so INITIALIZE or NEW is good too
        # so A_INIT  would be called once, resulting in creation of the object
        # In fact, a Gannet-C service is an interface, so we could use the SC_ name as the initialize method
        # So what we would have is 
        # when SC_LOOPSERVICE 
        #     service=LoopService.new(parent,lstart,lstop,res_data)
        if parent.init()
            # create service object           
            service=ConwayLife.new(parent,bloksize) #C++ LoopService* service = new LoopService(parent,lstart,lstop,res_data); 
            parent.store(service)            
        else
            # load service object
            service=parent.load()
        end
        
        case operation 
        when M_ConwayLife_CONWAYLIFE_init            
            service.init(...)
        when M_ConwayLife_CONWAYLIFE_...
            res_data=service. ...(...)
        else #C++ default:
            raise "Unknown service: #{operation}" 
            #C++   exit(0);
        end #;
        
        if !service.done()     
            # store service object. I wonder if I need to do this only once?
            # Also, maybe we can merge suspend and store
            
        #            parent.suspend()
            return []
        else
            puts "RESULT: #{res_data}"
            result_list=[res_symbol,res_data]  #C++ result_list.push_back(res_data);
            #C++ delete(service);
            return result_list
        end
    end # of cs_CONWAY_LIFE
    
    def cs_CONWAY_LIFE_IO()
        
    end # of cs_CONWAY_LIFE_IO
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------     
#         
# this is a dummy for unused services
    def none(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
    #    return 0 #C++ Result res; res.push_back((Word)0); return res;
    end        


end #skip

=begin

# --------------------------------------------------------------------------
class ConwayLifeCore
#    attr_accessor 
    def initialize ()
        @new_imgblock = ImgBlock.new([],_bsz)
        @crow=1
        @bsz=_bsz
    end
    def init(_imgblock,_bsz)
        @imgblock = ImgBlock.new(_imgblock,_bsz)
    end
    #    def calc_core()           
    #        for ccol in 1..@bsz-2
    #            # calculate new values for crow and ccol
    #        end
    #        @crow+=1
    #        if @crow<@bsz-2
    #            @sys.iterate()
    #        end
    #    end
    # To calculate the core values we don't need neighbouring edges
    def calc_core()
        for crow in 1..@bsz-2
            for ccol in 1..@bsz-2
                life_rules_core(crow,ccol)                 
            end
        end
    end    
    # FIXME: for the 
    def calc_edges(tlc,trow,trc,rcol,brc,brow,blc,lcol)        
       for crow in 1..@bsz-2
           # left edge
           life_rules(crow,0)  
           # right edge
           life_rules(crow,@bsz-1)
       end       
       for ccol in 0..@bsz-1
           # top row
           life_rules(0,col)
           # bottom row
           life_rules(@bsz-1,col)
       end 
    end
    
    # Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    # Any live cell with two or three live neighbours lives on to the next generation.
    # Any live cell with more than three live neighbours dies, as if by overcrowding.
    # Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction
    def life_rules_core(x,y)
        if n_neighbours_core(x,y)<2 or n_neighbours_core(x,y) >3
            @new_imgblock.write(x,y,0)
        elsif n_neighbours_core(x,y)==3
            @new_imgblock.write(x,y,1)
        end
    end 
    
    def n_neighbours_core(x,y)
        n=0;
        for i in x-1..x+1
            for j in y-1 .. y+1
                if i!=x and j!=y and @imgblock.read(i,j)==1
                    n+=1
                end
            end
        end
        return n
    end   
             
    def brc()
        return [@imgblock.read(@bsz-1,@bsz-1)]
    end
    
    def brow()
        return @imgblock.row(@bsz-1)
    end
    
    def blc()
        return [@imgblock.read(@bsz-1,0)]
    end
        
    def lcol()
        return @imgblock.col(0)
    #        lc=[]
    #        for r in 0..@bsz-1
    #            lc[r]=  @imgblock[r][0]
    #        end
    #        return lc
    end
    
    def tlc()
        return [@imgblock.read(0,0)]
    end
    
    def trow()
        return @imgblock.row(0)
    end
        
    def trc()
        return [@imgblock.read(0,@bsz-1)]
    end
        
    def rcol()
        return @imgblock.col(@bsz-1)
    #        rc=[]
    #        for r in 0..@bsz-1
    #            rc[r]=  @imgblock[r][@bsz-1]
    #        end
    #        return rc        
    end
    def block()
        return @new_imgblock
    end
    def done()
        return (@crow>=@bsz-1)
    end
end # END of ConwayLifeCore

# --------------------------------------------------------------------------
# ImgBlock is a 4x128-Word Word_List masquerading as a 128x128 matrix of bits
class ImgBlock
    def initialize(_imgblock,_bsz)
        @bsz=_bsz
        @block=_imgblock
    end
    def write(x,y,b)
        # in which word is this?
        word = ( ( x - x % WORDSZ ) / WORDSZ ) + y * ( @bsz / WORDSZ )
        bitpos = x % WORDSZ
        @block[word] = (@block[word] & (F_Symbol-(1<<bitpos)))+(b<<bitpos)        
    end
    
    def read(x,y)
        word = ( ( x - x % WORDSZ ) / WORDSZ ) + y * ( @bsz / WORDSZ )
        bitpos = x % WORDSZ
        return (@block[word]>>bitpos)&0x1         
    end
    # these methods return a list of @bsz/WORDSZ words
    def row(r)
        rw=[]
        for w in r*(@bsz/WORDSZ) .. (r+1)*(@bsz/WORDSZ)-1
            rw.push(@block[w])
        end
        return rw
    end
    
    def col(c)
        cl=[]
        w=0
        for r in 0..@bsz-1
            bit=read(r,c)
            rs=r%WORDSZ
            w+=bit<<rs
            if rs==WORDSZ-1
                cl.push(w)
                w=0
            end
        end
        return cl
    end
end # END of ImgBlock

#skipcc
#C++ }} // namespaces       
#endskipcc
#endif // NO_SERVICES

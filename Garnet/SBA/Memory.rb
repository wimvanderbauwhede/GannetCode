#   
# :title: Gannet Service-based SoC project - SBA Memory class
#
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
# $Id: Memory.rb 2532 2009-04-22 16:15:08Z socgroup $
#++


# This class is a model for the data store of the SBA Service Manager.
# It is actually a wrapper around an SBA_Store 
# SBA_Memory#read and SBA_Memory#write are aliases for SBA_Store#get and SBA_Store#put
# SBA_Memory#is_free? is the inverse of SBA_Store#has

#skip

# SBA_Store needs a header as we want to organise it as a linked list;
# even if we don't, we need to store some status information.
# So the first word of any Linked list mem block should contain some status information
# the first word of the chunk must contain a pointer to the next address,
# as well as a mask indicating 
# - this chunk is full/not full: 1 bit
# - this LL only contains a single value? or is every chunk a list element? 1 bit
# - status of the data in the chunk: 2 bits
# the "next address" should have a range of say 1M at least 
# so let's say we use a 20-bit word for the pointer and 4 bits for the mask
# Assuming chunk sizes of 64 words or less, the number of used words can be expressed in 6 bits (i.e. 2**6)
# Assuming 32-bit word store, we also need 2 bits to indicate the length of the last word in bytes. if 0, it's 4
# So for a single-linked list we have 20+4+6+1+1=32. 
# Now create a nice convenient object for this header word
# 
# For the usual high-level model we actually only use Data_Status
# The SBA_Chunk_Header is always at address 0 for anything stored using mput.
class SBA_Chunk_Header
attr_accessor :Streaming, :Data_Status, :Subtask, :Next_Addr, :Full, :List, :Used_Words, :Last_Word_Size
    def initialize()
        @Streaming=0;
        @Data_Status=0;
        @Subtask=0;
        # For linked list. Physically a separate word in memory
        @Next_Addr=0
        @Full=0;
        @List=0;        
        @Used_Words=0;
        @Last_Word_Size=0;
    end
end

class SBA_Memory 

    def initialize
        @store=SBA_Store.new()
    end
    # RAM Write operation	
    def write(address,data)
        @store.put(address,data)
    end
    
    # RAM Read operation
    def read(address)
        return @store.get(address)
    end
    
    # Check if address is free
    def is_free?(address)
        return (@store.has(address)?0:1)
    end
    
    # Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
    def remove(address)
        @store.remove(address)
    end
    
    # For monitoring
    def utilized
        return @store.utlised
    end	
end

# This class is a model for the RAM memory of the SBA Service Manager.
# SBA_Store#has and SBA_Store#remove could be implemented by a single bit at the start of the word
# Later we can create a proper memory-managed store, e.g. with Knuths tagged approach

class SBA_Store
attr_reader :store
    def initialize
        @store={}
    end

    # -Either we sneakily add a header as first element
    # -Or we require data given to mput to contain the header
    # -Or we use a structure [header,data]
    # In HW, the data would be written transparently. Accessing the status info could be a special operation.
    # So maybe we do this:
    def init(address)
#        header=SBA_Chunk_Header.new()
#        @store[address]=[header]
        @store[address]=[]
    end
    
    def status(address,*value)
        if not @store[address].is_a?(Array)
#            @store[address]=[SBA_Chunk_Header.new(),[]]
            @store[address]=[[]]
        end
        if value.length==0
            return @store[address][0].Data_Status
        else
            @store[address][0].Data_Status=value[0]
        end
    end    

    def subtask(address,*value)
        if not @store[address].is_a?(Array)
#           @store[address]=[SBA_Chunk_Header.new(),[]]
           @store[address]=[[]]
        end
    if value.length==0
        return @store[address][0].Subtask
        else
        @store[address][0].Subtask=value[0]
        end
    end    

    def streaming(address,*value)
    if value.length==0
#        return @store[address][0].Streaming
        0
        else
#        @store[address][0].Streaming=value[0]
        end
    end    
    
    # RAM Write operation for list of words
    def mput(address,data)
        if data.is_a?(Array) and (data[0].is_a?(Integer) or data==[])
            if not @store[address].is_a?(Array)
#                @store[address]=[SBA_Chunk_Header.new(),[]]
                @store[address]=[[]]
            end
            @store[address][1]=data
        else
            raise "Trying to mput wrong data type in store: #{data.class}, #{data[0].class} "
        end
    end
    
    # RAM Read operation for list of words
    def mget(address)
        if not @store[address].is_a?(Array)
#    @store[address]=[SBA_Chunk_Header.new(),[]]
       @store[address]=[[]]
        end
    if @store[address][1].is_a?(Array) and @store[address][1][0].is_a?(Integer)
        return @store[address][1]
        else
        return []
#        raise "Trying to get wrong data type from store: #{@store[address].class}, #{@store[address][1][0].class}"
        end
    end
    
    # RAM-FIFO Push operation for list of words
    def mpush(address,data)
    raise "FIFO Push"
    if data.is_a?(Array) and data[0].is_a?(Integer)
        @store[address][1]=[] unless @store.has_key?(address)
        @store[address][1].push(data)
        else
          raise "Trying to mput wrong data type in store: #{data.class}, #{data[0].class} "
        end
    end

    # RAM-FIFO Shift operation for list of words
    def mshift(address)
     raise "FIFO Shift"
    if @store[address][1].is_a?(Array) and @store[address][1][0].is_a?(Integer)
        return @store[address][1].shift
        else
        raise "Trying to get wrong data type from store: #{@store[address].class}, #{@store[address][0].class}"
        end
    end

    # RAM-FIFO Head operation for list of words
    def mhead(address)
     raise "FIFO Head"
    if @store[address][1].is_a?(Array) and @store[address][1][0].is_a?(Integer)
        return @store[address][1][0]
        else
        raise "Trying to get wrong data type from store: #{@store[address].class}, #{@store[address][0].class}"
        end
    end
            
    # RAM Write operation for single word	
    def put(address,data)
    if data.is_a?(Integer)     
        @store[address][1]=data
        else   
        raise "Trying to put wrong data type in store: #{@store[address].class}"
        end
    end

    # RAM Read operation for single Word
    def get(address)    
    if @store[address][1].is_a?(Integer)
        return @store[address][1]
        else
        raise "Trying to get wrong data type from store: #{@store[address].class}"
        end
    end
    
    # Check if address is in use
    def has(address)
        return @store.has_key?(address)
    end
    
    # Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
    def remove(address)   
        @store.delete(address)
# puts "DEL ASSIGN #{address}: #{@store.inspect}"        
    end
    
    # For monitoring
    def utilised
        return @store.keys.length
    end
    
    def dump
            for address in @store.keys
        puts "#{address}:\t#{@store[address].inspect}"
        end
    end
end # of SBA_Store
# -----------------------------------------------------------------------------
=begin A RAM-resident stack model.
We create a stack on the data store starting at stack_address+1
stack_address holds the stack pointer.
the stack pointer is relative to stack_address, so 0 means the stack is full and pop() will return the value at stack_address+1
pop() means: read the value at the stack pointer address; increment the stack pointer
push() means: write the value at the stack pointer address; decrement the stack pointer
size() returns the value of the stack pointer 
-------
stack_pointer @ stack_address
-------
addr1 @ stack_address+1; 
addr2 @ stack_address+2
addr3 @ stack_address+3
...
addr_stacksize  @ stack_address+stack_size
-------
... (offset)
------
loc[i] @ addr[i] = stack_address+stack_size+offset+i
=end
module SBA_RAM_Stack
    # Fill the stack with addresses
    def SBA_RAM_Stack.init(sba_tile,stack_address,stack_size,offset)
        for i in 1..stack_size
            sba_tile.data_store.put(stack_address+i,i+offset+stack_size+stack_address)
        end
        sba_tile.data_store.put(stack_address,0)
    end
    
    # Pop an address to use off the stack.
    def SBA_RAM_Stack.pop(sba_tile,stack_address,stack_size)
        stack_pointer=sba_tile.data_store.get(stack_address)
        if stack_pointer==stack_size
            puts "ERROR: RAM Stack is empty!"
            exit
        end        
        address=sba_tile.data_store.get(stack_address+stack_pointer+1)
        stack_pointer+=1
        sba_tile.data_store.put(stack_address,stack_pointer)
        return address
    end
    
    # Push a freed address back onto the stack.
    def SBA_RAM_Stack.push(sba_tile,stack_address,address)
        stack_pointer=sba_tile.data_store.get(stack_address)
        if stack_pointer==0
            puts "ERROR: RAM Stack is already full!"
            exit
        end
        sba_tile.data_store.put(stack_address+stack_pointer,address)
        stack_pointer-=1
        sba_tile.data_store.put(stack_address,stack_pointer)
        return stack_pointer
    end
    
    
    def SBA_RAM_Stack.size(stack_address,stack_size)
        stack_pointer=sba_tile.data_store.get(stack_address)
        return stack_size-stack_pointer
    end
    
    
end # of SBA_RAM_Stack
#------------------------------------------------------------------------------
=begin A RAM-resident FIFO model.
We create a FIFO on the data store starting at fifo_address+3
fifo_address holds the push_pointer.
fifo_address+1 holds the shift_pointer.
fifo_address+2 holds the status.
fifo_address+3 holds the occupancy.
the pointers are relative to fifo_address, so 0 means the fifo is empty 
and push() will return the value at fifo_address+1
shift() means: read the value at the  pointer address; increment the  pointer
push() means: write the value at the  pointer address; decrement the  pointer
size() returns the number of adresses in use
-------
=end
module SBA_RAM_Fifo
    # Fill the fifo with addresses
    def SBA_RAM_Fifo.init(sba_tile,fifo_address,fifo_size)
        sba_tile.data_store.put(fifo_address,0) # push_address
        sba_tile.data_store.put(fifo_address+1,0) # shift_address
        sba_tile.data_store.put(fifo_address+2,2) # status: 10 (2)=>  push_ok; 11 (3)=> shift_ok; push_ok; 01 (1)=> shift_ok; 00 (0)=> trouble        
        sba_tile.data_store.put(fifo_address+3,0) # occupancy
    end
    
    # Push a value onto the fifo.
    def SBA_RAM_Fifo.push(sba_tile,fifo_address,fifo_size,value)
        fifo_status=sba_tile.data_store.get(fifo_address+2)
#        puts fifo_status
        if fifo_status<=1
            raise "ERROR: FIFO Stack is full!"
        end    
        fifo_push_pointer=sba_tile.data_store.get(fifo_address)
        sba_tile.data_store.put(fifo_address+4+fifo_push_pointer,value)
#        puts "Stored #{value} at #{fifo_address+4+fifo_push_pointer}"
        fifo_push_pointer=incr(fifo_push_pointer,fifo_size)
        
        occ=sba_tile.data_store.get(fifo_address+3)
        occ+=1
        sba_tile.data_store.put(fifo_address+3,occ)
        
        sba_tile.data_store.put(fifo_address,fifo_push_pointer)        
        
        sba_tile.data_store.put(fifo_address+2,set_status(occ,fifo_size))
    end
    
    # Shift a value to use off the fifo.
    def SBA_RAM_Fifo.shift(sba_tile,fifo_address,fifo_size)
        fifo_status=sba_tile.data_store.get(fifo_address+2)
        if fifo_status==2 or fifo_status==0
            puts "ERROR: FIFO Stack is empty!"
            exit        
        end    
        
        fifo_shift_pointer=sba_tile.data_store.get(fifo_address+1)
        value=sba_tile.data_store.get(fifo_address+4+fifo_shift_pointer)
        fifo_shift_pointer=incr(fifo_shift_pointer,fifo_size)
        
        occ=sba_tile.data_store.get(fifo_address+3)
        occ-=1
        sba_tile.data_store.put(fifo_address+3,occ)
        
        sba_tile.data_store.put(fifo_address+1,fifo_shift_pointer)
        
        sba_tile.data_store.put(fifo_address+2,set_status(occ,fifo_size))
        return value
    end
    
    def status(fifo_address)
        return   sba_tile.data_store.get(fifo_address+2)
    end
    
    def SBA_RAM_Fifo.size(fifo_address)
        return sba_tile.data_store.get(fifo_address+3)
    end
    
    def SBA_RAM_Fifo.set_status(occ,fifo_size)
        status=3
        if occ==0
            status=2 # only push         
        elsif occ==fifo_size
            status=1 # only shift
        end
        return status
    end # set_status
    
    def SBA_RAM_Fifo.incr(pointer,fifo_size)
        pointer+=1
        if pointer==fifo_size
            pointer=0
        end
        return pointer
    end # incr
    
end # of SBA_RAM_Fifo
#endskip

#skipcc
=begin #h
#ifndef STATIC_ALLOC
#ifdef WV_SYSTEMC
#include <systemc.h>
#endif // WV_SYSTEMC
#include "Types.h"

using namespace std;

namespace SBA {

/**
 This class is a model for the RAM memory of the SBA Service Manager.
 It is a simple wrapper around a Store object
 Memory#read and Memory#write are aliases for Store#get and Store#put
 Memory#is_free is the inverse of Store#has
 */ 	
 
class Memory : public Store {
	public:
/*
 Main methods
*/

	/// RAM Write operation	
	void write(MemAddress address,Data data);
	/// RAM Read operation
	Data read(MemAddress address);
	/// Check if address is free
	bool is_free(MemAddress address);
};

} // namespace SBA
#endif
=end #h
#endskipcc

#skiph
=begin #cc
#ifndef STATIC_ALLOC
#include "Memory.h"

using namespace std;
using namespace SBA;
 	

	//* RAM Write operation	
	void Memory::write(MemAddress address,Data data) {
		mput(address,data);
	}
	
	//* RAM Read operation
	Data Memory::read(MemAddress address) {
		return mget(address);
	}
#ifndef STATIC_ALLOC_	
	//* Check if address is free
	bool Memory::is_free(MemAddress address) {
		return (has(address)?false:true);
	}
#endif	
#endif
=end #cc
#endskiph
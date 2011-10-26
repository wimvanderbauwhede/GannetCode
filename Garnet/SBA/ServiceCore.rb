#   
# Gannet Service-based SoC project - SBA Service Core class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: ServiceCore.rb 2547 2009-05-08 11:28:47Z socgroup $

=begin #inc
#include "Base/System.h" //skipcc
#include "Base/Tile.h" //skipcc
#include "Base/ServiceCore.h" //skipcc
#include "ServiceConfiguration.h" //skipcc
#include "System.h" //skiph
#include "LookupTable.h" //skipcc
#include "Tile.h" //skiph
#include "ServiceCore.h" //skiph
=end #inc

=begin
# WV10012011: remove support for multi-threaded core. See -r 4232 or earlier for the code
# WV20110609: for the old configuration behaviour revert to before r4987 and set NEW=0 and NEWER=0
=end

class SBA_ServiceCore
    include SBA

#H    	Base::System* sba_system_ptr;
#H    	Base::Tile* sba_tile_ptr;  
    attr_accessor :state_register,#t State_Register;
                    :lookup_table, #t LookupTable;
                    :core_status,:core_return_type,:ack_ok,:n_args #t Core_Status;Packet_Type;uint;uint;
    attr_reader :service,:nservice,:address,:current_subtask,:tid,:scid,:sclid,:opcode,:verbose #t Service;Service;ServiceAddress;Subtask;uint;uint;uint;uint;bool;
 #skipcc    
=begin #constructor
        ServiceCore(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) :
            sba_system_ptr(sba_s_), 
            	sba_tile_ptr(sba_t_),             	           	           	
            	core_status(CS_idle),
                service(s_), address(addr_), 
                current_subtask(0), 
                tid(tid_),
                scid(0)
			{
                
			};
=end #constructor
#endskipcc

=begin
Constructor
=end

#skip 
    def initialize(sba_system,sba_tile,service_,tid_)
        @sba_system=sba_system
        @sba_tile=sba_tile
        @service=service_
        # We add a simple state register to the core. For data storage it should use the tile memory
        @state_register=[0,0] #skip
        @lookup_table=SBA_LookupTable.new() 
        @current_subtask=0
        @tid=tid_
        @scid=sba_tile.service_manager.scid        
        @sclid=sba_tile.service_manager.sclid
#        puts "SCId: #{@scid}"
        @core_status=CS_done # 3 bits
        @core_return_type=P_data # 3 bits
        @ack_ok=1 #t bool
        @opcode=0 #t 8 bits
        @n_args=0 # 3 bits 
        @verbose=(VERBOSE==1)      
    end
#endskip
# ------------------------------------------------------------------------------
    # This method mimics/models the Service Core.

=begin
In the HW implementation, the ServiceCore should be part of the ServiceManager, it should be called ServiceCoreInterface anyway.
The actual service core will have yet another wrapper which has following ports:
-an input port data_rdy
-an output port result_rdy
-an input port [8bits] opcode
-an input FIFO arg_addresses [10bits]
-an output FIFO res_values [32bits]

The state transition is:
if CS_idle -> CS_ready -> CS_busy => set data_rdy; core takes data & resets data_rdy (ServiceMgr doesn't care); 
if result_rdy -> CS_done

For a HW core in the SW ServiceManager, we have CS_managed until 
=end    

    def run() # sba_system,sba_tile
    #tile       
        @scid=@sba_tile.service_manager.scid        
        @sclid=@sba_tile.service_manager.sclid
        if @sba_tile.service_manager.core_status==CS_busy 
            @current_subtask=@sba_tile.service_manager.current_subtask
               
            # The service core in the real SBA will of course have been configured in advance.
            # Here we use the service name to lookup the actual function which implements the core

            # Actual call to the CORE
#            #C++ cout << "Calling CORE "<< service <<"\n";
#            puts @sba_tile.service_manager.arg_addresses.inspect #skip
#iv
            if @verbose #skip
            puts "#{service} ServiceCore: #{@sba_tile.service_manager.arg_addresses.length()} addresses"
            end #skip 
#ev
# In HW, some event is required to set the CS_done flag, e.g. the resuls_store is filled
# I put the status change before the call as this allows the core to set the status if it likes to

            @sba_tile.service_manager.core_status=CS_done
            @core_status=CS_done
            #FIXME: make service_id a separate register in the ServiceManager
            service_id=@sba_tile.service_manager.service_id #t Service
            @service=service_id
            @n_args=@sba_tile.service_manager.n_args
            @core_status=@sba_tile.service_manager.core_status
            @core_return_type=@sba_tile.service_manager.core_return_type
            @ack_ok=@sba_tile.service_manager.ack_ok         
            @opcode=@sba_tile.service_manager.opcode 
#iv
            if @verbose #skip
            puts "CALL TO CORE #{@sclid-1} #{@scid} "
                end #skip
#ev            

            @sba_system.services[@sclid-1][@scid][0].call(@sba_tile,self) #skip                    
    #C++        FuncPointer fp=sba_system.cfg.services[(sclid<<8)+scid].core;
    #C++        (*fp)((Base::ServiceCore*)this);
#iv                
                if @verbose #skip
        puts "DONE CALL TO CORE"
                end #skip
#ev
            @sba_tile.service_manager.core_return_type=@core_return_type
            @sba_tile.service_manager.ack_ok=@ack_ok   
            @sba_tile.service_manager.n_args=@n_args    
            @sba_tile.service_manager.opcode=@opcode     
            @sba_tile.service_manager.core_status=@core_status
            # For threads (or more precisely, for the new Tile.run() with status required for threads)
            if (@sba_tile.service_manager.subtask_code_fifo.length>0) or (@sba_tile.service_manager.subtask_reference_fifo.length>0) or (@sba_tile.service_manager.core_status!=CS_idle)
              @sba_tile.service_manager.status=true
            end
if @verbose #skip
            puts "ServiceCore: CORE STATUS : #{@sba_tile.service_manager.core_status}=#{@core_status}"
end #skip 
#                #C++ cout << "CORE "<< service << " has returned \n";
        end                
            

    end # of run()
    
    ################################################################
    ####     Minimal API for task and state manipulation        ####
    ################################################################    
=begin
    "suspend" should do the following:
    1. set the core status to CS_managed => will lead to CS_idle automatically
    2. set the subtask status to STS_blocked => will lead to STS_new automatically. Maybe "inactive" is what we need? 
    3. push the subtask back onto the pending subtasks queue => might be that we need to change core_control to set STS to processing
    if we do that, will it work? Will the STS_new task status not cause a problem?
=end    
    def suspend() #t void () 
        #tile
        puts "suspend()" #skip
        @core_status=CS_managed
        @sba_tile.service_manager.subtask_list.status(@sba_tile.service_manager.current_subtask,STS_blocked)        
        @sba_tile.service_manager.pending_subtasks_fifo.push(@sba_tile.service_manager.current_subtask)        
    end
#skip    
    def iterate(*v)
        suspend()
        if v!=nil
            return v
        end
    end
#endskip
#skipcc

=begin #C++ 
    Word iterate();
    Word iterate(Word v);
=end #C++ 
#endskipcc
#skiph
=begin #C++
    Word ServiceCore::iterate() {
        suspend();
        return 0;
    }
    Word ServiceCore::iterate(Word v) { // FIXME: can I use a template here?
        suspend();
        return v;
    }
=end #C++
#endskiph
=begin 
    keep vim happy
=end 
    
    def init() #t bool ()
        puts "init()" #skip
        if @state_register[0]==0
            @state_register[0]=1
            return true
        else
            return false
        end
    end
    def store(state) #t void (void*)        
        @state_register[1]=state #C++ state_register[1]=(MWord)state; 
    end
    def load() #t void* ()
        return @state_register[1] #C++ return (void*)state_register[1];
    end
    # We will also need methods to pack/unpack the Word_List
    def unpack(wl) #t void* (Word_List)
        return wl[0] #skip
        # in C++ we need to create a buffer with the size of the Word_List and populate it
        # This is much less easy than it seems!
        #C++ void* pt = static_cast<void*>(&wl.at(0));
        #C++ return pt;
    end
    def pack(res) #t Word_List (void*)
        wl=[] #t Word_List
        wl.push(res) #skip
        # in C++ we need to convert this void* buffer into a Word_List
        # This is much less easy than it seems!
        return wl
    end
    

################################################################
####     General API for easy kernel implementation         ####
################################################################    
    
    # Convenience function to return the argument value    
    # Now, we can return scalars or Word_List
    # I guess it is maybe better to return the Word_List as-is and have some other functions
    # to get the actual values, like getWord, getDouble, getString or simply get<T>
    # that function can be defined in Types.h
    def addresses() #t MemAddresses& ()
        #tile
        return @sba_tile.service_manager.arg_addresses
    end    
    def arg(argn) #t Word_List (uint)
        puts "ARGN: #{argn}" if @verbose #skip
        #tile
        argmode = @sba_tile.service_manager.subtask_list.argmodes(@sba_tile.service_manager.current_subtask)[argn] & 0x3 #t uint
# puts argmode
        words =[] #C++  Word_List words;
        if argmode == 0
            addr=@sba_tile.service_manager.arg_addresses[argn] #t MemAddress
#            puts @sba_tile.data_store.inspect            
            words=@sba_tile.data_store.mget(addr)            
        elsif argmode ==1 or argmode == 2            
            if argmode == 2    
                hword=EXTSYM #t Word
                hword=setKind(hword,(argmode>>5)&0x3)
                hword=setDatatype(hword,(argmode>>2)&0x3)
                words.push(hword)
            end
            words.push(@sba_tile.service_manager.arg_addresses[argn])
        else 
            raise "ARGMODE must be 0,1 or 2"
        end
    
        if @verbose #skip
        puts ">>"  #skip
        puts @sba_tile.service_manager.arg_addresses.inspect() #skip
        puts @sba_tile.service_manager.subtask_list.arguments(@sba_tile.service_manager.current_subtask).inspect() #skip
        puts words.inspect() #skip
        puts "<<" #skip
        end #skip

        return words
    end
    def addr(argn) #t MemAddress (uint)
        #tile
        return @sba_tile.service_manager.arg_addresses[argn]
    end
    def nargs() #t uint
        #tile
        return @sba_tile.service_manager.arg_addresses.length()
    end
    
    def result(wl) #t void (Word_List)
        #tile
        result_addr=@sba_tile.service_manager.subtask_list.result_address(@sba_tile.service_manager.current_subtask) #t MemAddress        
        @sba_tile.data_store.mput(result_addr,wl)         
#        puts "Stored #{wl[0]} @ #{result_addr}"
    end
    
    def put(addr,wl) #t void (MemAddress;Word_List)
        #tile
        @sba_tile.data_store.mput(addr,wl)
    end
    
    def method() #t uint         
        return @opcode+(@scid<< FS_SCId)+(@sclid<< FS_SCLId)  
    end
end # of SBA_ServiceCore class

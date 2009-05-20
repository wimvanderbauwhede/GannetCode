#   
# Gannet Service-based SoC project - SBA Service Core class
#
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
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

class SBA_ServiceCore
#H    	Base::System* sba_system_ptr;
#H    	Base::Tile* sba_tile_ptr;  
    attr_accessor :state_register,#t State_Register;
                    :lookup_table, #t LookupTable;
                    :core_status,:core_return_type,:ack_ok,:n_args #t Core_Status;Packet_Type;uint;uint;
    attr_reader :service,:nservice,:address,:current_subtask,:tid,:opcode #t Service;Service;ServiceAddress;Subtask;uint;uint;
 #skipcc    
=begin #constructor
        ServiceCore(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) :
            sba_system_ptr(sba_s_), 
            	sba_tile_ptr(sba_t_), 
            	service(s_), address(addr_),
            	current_subtask(0),
            	tid(tid_),
            	core_status(CS_idle)
			{
			/*
			 unsigned int zero=0;
			 state_register.push_back(zero);
			 state_register.push_back(zero);
			 */
			};
=end #constructor
#endskipcc
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
        @scid=sba_tile.service_manager.scid[tid_]        
        @core_status=CS_done # 3 bits
        @core_return_type=P_data # 3 bits
        @ack_ok=1 #t bool
        @opcode=0 #t 8 bits
        @n_args=0 # 3 bits       
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
                         
if MULTI_THREADED_CORE==1    
        if @sba_tile.service_manager.core_status[@tid]==CS_busy 
            @current_subtask=@sba_tile.service_manager.current_subtask[@tid]
            # The service core in the real SBA will of course have been configured in advance.
            # Here we use the service name to lookup the actual function which implements the core

            # Actual call to the CORE
#iv
            puts "#{service} ServiceCore #{@service} (tid:#{@tid}): #{@sba_tile.service_manager.arg_addresses[@tid].length()} addresses"
#ev
# In HW, some event is required to set the CS_done flag, e.g. the resuls_store is filled
# I put the status change before the call as this allows the core to set the status if it likes to
            @sba_tile.service_manager.core_status[@tid]=CS_done
            @core_status=CS_done
            service_id=@sba_tile.service_manager.service_id[@tid] #t Service
            @service=service_id
            @n_args=@sba_tile.service_manager.n_args[@tid]
            @core_status=@sba_tile.service_manager.core_status[@tid]
            @core_return_type=@sba_tile.service_manager.core_return_type[@tid]
            @ack_ok=@sba_tile.service_manager.ack_ok[@tid]
            @opcode=@sba_tile.service_manager.opcode[@tid]
            # scid is the service core id for the current thread id. So we need to look up scid from tid, i.e. getScid(tid)
            # This means working out how the threads are partitioned; or simply with a lookup @scid[tid]
            @sba_tile.service_manager.results_store[@tid]=@sba_system.services[service_id][@scid][0].call(@sba_system,@sba_tile,self,@sba_tile.service_manager.arg_addresses[@tid]) #skip                        
#           @sba_system.services[service_id][0].call(@sba_system,@sba_tile,self,@sba_tile.service_manager.arg_addresses[@tid]) #skip

#C++    		FuncPointer fp=sba_system.cfg.services[service_id].core; // FIXME: what about scid?
#C++            sba_tile.service_manager.results_store[tid]=(*fp)((Base::ServiceCore*)this,sba_tile.service_manager.arg_addresses[tid]);
                
            @sba_tile.service_manager.core_return_type[@tid]=@core_return_type
            @sba_tile.service_manager.ack_ok[@tid]=@ack_ok
            @sba_tile.service_manager.n_args[@tid]=@n_args
            @sba_tile.service_manager.opcode[@tid]=@opcode
            @sba_tile.service_manager.core_status[@tid]=@core_status
            # For threads (or more precisely, for the new Tile.run() with status required for threads)
            if (@sba_tile.service_manager.subtask_code_fifo.length>0) or (@sba_tile.service_manager.subtask_reference_fifo.length>0) or (@sba_tile.service_manager.core_status[@tid]!=CS_idle)
              @sba_tile.service_manager.status=true
            end
            
            puts "ServiceCore: CORE STATUS : #{@sba_tile.service_manager.core_status[@tid]}=#{@core_status}" #skip
            
        end                
else # MULTI_THREADED_CORE==0            
                   
        if @sba_tile.service_manager.core_status==CS_busy 
            @current_subtask=@sba_tile.service_manager.current_subtask
               
            # The service core in the real SBA will of course have been configured in advance.
            # Here we use the service name to lookup the actual function which implements the core

            # Actual call to the CORE
#            #C++ cout << "Calling CORE "<< service <<"\n";
#            puts @sba_tile.service_manager.arg_addresses.inspect #skip
#iv
            puts "#{service} ServiceCore: #{@sba_tile.service_manager.arg_addresses.length()} addresses"
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
            @sba_tile.service_manager.results_store=@sba_system.services[@service][@scid][0].call(@sba_system,@sba_tile,self,@sba_tile.service_manager.arg_addresses) #skip
#            @sba_tile.service_manager.results_store=@sba_system.services[@service][0].call(@sba_system,@sba_tile,self,@sba_tile.service_manager.arg_addresses) #skip

#C++    		FuncPointer fp=sba_system.cfg.services[service].core;
#C++        sba_tile.service_manager.results_store=(*fp)((Base::ServiceCore*)this,sba_tile.service_manager.arg_addresses);
            # @sba_tile.service_manager.core_status=@core_status
                
            @sba_tile.service_manager.core_return_type=@core_return_type
            @sba_tile.service_manager.ack_ok=@ack_ok   
            @sba_tile.service_manager.n_args=@n_args    
            @sba_tile.service_manager.opcode=@opcode     
            @sba_tile.service_manager.core_status=@core_status
            # For threads (or more precisely, for the new Tile.run() with status required for threads)
            if (@sba_tile.service_manager.subtask_code_fifo.length>0) or (@sba_tile.service_manager.subtask_reference_fifo.length>0) or (@sba_tile.service_manager.core_status!=CS_idle)
              @sba_tile.service_manager.status=true
            end
            
            puts "ServiceCore: CORE STATUS : #{@sba_tile.service_manager.core_status}=#{@core_status}" #skip
#                #C++ cout << "CORE "<< service << " has returned \n";
#                #C++ cout << sba_tile.service_manager.results_store[0] <<"\n";
#                #C++ cout << sba_tile.service_manager.results_store[1] <<"\n";
        end                
            
end #  MULTI_THREADED_CORE    

    end # of run()
  
end # of SBA_ServiceCore class

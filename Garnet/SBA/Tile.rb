# Tile.rb
#   
# :title: Service-based SoC project - SBA Tile class
#
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
# $Id: Tile.rb 2532 2009-04-22 16:15:08Z socgroup $
#++

require "SBA/Transceiver.rb"
require "SBA/ServiceManager.rb"
require "SBA/Memory.rb"
require "SBA/LookupTable.rb"
require "SBA/ServiceCore.rb"

# Let's start with a tile that is simply a wrapper around the ServiceManager
# Then move the Store outside, to the memory; use SBA_Store, not SBA_Memory
# Then move the Core outside.

#skip
class SBA_Tile #< public Base::Tile
	attr_reader :service,:address,:status
	attr_accessor :data_store,:code_store,:lookup_table,:service_manager,:service_core,:transceiver,:th
	
	def initialize(sba_system,service,address)		
		@service=service
		@address=address
		@data_store=SBA_Store.new()
		@code_store=SBA_Store.new()
		@lookup_table=SBA_LookupTable.new()
		@service_manager=SBA_ServiceManager.new(self,service) # Need self for Mem and Core		
if MULTI_THREADED_CORE==1
		@service_core=[]
		for i in 0..NCORE_THREADS
    		@service_core[i]=SBA_ServiceCore.new(sba_system,self,service,i)
        end
else # MULTI_THREADED_CORE==0        
		@service_core=SBA_ServiceCore.new(sba_system,self,service,0)
end # MULTI_THREADED_CORE
		
		@transceiver=SBA_Transceiver.new(sba_system,service)
		@status=false # idle at start
	end
	
	def run(sba_system)
#		puts "ServiceTile #{@service} STATUS: #{(@transceiver.rx_fifo.length>0) or (@transceiver.rx_fifo.length>0) or @service_manager.status}"
#		puts (@transceiver.rx_fifo.length>0),(@transceiver.tx_fifo.length>0) 
#		if ((@transceiver.rx_fifo.length>0) or (@transceiver.rx_fifo.length>0) or (@service_manager.status==true))

# THREADS: we need a blocking if @transceiver.rx_fifo.has_packets, then we set @status to true, then we go into a while loop, while (@status) do work 
# the problem is that the ServiceCore can put packets in the ServiceManager tx_queue, but the status does not reflect this
  if @transceiver.rx_fifo.has_packets()
    @status=true
    while (@status==true)
#puts "TRX #{@service}: #{@transceiver.rx_fifo.packets.length}"
		@service_manager.run()	
#		end
if MULTI_THREADED_CORE==1
    for i in 0..NCORE_THREADS-1 #t uint		
		  if (@service_manager.core_status[i]==CS_busy)
            @service_core[i].run()
		  end
		end # loop over "threads"
else # MULTI_THREADED_CORE==0
		if (@service_manager.core_status==CS_busy)
		@service_core.run()
		end
end # MULTI_THREADED_CORE 
#		if (@transceiver.tx_fifo.length>0 or sba_system.network.tx_fifo[@address].length>0)
		  @transceiver.run()
#		end
		@status = (true and (@service_manager.status or (@transceiver.tx_fifo.length>0) or (@transceiver.rx_fifo.length>0)))
#		puts "ServiceTile #{@service} STATUS-AFTER: #{@service_manager.status} or #{@transceiver.tx_fifo.length>0} or #{@transceiver.rx_fifo.length>0}:#{@transceiver.rx_fifo.packets.length}"
#		puts "ServiceTile #{@service} STATUS-AFTER: #{@status}"
    end
  end
end

if USE_THREADS==1	
    def run_th(sba_system)
        puts "Starting Tile #{@service}"
        @th=Thread.new("Instance #{@service}") do    
            while (true)
                run(sba_system)
            end                
        end        
    end	
end # USE_THREADS
end
#endskip

#skipcc
=begin #h
#ifdef WV_SYSTEMC
#include <systemc.h>
#endif // WV_SYSTEMC
#include "./ServiceConfiguration.h"
#include "./Transceiver.h"
#include "./ServiceManager.h"
#ifndef STATIC_ALLOC
#include "./Memory.h"
#endif
#include "./LookupTable.h"
#include "./ServiceCore.h"
#include "Base/Tile.h"
#include "Base/System.h" 

using namespace std;
/**
 * Ideally, each service has its own configuration. 
 * Therefore, we should store the per-service confgiration in an array. 
 * But for now, we use a common configuration file
 */
namespace SBA { 
#if USE_THREADS==1
    void *run_tile_loop(void*);
#endif
#ifdef WV_SYSTEMC 
SC_MODULE (Tile) {
#else
class Tile : public Base::Tile {
#endif	
		public:
		Base::System* sba_system_ptr;			
		Service service;
		ServiceAddress address;
		bool status;
		Transceiver transceiver;
		ServiceManager service_manager;
#ifndef STATIC_ALLOC
		Memory data_store;		
		Memory code_store;
#else
        Store<DATA_SZ> data_store;
        Store<CODE_SZ> code_store;
#endif
        LookupTable lookup_table;

#if MULTI_THREADED_CORE==1
        ServiceCore* service_core[NCORE_THREADS];
#else
		ServiceCore service_core;
#endif		
		bool finished;
#ifdef WV_SYSTEMC		
	SC_HAS_PROCESS (Tile);
	Tile(sc_module_name n_, Service& s_, ServiceAddress addr_) 	
	: sc_module(n_), service(s_), address(addr_),	
	transceiver("transceiver",s_,addr_), 
	service_manager("sba_servicemanager",service,address)	
	service_core("sba_servicecore",s_,addr_),
	finished(false)	
#else
	Tile(Base::System* sba_s_, Service& s_, ServiceAddress addr_)
	: sba_system_ptr(sba_s_), service(s_), address(addr_),
	status(false),
	transceiver(sba_s_,this,s_,addr_), 
	service_manager(sba_s_,this,s_,addr_),
#if MULTI_THREADED_CORE==0	
	service_core(sba_s_,this,s_,addr_,0),
#endif	
	finished(false)
#endif	
	 {
#if MULTI_THREADED_CORE==1
        for (uint i=0;i<NCORE_THREADS;i++) {
            service_core[i] = new ServiceCore(sba_system_ptr,this,service,address,i);
        }
#endif		 
	 };	// END of constructor	
	
	Tile()
	: sba_system_ptr(NULL), service(0), address(0), status(false),
	transceiver(sba_system_ptr,this,service,address), 
	service_manager(sba_system_ptr,this,service,address),
#if MULTI_THREADED_CORE==0	
	service_core(sba_system_ptr,this,service,address,0),
#endif	
	finished(false)
	{
#if MULTI_THREADED_CORE==1
        for (uint i=0;i<NCORE_THREADS;i++) {
            service_core[i] = new ServiceCore(sba_system_ptr,this,service,address,i);
        }
#endif	
	};
/*
 Main methods
*/
	void run();

#if USE_THREADS==1
    pthread_t tid;
    pthread_attr_t attr;
    void* tstatus;
    void run_th ();
#endif 
	
}; // end # of Tile class definition

} // namespace SBA
=end #h
#endskipcc

#skiph
=begin #cc
#include "System.h"
#include "Tile.h"
// The SBA Tile is the interface between the Service Manager and the Network.
// It transfers data from the Network tx_fifo to a local rx_fifo 
// and from the local tx_fifo to the Network rx_fifo


    //-- ----------------------------------------------------------------------------
    //
    // Main methods
    //
    
using namespace std;
using namespace SBA;
void Tile::run() {
//#ifdef VERBOSE
//	cout <<"Running tile "<<Tile::service << "("<<Tile::address<<")\n";
//	cout <<Tile::service << " ("<<Tile::address<<")\n";
//#endif // VERBOSE	
        System& sba_system=*((System*)sba_system_ptr);
	    if ((transceiver.rx_fifo.length()>0) || (transceiver.rx_fifo.length()>0) || service_manager.status) {
        service_manager.run();
        }
#if MULTI_THREADED_CORE==1
		for (uint i = 0;i<NCORE_THREADS;i++) {		
		  if (service_manager.core_status[i]==CS_busy) {
            service_core[i]->run();
		  }
		} // loop over "threads"
#else // MULTI_THREADED_CORE==0
        if (service_manager.core_status==CS_busy) {
        service_core.run();
        }
#endif // MULTI_THREADED_CORE         

        if (transceiver.tx_fifo.length()>0 || sba_system.network.tx_fifo[address].length()>0) {
	    transceiver.run();
	    }
        status= true && (service_manager.status || (transceiver.tx_fifo.length()>0) || (transceiver.rx_fifo.length()>0));
} //  of run()

#if USE_THREADS==1
    void *SBA::run_tile_loop(void* voidp) { 
        SBA::Tile* the_object = (SBA::Tile*)voidp;
        while (1) { 
        the_object->run(); 
        }
        pthread_exit((void *) 0);
    }
    void Tile::run_th () {
#ifdef VERBOSE    
    cout << "Starting Tile " << service << "\n";
#endif // VERBOSE 
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_tile_loop, (void*)this);        
    }        
#endif // USE_THREADS==1   

=end #cc


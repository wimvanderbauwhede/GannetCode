# Tile.rb
#   
# :title: Gannet Service-based SoC project - SBA Tile class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk> 
#
# $Id: GatewayTile.rb 2532 2009-04-22 16:15:08Z socgroup $


require "SBA/Transceiver.rb"
require "SBA/Gateway.rb"
require "SBA/Memory.rb"

# Let's start with a tile that is simply a wrapper around the Gateway
# Then move the Store outside, to the memory; use SBA_Store, not SBA_Memory
# We can use it for DATA, later on

#skip
class SBA_GatewayTile #< public Base::Tile
	attr_reader :service,:status,:address
	attr_accessor :gateway,:data_store,:transceiver,:finished,:result_store,:th
	
	def initialize(sba_system,service,address,task_descriptions)	
	    @finished=false
	    @service=service	    
	    @address=address
	    @status=true # always run Gateway
#if DATA==1
#		@data_store=SBA_Store.new()
#endif // DATA		
		@result_store=SBA_Store.new()
		@gateway=SBA_Gateway.new(sba_system,self,service,address,task_descriptions) #Need self for Mem and Core, system for DATA store		
		@transceiver=SBA_Transceiver.new(sba_system,service)
		@verbose=(VERBOSE==1)
	end
	
	def run(sba_system)
        if VERBOSE==1
    	puts "Running Gatewaytile #{@service}" 
        end # VERBOSE
	    # The Gateway must run until all packets are sent, then block until it receives an result        
		@gateway.run()
#        if VERBOSE==1
#		puts "Gateway listening for reply" 
#        end # VERBOSE
		@transceiver.run()
	end
if USE_THREADS==1	
    def run_th(sba_system)
        if VERBOSE==1
        puts "Starting Gateway" 
        end # VERBOSE
        @th=Thread.new("GWInstance") do    
            while (!@finished)
                run(sba_system)
            end                
        end        
    end		
end # USE_THREADS
if DISTR==1
    def run_proc(ncycles)        
        puts "Starting Gateway" if @verbose #skip
            while (!@finished and ncycles!=0)
                run(nil)
                ncycles-=1
            end
    end
end # DISTR    
end
#endskip
# =============================================================================
#skipcc
=begin #h
#ifdef WV_SYSTEMC
#include <systemc.h>
#endif

#include "./ServiceConfiguration.h"
#include "Types.h"
#include "TaskDescription.h"
#include "Gateway.h"
#ifndef STATIC_ALLOC
#include "Memory.h"
#endif
#include "Transceiver.h"
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
    void *run_gwtile_loop(void*);
#endif

#ifdef WV_SYSTEMC 
SC_MODULE (GatewayTile) {
#else
class GatewayTile : public Base::Tile {
#endif	
		public:
		Service service;
		ServiceAddress address;
		Base::System* sba_system_ptr;	
		Gateway gateway;
#if DATA==1		
		Memory data_packet_store;
#endif // DATA		
#ifndef STATIC_ALLOC
		Memory result_store;		
#else
	    Store<DATA_SZ> result_store; // not DATA_SZ: 1 packet per task
#endif	    	
		Transceiver transceiver;		
		TaskDescList task_descriptions;
		bool finished;
#ifdef WV_SYSTEMC		
	SC_HAS_PROCESS (GatewayTile);
	GatewayTile(sc_module_name n_, vector<string> tds_) 	
	: sc_module(n_), service(s_), address(addr_),	gateway("sba_gateway",tds_)	
#else
	GatewayTile(Base::System* sba_s_, Service s_, ServiceAddress addr_,TaskDescList& tds_)
	: service(s_), address(addr_), 
	sba_system_ptr(sba_s_), 
	gateway(sba_system_ptr,this,s_,addr_,tds_),
	transceiver(sba_s_,this,s_,addr_), 
	task_descriptions(tds_), finished(false)
#endif	
	 {};	// END of constructor	
	
/*
 Main methods
*/
	void run();
#if USE_THREADS==1
    pthread_t th;
    pthread_attr_t attr;
    void* tstatus;
    void run_th ();
#endif 	
}; // end # of GatewayTile class definition

} // namespace SBA
=end #h
#endskipcc
# ====================================================================================
#skiph
=begin #cc
#include "GatewayTile.h"
// The SBA GatewayTile is the interface between the Service Manager and the Network.
// It transfers data from the Network tx_fifo to a local rx_fifo 
// and from the local tx_fifo to the Network rx_fifo


    //-- ----------------------------------------------------------------------------
    //
    // Main methods
    //
    
using namespace std;
using namespace SBA;
            
void GatewayTile::run() {
#ifdef VERBOSE
//	cout << "Running Gatewaytile "<<GatewayTile::service<<"\n"; // ("<<GatewayTile::address<<")\n";
#endif // VERBOSE	
        gateway.run();
        transceiver.run();
} //  of run()

#if USE_THREADS==1
    void *SBA::run_gwtile_loop(void* voidp) { 
        SBA::GatewayTile* gwtilep = (SBA::GatewayTile*)voidp;
        while (!gwtilep->finished) { 
            gwtilep->run(); 
        }
        pthread_exit((void *) 0);
    }
    void GatewayTile::run_th () {
#ifdef VERBOSE    
    cout << "Starting Tile " << service << "\n";
#endif // VERBOSE 
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
        pthread_create(&th, &attr, SBA::run_gwtile_loop, (void*)this);        
    }        
#endif // USE_THREADS==1 

=end #cc
#endskiph

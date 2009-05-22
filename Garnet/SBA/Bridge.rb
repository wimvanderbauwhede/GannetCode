#
# Gannet Service-based SoC project - NoC Bridge class
#
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  

#
#//==============================================================================
#//
#// Gannet Service-based SoC project - NoC Bridge class 
#//
#//==============================================================================
#
#// $Id: Bridge.rb 2155 2009-01-28 11:39:41Z socgroup $

=begin #inc
#include "Base/System.h" //skipcc
#include "Base/Bridge.h" //skipcc
#include "System.h" //skiph
#include "Bridge.h" //skiph
=end #inc

# NoC Bridge corresponds to Tile, i.e. it is the Bridge Core + Transceiver
# 

require "SBA/BridgeTransceiver.rb"
require "SBA/BridgeCore.rb"

# Let's start with a tile that is simply a wrapper around the Bridge Core

#skip
class SBA_Bridge #< public Base::Tile
	attr_accessor :bridge_core,:transceiver #t BridgeCore;Transceiver
	
	def initialize(sba_system)	
		@bridge_core=SBA_BridgeCore.new(sba_system,self)		
		@transceiver=SBA_BridgeTransceiver.new(sba_system)
	end
	
	def run()
#    	puts "Running Bridge";
		@bridge_core.run()	
		@transceiver.run()
	end
	
if USE_THREADS==1	
    def run_th()
        puts "Starting Bridge"
        @th=Thread.new("Bridge") do    
            while (true)
                run()
            end                
        end        
    end	
end # USE_THREADS	
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
#include "BridgeCore.h"
#include "BridgeTransceiver.h"
#include "Base/Bridge.h"
#include "Base/System.h"

using namespace std;
/**
 * Ideally, each service has its own configuration. 
 * Therefore, we should store the per-service confgiration in an array. 
 * But for now, we use a common configuration file
 */
namespace SBA { 

#if USE_THREADS==1
    void *run_bridge_loop(void*);
#endif
#ifdef WV_SYSTEMC 
SC_MODULE (Bridge) {
#else
class Bridge : public Base::Bridge {
#endif	
		public:
		Base::System* sba_system_ptr;	
		BridgeCore bridge_core;
		BridgeTransceiver transceiver;
#ifdef WV_SYSTEMC				
	SC_HAS_PROCESS (Bridge);
	Bridge(sc_module_name n_) 	
	: sc_module(n_),	
	bridge_core("sba_bridge_core"),
	transceiver("bridge_transceiver"),
#else
	Bridge(Base::System* sba_s_)
	: sba_system_ptr(sba_s_), 
	bridge_core(sba_s_,this),
	transceiver(sba_s_,this)
#endif	
	 {};	// END of constructor	
	
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
	
}; // end # of Bridge class definition

} // namespace SBA
=end #h
#endskipcc
# ====================================================================================
#skiph
=begin #cc
#include "Bridge.h"

    //-- ----------------------------------------------------------------------------
    //
    // Main methods
    //
    
using namespace std;
using namespace SBA;
            
void Bridge::run() {
#ifdef VERBOSE
//	cout << "Running Bridge\n";
#endif // VERBOSE	
        transceiver.run();
        bridge_core.run();
} //  of run()

#if USE_THREADS==1
    void *SBA::run_bridge_loop(void* voidp) { 
        SBA::Bridge* the_object = (SBA::Bridge*)voidp;
        while (1) { 
        the_object->run(); 
        }
        pthread_exit((void *) 0);
    }
    void Bridge::run_th () {
#ifdef VERBOSE    
    cout << "Starting Bridge\n";
#endif // VERBOSE 
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_bridge_loop, (void*)this);        
    }        
#endif // USE_THREADS==1 

=end #cc
#endskiph


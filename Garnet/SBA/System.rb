#   
# Gannet Service-based SoC project - SBA System (toplevel) class
#
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
# $Id: System.rb 2532 2009-04-22 16:15:08Z socgroup $

=begin #inc
#ifdef WV_SYSTEMC
#include <systemc.h> //skipcc
#endif // WV_SYSTEMC
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/System.h" //skipcc
#include "Tile.h" //skipcc
#include "GatewayTile.h" //skipcc
#include "Bridge.h" //skipcc
#include "Network.h" //skipcc
#include "SystemConfiguration.h" //skipcc
#include "System.h" //skiph
=end #inc
if USE_THREADS==1
require 'thread' #skip
end # USE_THREADS
require "SBA/Network.rb"
require "SBA/Tile.rb"
require "SBA/GatewayTile.rb"
require "SBA/Bridge.rb"
# The SBA simulator is extremely simple, even naive. 
# We instantiate all objects, and then "run" all instances in a for loop in a while loop
# to have a kind of rudimentary DES.

class SBA_System #< public Base::System

#H    Config cfg; ///< System service configuration, a list defining which services are where.
    
    attr_accessor :gw_address,:services,:nservices,:finished, #t ServiceAddress;Services;uint;uint;
#if DATA==1
    :data_packet_store, #t map<uint,Word>;
#endif // DATA
    :task_descriptions, #t TaskDescList;
    :bytecode,:results,:io_mech, #t Bytecode;Word_List;uint;
    :network,:bridge, #t Network;Bridge;
    :gw_instance, #t GatewayTile; 
    :instances #C++ Tile* instances[NSERVICES];

#skipcc
=begin #constructor
	System(TaskDescList& tds_) : gw_address(NSERVICES),
	finished(false), task_descriptions(tds_), io_mech(1)
#ifndef SYSC    
    ,network(this)
    ,bridge(this)
	,gw_instance(this,0,NSERVICES,tds_) 
    {
		//Services
		nservices=NSERVICES;
#ifndef STATIC_ALLOC
		services=cfg.services;		
		if(	services.size()!= nservices+1) {
			cerr<<"services.size()!=NSERVICES+1\n";
			exit(1);
		}
		// Instantiate SBA Tiles
		/* With static instances array */
		foreach(Services, services) {
			Service service_id=iter->first;
            ServicePair servicep = iter->second;
			ServiceAddress service_address=servicep.address;
			if	(service_address != NSERVICES) {
		      instances[service_address]=new Tile(this,service_id,service_address);
#ifdef VERBOSE
					cout << "\nInstantiating service "<<service_id<<" ("<<service_address<<")\n";
#endif // VERBOSE		      
		      }
        }
#else // STATIC_ALLOC   
        for (Service service_id=1;service_id<MAX_NSERVICES;service_id++) {
            ServicePair servicep=cfg.services[service_id];
            ServiceAddress service_address=servicep.address;
			if	(service_address != NSERVICES and service_address != MAX_NSERVICES) {
		      instances[service_address]=new Tile(this,service_id,service_address);
#ifdef VERBOSE
					cout << "\nInstantiating service "<<service_id<<" ("<<service_address<<")\n";
#endif // VERBOSE		      
		      }                        
        }    
#endif // STATIC_ALLOC        
#else
{
#endif
	};
	// --------------------------------------------------------------
	System(Bytecode& byc_) : gw_address(NSERVICES),
	finished(false), bytecode(byc_), io_mech(1)
    #ifndef SYSC
    ,network(this),	bridge(this), 
    gw_instance(this,0,NSERVICES,task_descriptions) {
		//Services
		nservices=NSERVICES;
#ifndef STATIC_ALLOC			
		services=cfg.services;		
		if(	services.size()!= nservices+1) {
			cerr<<"services.size()!=NSERVICES+1\n";
			exit(1);
		}
		// Instantiate SBA Tiles
		/* With static instances array */
		foreach(Services, services) {
				Service service_id=iter->first;
                ServicePair servicep = iter->second;
				ServiceAddress service_address=servicep.address;
				if	(service_address != NSERVICES) {
		      instances[service_address]=new Tile(this,service_id,service_address);
#ifdef VERBOSE
					cout << "\nInstantiating service "<<service_id<<" ("<<service_address<<")\n";
#endif // VERBOSE		      
		      }
        }
#else        
        for (Service service_id=1;service_id<MAX_NSERVICES;service_id++) {
            ServicePair servicep=cfg.services[service_id];
            ServiceAddress service_address=servicep.address;
			if	(service_address != NSERVICES and service_address != MAX_NSERVICES) {
		      instances[service_address]=new Tile(this,service_id,service_address);
#ifdef VERBOSE
					cout << "\nInstantiating service "<<service_id<<" ("<<service_address<<")\n";
#endif // VERBOSE		      
		      }                        
        }    

#endif // STATIC_ALLOC        
#else // SYSC
       {
#endif
	};	
//	System() {};
//    ServiceAddress address_by_service(Service);
=end #constructor    
#endskipcc

#skip
    # object constructor
    def initialize(services,tds_)
        @services=services
        @finished = 0
        @nservices=@services.keys.length
        @task_descriptions=tds_
        # we only need 2 IO mechanims: file IO and socket IO, call them 1 and 0, i.e. no files means socket
        if @task_descriptions.length>0
            @io_mech=1
        else 
            @io_mech=0
        end
        # To clean up the mess, we need a temporary interface_object variable
if DATA==1        
        @data_packet_store={}
end # DATA        
        # Instantiate SBA Network
        @network=SBA_Network.new(self)
        @bridge=SBA_Bridge.new(self)        
        @instances=[]                
        @gw_address=@services[0]['Addr']
        @gw_instance=(SBA_GatewayTile.new(self,0,@gw_address,@task_descriptions));
        for service_id in 1..MAX_NSERVICES-1
            if services.has_key?(service_id)
                service_address=services[service_id]['Addr']                            
                if	(service_address < @gw_address and service_address != MAX_NSERVICES)
		          @instances[service_address]=SBA_Tile.new(self,service_id,service_address)
                  puts "\nInstantiating service #{service_id} (#{service_address})"		
#                  puts @instances[service_address]      
                end                       
            end
        end   
    end	
#endskip

# service_by_address was defined here
 
=begin #C++    
    Service System::service_by_address(ServiceAddress address) { //H
    	Service service_id=0;
    	#ifndef STATIC_ALLOC
    	foreach(Services,services) {
            if (services[iter->first].address == address) {
            	service_id=iter->first;
    			break;
    		}
    	}
    	#else
        for (Service sid=1;sid<MAX_NSERVICES;sid++) {
            if (cfg.services[sid].address == address) {
            	service_id=sid;
    			break;
    		}
        }    	
    	#endif
    	return service_id;
    }    
   
=end #C++    
    # ---------------------------------------------------------------------------
#ifndef SYSC
    def run()
if VM==0
        @network.run()
else # VM==1
        @bridge.run()
end # VM
        @gw_instance.run(self) #s/self//

        if (@gw_instance.finished==false) 
#iv     		            
                print "Running Tiles\n";
#ev         
#                puts @instances
i=0 #skip
            for service_instance in @instances #C++ for (uint i=0;i<NSERVICES;i++) {       
                next if service_instance == nil #skip   
#C++                 Tile& service_instance=*instances[i];            
#iv
#                    puts "Running Tile #{i}: service id:#{service_instance.service}; address: #{service_instance.address}" 
#ev                    
if VM==0
                if true and (service_instance.status or (@network.tx_fifo[service_instance.address].length>0))
#                puts "STATUS #{service_instance.service} (#{service_instance.address}): #{service_instance.status or (@network.tx_fifo[service_instance.address].length>0)}"
                    service_instance.run(self) #s/self//	
                end		
else # VM==1
                if true and (service_instance.status or service_instance.transceiver.rx_fifo.status)
#                puts "STATUS #{service_instance.service} (#{service_instance.address}): #{service_instance.status or (@network.tx_fifo[service_instance.address].length>0)}"
                    service_instance.run(self) #s/self//	
                end	
end # VM                
                i=i+1 #skip
            end         
#iv
        print "\n";            
#ev
        else
            @finished=1;
        end   
             				 
    end # of run()
    
if USE_THREADS==1

    def run_th()
if VM==0
        @network.run_th()
else # VM==1
        @bridge.run_th()
end # VM
        @gw_instance.run_th(self)  #s/self//

#iv     		            
        print "Running Tiles\n";
#ev         
            for service_instance in @instances #C++ for (uint i=0;i<NSERVICES;i++) {          
#C++                 Tile& service_instance=*instances[i];            
if VM==0
                    service_instance.run_th(self) #s/self//	
else # VM==1
                    service_instance.run_th(self) #s/self//	
end # VM                
            end         
#iv
        print "\n";            
#ev            
        @gw_instance.th.join #C++ pthread_join(gw_instance.th,&gw_instance.tstatus);               

    end # of run_th()

end # USE_THREADS
#endif //SYSC        
end # of SBA_System

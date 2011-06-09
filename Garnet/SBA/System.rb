#   
# Gannet Service-based SoC project - SBA System (toplevel) class
#
# (c) 2004-2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#

=begin #inc
#ifdef WV_SYSTEMC
#include <systemc.h> //skipcc
#endif // WV_SYSTEMC
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/System.h" //skipcc
#if SEQVM==0
#include "Tile.h" //skipcc
#else
#include "TileVM.h" //skipcc
#endif
#if DISTR==0
#include "GatewayTile.h" //skipcc
//#include "Bridge.h" //skipcc
//#include "Network.h" //skipcc
#else
#define NSERVICES 1
#endif
#include "SystemConfiguration.h" //skipcc
#include "System.h" //skiph
=end #inc

=begin
# WV20110609: for the old configuration behaviour revert to before r4987 and set NEW=0 and NEWER=0
=end

if USE_THREADS==1
require 'thread' #skip
end # USE_THREADS
if DISTR==0
require "SBA/Network.rb"
end # DISTR
if SEQVM==1
require "SBA/TileVM.rb"
else # SEQVM==0
require "SBA/Tile.rb"
end # SEQVM 
if DISTR==0
require "SBA/GatewayTile.rb"
require "SBA/Bridge.rb"
end # DISTR

class SBA_System #< public Base::System

#H    Config cfg; ///< System service configuration, a list defining which services are where.

    attr_accessor :gw_address,:servicenodes,:configurations,:nservicenodes,:finished, #t ServiceAddress;Services;Configurations;uint;uint; # FIXME: type for ServiceNodes ??
    :task_data, #t string;
    :task_descriptions, #t TaskDescList;    
    :bytecode,:results,:io_mech,:multi_ip, #t Bytecode;Word_List;uint;uint
    :nodes #skip
if DISTR==0    
    attr_accessor :network,:bridge, #skip #t Network;Bridge;       
    :gw_instance #t GatewayTile; 
end # DISTR    
#skipcc
=begin #constructor
    Tile* nodes[NSERVICES];
	System(TaskDescList& tds_) : gw_address(0),
	finished(false), task_descriptions(tds_), io_mech(1)
#ifndef SYSC    
	,gw_instance(this,0,0,tds_)
    {
		//Services
		nservicenodes=NSERVICES;
#ifndef STATIC_ALLOC

    for (Service node_id=1;node_id<=NSERVICES;node_id++) {
        ServiceAddress service_address=node_id;
        if  (service_address != 0) {
          nodes[service_address]=new Tile(this,node_id,service_address);
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE             
          }
    }    
   
#else // STATIC_ALLOC

    for (Service node_id=1;node_id<=NSERVICES;node_id++) {
        ServiceAddress service_address=node_id;
        if  (service_address != 0 and service_address != MAX_NSERVICES) {
          nodes[service_address]=new Tile(this,node_id,service_address);
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE             
          }                        
    }      
         
#endif // STATIC_ALLOC        
#else // SYSC
{
#endif // SYSC
	};
#if DISTR==0
	// --------------------------------------------------------------
	System(Bytecode& byc_) : gw_address(0),
	finished(false), bytecode(byc_), io_mech(1)
    #ifndef SYSC
    ,gw_instance(this,0,0,task_descriptions)
    {
		//Services
		nservicenodes=NSERVICES;
#ifndef STATIC_ALLOC			

    for (Service node_id=1;node_id<=NSERVICES;node_id++) {
               ServiceAddress service_address=node_id;
               if  (service_address != 0 and service_address != MAX_NSERVICES) {
                 nodes[service_address]=new Tile(this,node_id,service_address);
   #ifdef VERBOSE
                       cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
   #endif // VERBOSE             
                 }                        
           }
        
#else // STATIC_ALLOC   

    for (Service node_id=1;node_id<=NSERVICES;node_id++) {
        ServiceAddress service_address=node_id;
        if  (service_address != 0 and service_address != MAX_NSERVICES) {
          nodes[service_address]=new Tile(this,node_id,service_address);
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE             
          }                        
    }
   
#endif // STATIC_ALLOC        
#else // SYSC
       {
#endif
	};	
#else // DISTR
    // --------------------------------------------------------------
    System(int node_id,int multi_ip_) : gw_address(node_id),multi_ip(multi_ip_),
    finished(false), io_mech(1)
    {
        nodes[0]=new Tile(this,node_id,multi_ip); //WV: we use the service_address as multi_ip! Dangerous!     
    };  
#endif // DISTR
=end #constructor    
#endskipcc

=begin
=end

#skip
    # object constructor
    def initialize(servicenodes,configurations,tds_)
        @servicenodes=servicenodes
        @configurations=configurations
        @finished = 0
        @nservicenodes=NSERVICES # @servicenodes.keys.length
        @task_descriptions=tds_        
        @task_data=""
        # we only need 2 IO mechanims: file IO and socket IO, call them 1 and 0, i.e. no files means socket        
        if @task_descriptions.length>0
            @io_mech=1
if DISTR==1
            @multi_ip=@task_descriptions.shift
end # DISTR            
        else 
            @io_mech=0
        end
#        raise "TASKS: #{@task_descriptions.inspect}"
        # To clean up the mess, we need a temporary interface_object variable
#if DATA==1        
#        @data_packet_store={}
#endif // DATA        
if DISTR==0        
        # Instantiate SBA Network
        @network=SBA_Network.new(self)
        @bridge=SBA_Bridge.new(self)
        @nodes=[]                
        @gw_address=0

        @gw_instance=SBA_GatewayTile.new(self,0,@gw_address,@task_descriptions)
        for node_id in 1..@nservicenodes
            service_address=node_id
		    @nodes[service_address]=SBA_Tile.new(self,node_id,service_address)
            puts "\nInstantiating service #{node_id} (#{service_address})"
        end		
else # DISTR
        node={}
        servicenodes.each_value {|value| node=value }
        node_id=node['Addr']
        service_address=node_id
        @nodes=[]
        @nodes[0]=SBA_Tile.new(self,node_id,service_address)                    
end # DISTR                
        @verbose=false
    end	
#endskip

# service_by_address was defined here
 
=begin #C++        
#if DISTR==0 
    Service System::service_by_address(ServiceAddress address) { //H
        Service node_id=address;
        return node_id;
    }    
=end #C++

#endif // DISTR       
=begin
=end
    # ---------------------------------------------------------------------------
#ifndef SYSC
    def run()
if DISTR==0                 
    
if VM==0
        @network.run() #skip
else # VM==1
        @bridge.run() #skip
end # VM
 
        @gw_instance.run(self) #s/self//
  
        if (@gw_instance.finished==false) 
#iv     		            
                print "Running Tiles\n";
#ev         
#                puts @nodes
            i=0 #skip
            for service_node in @nodes #C++ for (uint i=1;i<=NSERVICES;i++) {       
                next if service_node == nil #skip   
#C++                 Tile& service_node=*nodes[i];            
#iv
                    puts "Running Tile #{i}: service id:#{service_node.service}; address: #{service_node.address}" 
#ev                    
if VM==0
                if true and (service_node.status or (@network.tx_fifo[service_node.address].length>0))
                puts "STATUS #{service_node.service} (#{service_node.address}): #{service_node.status or (@network.tx_fifo[service_node.address].length>0)}"
                    service_node.run(self) #s/self//	
                end		
else # VM==1
                puts "STATUS #{service_node.service} (#{service_node.address}): #{service_node.status} or #{service_node.transceiver.rx_fifo.status()}" #C++ cout << "STATUS " <<service_node.service<< " (" <<service_node.address<< "): " << service_node.status <<" or "<< service_node.transceiver.rx_fifo.status() << endl;
                if true and (service_node.status or service_node.transceiver.rx_fifo.status())
					puts service_node.address
                puts "STATUS #{service_node.service} (#{service_node.address}): #{service_node.status or (@network.tx_fifo[service_node.address].length>0)}" #C++ cout << "STATUS " <<service_node.service<< " (" <<service_node.address<< "): " << endl;
                    service_node.run(self) #s/self//	
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
end # DISTR             				 
    end # of run()
    
if USE_THREADS==1

    def run_th()
if VM==0
        @network.run_th() #skip
else # VM==1
        @bridge.run_th() #skip
end # VM
        @gw_instance.run_th(self)  #s/self//

#iv     		            
        print "Running Tiles with Threads\n";
#ev
            for service_node in @nodes #C++ for (uint i=1;i<=NSERVICES;i++) {
                next if service_node == nil #skip           
#C++                 Tile& service_node=*nodes[i];            
if VM==0
                    service_node.run_th(self) #s/self//	
else # VM==1
                    service_node.run_th(self) #s/self//	
end # VM                
            end         
#iv
        print "\n";            
#ev            
        @gw_instance.th.join #C++ pthread_join(gw_instance.th,&gw_instance.tstatus);               

    end # of run_th()

end # USE_THREADS
if DISTR==1
    def run_proc()
        #iv                         
                puts "Running Tile as Process" if @verbose #skip
        #ev
        service_node = @nodes[0] #C++ Tile& service_node=*nodes[0];                    
        service_node.run_proc(self) #s/self// 
        #iv
                puts if @verbose #skip            
        #ev                    
    end
end # DISTR    


#endif //SYSC        
end # of SBA_System

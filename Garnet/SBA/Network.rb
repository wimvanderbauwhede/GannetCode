#   
# Gannet Service-based SoC project - Network class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>#
#
# $Id: Network.rb 2155 2009-01-28 11:39:41Z socgroup $

=begin #inc
#include "Types.h" //skipcc
#include "Base/System.h" //skipcc
#include "Base/Network.h" //skipcc
#include "Packet.h" //skipcc
#include "SystemConfiguration.h" //skipcc
#include "System.h" //skiph
#include "Network.h" //skiph
=end #inc

#skipcc
=begin #C++
#if USE_THREADS==1
namespace SBA {
    void *run_network_loop(void*);
}    
#endif
=end #C++
#endskipcc

# A star topology NoC model, the only aim is to have full connectivity
class SBA_Network #< public Base::Network
    include SBA
    
    attr_accessor :nservices,:tx_fifo,:rx_fifo, #skip
    :bridge_rx_fifo,:bridge_tx_fifo #skip
#skipcc
=begin #constructor
	Base::System* sba_system_ptr;
    uint nservices;
    RX_Packet_Fifo rx_fifo[NSERVICES+1];
    TX_Packet_Fifo tx_fifo[NSERVICES+1];

    RX_Packet_Fifo bridge_rx_fifo;
    TX_Packet_Fifo bridge_tx_fifo;
        
    Network(Base::System* sba_s_) : sba_system_ptr(sba_s_),
	    nservices(NSERVICES) {
#if VM==1	    
	    for (unsigned int i=0;i<=NSERVICES;i++) {
	    	rx_fifo[i].clear();
	    rx_fifo[i].status=0;	
	    tx_fifo[i].clear();
	    tx_fifo[i].status=0;
	    }
#endif	    
	    };
=end #constructor
#endskipcc

#skip    
    def initialize(sba_system)
    @sba_system=sba_system
        @nservices=NSERVICES #n_services
        @rx_fifo=[] 
        @tx_fifo=[] 
        for i in 0..NSERVICES #n_services-1 
            @rx_fifo[i]=SBA_RX_Packet_Fifo.new(self)
            @tx_fifo[i]=SBA_TX_Packet_Fifo.new()
        end        
        @bridge_rx_fifo=SBA_RX_Packet_Fifo.new(self)
        @bridge_tx_fifo=SBA_TX_Packet_Fifo.new()
    end # of initialize
#endskip
    
    def run()
        switch_packets()
    end
    
    def show_status      
        for i in 0..NSERVICES #C++ for (uint i=0;i<=NSERVICES;i++) {
            print "RX",i,":\t",@rx_fifo[i].status,"\n"; # FLAG
            print "TX",i,":\t",@tx_fifo[i].status,"\n"; # FLAG
        end
    end
    # switch_packets() transfers packets from the rx_fifo for a given inport 
    # (connected to a service tile) to the rx_fifo for 
    # the corresponding outport.
    def switch_packets() 
    #system
    #iv
        puts "switch_packets()"
    #ev        
        for i in 0..NSERVICES  #C++ for (uint i=0;i<=NSERVICES;i++) {
            if @rx_fifo[i].status==1 # there are packets
            #iv
                print "\nNetwork Port ",i,":","Sending: \n"
                #ev
                while @rx_fifo[i].status==1
                    packet=@rx_fifo[i].shift #t Packet_t
                #iv
                    print "Network:HEADER:",getTo(getHeader(packet)),"\n" #skip
                    #C++ cout << "Network:HEADER:" << (int)getTo(getHeader(packet)) << "\n";    
                #ev
                    service_id=getTo(getHeader(packet)) #t Service
                    dest=@sba_system.services[service_id]['Addr'] #C++ ServiceAddress dest=sba_system.cfg.services[service_id].address;
            # Putting in the Bridge requires some extra logic
               if service_id < SBA_BRIDGE_ADDR # it must be the service id, not the address! So I should call it SBA_BRIDGE_ID
                    @tx_fifo[dest].push(packet)
               else
                   warn "Found HW service" #skip
                    @bridge_tx_fifo.push(packet)
               end                    
                end
                #iv
                print "\n"
                #ev
            end
        end
        if @bridge_rx_fifo.status()==1
     #iv
                print "\nNetwork Bridge Sending: \n"
                #ev
                while @bridge_rx_fifo.status()==1
                    packet=@bridge_rx_fifo.shift #t Packet_t
                #iv
                    print "Network:HEADER:",getTo(getHeader(packet)),"\n"
                    #ev
                    service_id=getTo(getHeader(packet)) #t Service
                    dest=@sba_system.services[service_id]['Addr'] #C++ ServiceAddress dest=sba_system.cfg.services[service_id].address;
               if service_id < SBA_BRIDGE_ADDR
                    @tx_fifo[dest].push(packet)
               else
               warn "Found HW service"  #skip
                    @bridge_tx_fifo.push(packet)
               end                    
                end
                #iv
                print "\n"
                #ev        
        end
    end # of switch_packets

if USE_THREADS==1
#skip	
    def run_th()    
        puts "Starting Network"      
        @th=Thread.new("Network") do    
            while (true)
                run()
            end                
        end        
    end	
#endskip

#skipcc
=begin #h
#if USE_THREADS==1
    pthread_t tid;
    pthread_attr_t attr;
    void* tstatus;
    void run_th ();
#endif 
=end #h
#endskipcc

#skiph
=begin #cc
    void *SBA::run_network_loop(void* voidp) { 
        SBA::Network* the_object = (SBA::Network*)voidp;
        while (1) { 
        the_object->run(); 
        }
        pthread_exit((void *) 0);
    }
    void Network::run_th () {
#ifdef VERBOSE    
    cout << "Starting Network\n";
#endif  
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_network_loop,(void*)this);        
    }        
=end #cc
#endskiph

end # USE_THREADS    
end # of SBA_Network

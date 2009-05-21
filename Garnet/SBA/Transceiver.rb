# Transceiver.rb
#   
# :title: Gannet Service-based SoC project - Transmitter/Receiver class
#
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#//==============================================================================
#//
#// Gannet Service-based SoC project - Tansmitter/Receiver class 
#//
#//==============================================================================
#
#// $Id: Transceiver.rb 2513 2009-04-15 13:47:03Z socgroup $
#++

=begin #inc
#include "Base/Network.h" //skipcc
#include "Base/Tile.h" //skipcc
#include "Base/System.h" //skipcc
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Network.h" //skiph
#include "System.h" //skiph
#include "Transceiver.h" //skiph
=end #inc

# The SBA Transceiver is the interface between the Service Manager and the Network.
# It transfers data from the Network tx_fifo to a local rx_fifo 
# and from the local tx_fifo to the Network rx_fifo
# FIXME: I'm not entirely satisfied with this implementation of Transceiver and Network. But at least it's decoupled from the Service manager. 
require "SBA/System.rb"
class SBA_Transceiver
    include SBA

#H    	Base::System* sba_system_ptr;
#H      Base::Tile* sba_tile_ptr;    
    attr_reader :service,:address #t Service;ServiceAddress;
    attr_accessor 	:rx_fifo,:tx_fifo #t RX_Packet_Fifo;TX_Packet_Fifo
#skipcc    
=begin #constructor
    #ifdef VERBOSE
    int nnservice;
    #endif
    Transceiver(Base::System* sba_s_,Base::Tile* sba_t_,Service& s_, ServiceAddress addr_) :    
    sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_),
    service(s_), address(addr_) 
    {
    #ifdef VERBOSE
       nnservice=service;
    #endif
    };
=end #constructor
#endskipcc    
#skip    
    def initialize(sba_system,service)
        @sba_system=sba_system
        @service=service        
        @nnservice=num2name(service)
        @v=(VERBOSE==1)
        @rx_fifo=SBA_RX_Packet_Fifo.new(self)
        @tx_fifo=SBA_TX_Packet_Fifo.new() 
        @address=address_by_service(service)

    end		
#endskip
    #-- ----------------------------------------------------------------------------
    #
    # Main methods
    #++
    
    # At every timestep, the run() function checks for events:
    # - is there a packet for me?
    # - has my service processing finished? 
    
    def run()
#        puts "Running Transceiver at #{@service} @#{@address}"
        # - Check if there are new packets and receive them (i.e. put in RX FIFO)
if VM==0
        receive_packets()
end # VM        
        # If there are any packets in the TX FIFO, dispatch them
        transmit_packets()	
    end #  of run()
    
    
    #==============================================================================
    
    # This is the main receiver module. 
    # It simply transfers all packets from the local node switch's tx_fifo
    # to the local rx_fifo
if VM==0
    def receive_packets() 
    #system    
#iv
#        puts "#{@service} TRX:receive_packets()" 
#        puts @sba_system.network.tx_fifo[@address].status
#            print "\n",@service," (addr:#{@address}) TRX RX:\n" 
#            print @service,":","Receiving ",@sba_system.network.tx_fifo[@address].length," packet(s) from network ...\n" 
#ev        
        while @sba_system.network.tx_fifo[@address].status==1                
            # receive a packet
            tmp_packet=@sba_system.network.tx_fifo[@address].shift #t Packet_t 
            @rx_fifo.push(tmp_packet);  
        end # while there are packets to be received.         
    end # of receive_packets
end # VM    
    # -----------------------------------------------------------------------------
    
    def transmit_packets() 
    #system
#iv
#        puts "#{@service} TRX:transmit_packets()" 
#        puts @tx_fifo.status()
#ev            
        if @tx_fifo.status==1 # packets in FIFO 
        #iv
            print "\n",@service," (addr:#{@address}) TRX TX:","Sending: \n"
            #ev
            while @tx_fifo.status==1
                packet=@tx_fifo.shift #t Packet_t
                service_id= getTo(getHeader(packet)) #t Service                
                dest=@sba_system.services[service_id]['Addr'] #C++ ServiceAddress dest=sba_system.cfg.services[service_id].address;

#iv
                puts "HEADER:"
                puts getTo(getHeader(packet)) #s/get/(int)get/
                print "PACKET:\n",ppPacket(packet),"\n"
                puts "service id: #{service_id}; dest addr: #{dest}"
                print "#{ getType(getHeader(packet))} #{ getReturn_as(getHeader(packet))} to #{service_by_address(dest)} (addr:#{dest})\n" #skip
#C++                 cout << "" <<(int)getType(getHeader(packet))<< " " <<getReturn_as(getHeader(packet))<< " to " <<sba_system.service_by_address(dest)<< " (addr:" <<dest<< ")\n";

#ev
if VM==0    
                @sba_system.network.rx_fifo[@address].push(packet)
else # VM==1
                if dest==@sba_system.gw_address # GW
                    @sba_system.gw_instance.transceiver.rx_fifo.push(packet)              
                else 
                    if service_id < SBA_BRIDGE_ADDR
                        @sba_system.instances[dest].transceiver.rx_fifo.push(packet) #C++ sba_system.instances[dest]->transceiver.rx_fifo.push(packet);
                    else
                        warn "Found HW service" #skip
                        @sba_system.bridge.transceiver.rx_fifo.push(packet)
                    end                    
                end
end # VM                
            end
        end
    end # of transmit_packets
end # of SBA_Transceiver class

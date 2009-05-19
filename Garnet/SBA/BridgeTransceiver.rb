# BridgeTransceiver.rb
#   
# :title: Service-based SoC project - Bridge Transmitter/Receiver class
#
#--
#
#/* ***** BEGIN LICENSE BLOCK *****
# * Version: AFL 2.1
# *
# * The contents of this file are subject to the Academic Free License Version
# * 2.1 (the "License") you may not use this file except in compliance with
# * the License. You may obtain a copy of the License at
# * http://opensource.org/licenses/afl-2.1.php
# *
# * Software distributed under the License is distributed on an "AS IS" basis,
# * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# * for the specific language governing rights and limitations under the
# * License.
# *
# *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
# *
# * ***** END LICENSE BLOCK ***** */
#
#//==============================================================================
#//
#// Service-based SoC project - Tansmitter/Receiver class 
#//
#//==============================================================================
#
#// $Id: BridgeTransceiver.rb 2155 2009-01-28 11:39:41Z socgroup $
#++

=begin #inc
#include "Base/Network.h" //skipcc
#include "Base/Bridge.h" //skipcc
#include "Base/System.h" //skipcc
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Network.h" //skiph
#include "System.h" //skiph
#include "BridgeTransceiver.h" //skiph
=end #inc

# The SBA BridgeTransceiver is the interface between the Service Manager and the Network.
# It transfers data from the Network tx_fifo to a local rx_fifo 
# and from the local tx_fifo to the Network rx_fifo
# FIXME: I'm not entirely satisfied with this implementation of BridgeTransceiver and Network. But at least it's decoupled from the Service manager. 
require "SBA/System.rb"
class SBA_BridgeTransceiver
    include SBA

#H    	Base::System* sba_system_ptr;
#H      Base::Bridge* sba_tile_ptr;    

    attr_accessor 	:rx_fifo,:tx_fifo #t RX_Packet_Fifo;TX_Packet_Fifo
#skipcc    
=begin #constructor
    #ifdef VERBOSE
    int nnservice;
    #endif
    BridgeTransceiver(Base::System* sba_s_,Base::Bridge* sba_t_) :    
    sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_)
    {

    };
=end #constructor
#endskipcc    
#skip    
    def initialize(sba_system)
        @sba_system=sba_system
        @rx_fifo=SBA_RX_Packet_Fifo.new(self)
        @tx_fifo=SBA_TX_Packet_Fifo.new() 

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
#        puts "Running BridgeTransceiver at #{address}"
        # - Check if there are new packets and receive them (i.e. put in RX FIFO)
        receive_packets()
        # If there are any packets in the TX FIFO, dispatch them
        transmit_packets()	
    end #  of run()
    
    
    #==============================================================================
    
    # This is the main receiver module. 
    # It simply transfers all packets from the local node switch's tx_fifo
    # to the local rx_fifo
    def receive_packets() 
    #system
#iv
        if @sba_system.network.bridge_tx_fifo.status==1        
            print "\nBridge TRX RX:\n";
            print "Receiving ",@sba_system.network.bridge_tx_fifo.length," packet(s) from network ...\n" 
        end
#ev        
        while @sba_system.network.bridge_tx_fifo.status==1                
            # receive a packet
            tmp_packet=@sba_system.network.bridge_tx_fifo.shift #t Packet_t 
            @rx_fifo.push(tmp_packet);  
        end # while there are packets to be received.         

           
    end # of receive_packets
    # -----------------------------------------------------------------------------
    
    def transmit_packets() 
    #system

            
        if @tx_fifo.status==1 # packets in FIFO 
        #iv
            print "\nBridge TRX TX:","Sending: \n"
            #ev
            while @tx_fifo.status==1
                packet=@tx_fifo.shift #t Packet_t
            #iv
                puts "HEADER:", getTo(getHeader(packet))
                print "PACKET:\n",ppPacket(packet),"\n"
                service_id= getTo(getHeader(packet)) #t Service
                dest=@sba_system.services[service_id]['Addr'] #C++ ServiceAddress dest=sba_system.services[service_id].address;
                print "#{ getType(getHeader(packet))} #{ getReturn_as(getHeader(packet))} to #{service_by_address(dest)} (#{dest})\n" #skip
#C++                 cout << "" <<(int)getType(getHeader(packet))<< " " <<getReturn_as(getHeader(packet))<< " to " <<sba_system.service_by_address(dest)<< " (" <<dest<< ")\n";

                #ev
                @sba_system.network.bridge_rx_fifo.push(packet)
            end
        end
    end # of transmit_packets
end # of SBA_BridgeTransceiver class

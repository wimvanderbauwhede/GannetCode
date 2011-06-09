#
# Service-based SoC project - NoC Bridge Core class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: BridgeCore.rb 2155 2009-01-28 11:39:41Z socgroup $

=begin #inc
#include "Base/System.h" //skipcc
#include "Base/Bridge.h" //skipcc
#include "Packet.h" //skipcc
#include "System.h" //skiph
#include "Bridge.h" //skiph
=end #inc

# BridgeCore is the equivalent of ServiceCore in Tile. It contains the actual HW interface
# For HPC-Gannet, the BridgeCore contains the MPI send/receive calls
#    The rules are as follows:
#    Addresses 0-7: all go to node 0. For global services like LET, APPLY etc and the GW
#    Addresses 8-15: these are switched locally, so every node will implement these services, e.g. IF, ALU, IO
#    Addresses 16-higher: these go to the corresponding node
#    
#    Global control services are:
#    GATEWAY (0)
#    LET (6)
#    APPLY (7)
#    
#    Local control services are
#    
#    IF (1)
#    ALU (2)
#    CONFIG (3)
#    IO (4)
#    BEGIN (5)
#        
#    A slight complication is that the GW does not have address 0 but should run on the head node, i.e. node 0
#    
#    Currently, the NoC switches all addresses with service id > SBA_BRIDGE_ADDR to the bridge. 
#    The modification we need is: GW (id 0) should go to the bridge; LET and APPLY as well. The latter are easy: give them id > SBA_BRIDGE_ADDR
#    The actual core service might very well have an id > SBA_BRIDGE_ADDR, but it should of course be delivered locally.
#        SBA_BRIDGE_ADDR=5
#    if (service_id > SBA_BRIDGE_ADDR and node_id != (service_id - 7)) or (service_id==0 and node_id!=0)
#         switch to bridge
#    end 
#    
#    Then, the Bridge itself does this:
#    
#    if service_id<8 
#        send to node 0
#    else
#        send to node (service_id - 7) 
#    end


class SBA_BridgeCore  
    include SBA

#H    	Base::System* sba_system_ptr;
#H    	Base::Bridge* sba_tile_ptr;  
    attr_reader :bridge_address #t ServiceAddress;
    attr_accessor :hw_reg #t Word_List;
#    Packet_Fifo;Packet_Fifo;

#skipcc
=begin #constructor
BridgeCore(Base::System* sba_s_, Base::Bridge* sba_t_)
			: sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_), bridge_address(SBA_BRIDGE_ADDR) {};
=end #constructor  

#endskipcc
#skip    
    def initialize(sba_system,sba_tile)
#        @hw_reg=SBA_Packet_Fifo.new()
#C++ #ifndef HIWF        
        @hw_reg=[]        
#C++ #endif // HWIF        
        @sba_tile=sba_tile
        @sba_system=sba_system        
        @bridge_address=SBA_BRIDGE_ADDR
    end		
#endskip    
    # -----------------------------------------------------------------------------
    #
    # Main methods
    # 
    # At every timestep, the run() function checks for events:
    # - is there a packet for me?
    # - has my service processing finished? 
    def run()

=begin #C++ HWIF    
#ifdef HWIF
// Read the state register
   Xuint32 fifo_status;

   fifo_status = GANNET_VM_OPB_IF_WV_mReadSlaveReg0(baseaddr);
    
   Xuint32 b_WFIFO_VM_can_write=fifo_status & 1;
   Xuint32 b_RFIFO_VM_can_read=(fifo_status & 4)>>2;
   
   Xuint32 nfifo_status;
#endif // HWIF
=end #C++ HWIF

        # - Check if there are new packets and receive them
        receive_packets()
        # If there are any packets in the TX FIFO, transmit them
        transmit_packets()	
    end #  of run()
    # =============================================================================
    # This is the main receiver module for the service manager. 
    # As such, it should probably be an object and not a method for the SBA_Bridge object.
    # At the moment, I use a FIFO per packet type. In practice, all these FIFOs would be emulated in RAM.
    # Still, having a FIFO per type would facilitate processing, so I probably keep the concept
    def receive_packets() 
#C++        	Bridge& sba_tile=*((Bridge*)sba_tile_ptr);

#iv
if @sba_tile.transceiver.rx_fifo.status()==1
puts "NoC Bridge: receive_packets(): #{@sba_tile.transceiver.rx_fifo.status()};"
puts @sba_tile.transceiver.rx_fifo.length #C++ cout << "NoC Bridge: "<<sba_tile.transceiver.rx_fifo.size() << "\n";
end
#ev
=begin #C++ HWIF
#ifdef HWIF
    if (b_WFIFO_VM_can_write==1) {
        nfifo_status = (fifo_status & 0x1101) ;
        GANNET_VM_OPB_IF_WV_mWriteSlaveReg0(baseaddr,  nfifo_status);
        GANNET_VM_OPB_IF_WV_mResetWriteFIFO(baseaddr); 
#endif // HWIF 
=end #C++ HWIF
        while @sba_tile.transceiver.rx_fifo.status()==1 and get_txfifo_status()==1   
            rx_packet= @sba_tile.transceiver.rx_fifo.shift #t Packet_t 
            puts "PACKET:",rx_packet.inspect #skip
            put_packet(rx_packet)
        end # while there are packets to be received.
=begin #C++ HWIF   
#ifdef HWIF
       nfifo_status = (fifo_status & 0x1101) | 0x0010;
       GANNET_VM_OPB_IF_WV_mWriteSlaveReg0(baseaddr,  nfifo_status);        
    } 
#endif // HWIF 
=end #C++ HWIF     
    end # of receive_packets
    # -----------------------------------------------------------------------------
    def transmit_packets() 
#C++        	Bridge& sba_tile=*((Bridge*)sba_tile_ptr);

#iv
    first=1 #t int
#ev        
=begin #C++ HWIF   
#ifdef HWIF
    if (b_RFIFO_VM_can_read==1) {
        nfifo_status = (fifo_status & 0x0111);
        GANNET_VM_OPB_IF_WV_mWriteSlaveReg0(baseaddr,  nfifo_status);   
#endif // HWIF
=end #C++ HWIF     
        while get_rxfifo_status()==1        
#iv        
        if first==1
                puts "NoC Bridge: transmit_packets()"
                first=0
        end
#ev        
            tx_packet = get_packet() #t Packet_t
            @sba_tile.transceiver.tx_fifo.push(tx_packet)  
        end
=begin #C++ HWIF   
#ifdef HWIF        
        GANNET_VM_OPB_IF_WV_mResetReadFIFO(baseaddr);
        nfifo_status = (fifo_status & 0x0111) | 0x1000;
        GANNET_VM_OPB_IF_WV_mWriteSlaveReg0(baseaddr,  nfifo_status); 
    } 
#endif // HWIF 
=end #C++ HWIF         
    end # of transmit_packets    
    # ------------------------------------------------------------------------------------
    # This routine writes the packet word by word to the HW register
    
    def put_packet(packet) #t (Packet_t)
        header=getHeader(packet) #t Header_t
        # we veronderstellen dat HW service id = VM service id + SBA_BRIDGE_ADDR
        # so to get loopback, we simply subtract the bridge address
        # e.g. HW id = 39; bridge_address = 32 =>  SW is = 7
        mod_header=setTo(header,getTo(header) - @bridge_address) #t Header_t
        mod_packet=setHeader(packet,mod_header) #t Packet_t
        hw1=mod_packet.shift #C++ Word hw1=mod_packet.front();mod_packet.pop_front();
        mod_packet_length=(hw1 & F_Length) >> FS_Length #t Length_t
        @hw_reg.push(hw1)
        for i in 1..mod_packet_length+2 #t Length_t
            w=mod_packet.shift #C++ Word w=mod_packet.front();mod_packet.pop_front();
            @hw_reg.push(w) #skip
=begin #C++ HWIF   
#ifdef HWIF              
            GANNET_VM_OPB_IF_WV_mWriteToFIFO(baseaddr, w);             
#else         
            hw_reg.push_back(w);   
#endif // HWIF          
=end #C++ HWIF 
        end
    end
    # -----------------------------------------------------------------------------
    # This routine reads the packet word by word from the HW register    
    def get_packet() #t Packet_t
        packet=[] #t Packet_t
        hw1=@hw_reg.shift #C++ Word hw1=hw_reg.front();hw_reg.pop_front();
        packet.push(hw1)
        packet_length=(hw1 & F_Length) >> FS_Length #t Length_t
        for i in 1..packet_length+2 #t Length_t
            w=@hw_reg.shift #skip
=begin #C++ HWIF   
#ifdef HWIF         
             Xuint32 w= GANNET_VM_OPB_IF_WV_mReadFIFOOccupancy(baseaddr);
#else
             Word w=hw_reg.front();
             hw_reg.pop_front();
#endif 
=end #C++ HWIF 
            packet.push(w)            
        end
        return packet
    end    
    # -----------------------------------------------------------------------------
    # 1: not full, 0: full
    def get_txfifo_status() #t int
#C++ #ifdef HWIF
#C++      Xuint32 nvac=GANNET_VM_OPB_IF_WV_mWriteFIFOVacancy(baseaddr);
#C++ #else
nvac=SBA_BRIDGE_HW_FIFO_SZ-@hw_reg.length #t Word
#C++ #endif // HWIF
#
# This is slightly inefficient because in the worst case we leave 12 words/4 packets space unused
# But otherwise I have to backtrack if the next packet is too big
        if nvac>=MAX_PACKET_SZ
            return 1;
        else 
            return 0;
        end         
    end
    # 1: not empty, 0: empty
    def get_rxfifo_status() #t int
#C++ #ifdef HWIF
#C++     Xuint32 nocc= GANNET_VM_OPB_IF_WV_mReadFIFOOccupancy(baseaddr);
#C++ #else
nocc=@hw_reg.length #t Word
#C++ #endif // HWIF
        if nocc>0
            return 1;
        else 
            return 0;
        end         
    end        
end # of SBA_BridgeCore class

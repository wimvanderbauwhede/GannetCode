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
#include "Base/Tile.h" //skipcc
#include "Base/System.h" //skipcc
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "System.h" //skiph
#include "Transceiver.h" //skiph
=end #incl

=begin
# WV20110609: for the old configuration behaviour revert to before r4987 and set NEW=0 and NEWER=0
FIXME: I'm not entirely satisfied with this implementation of Transceiver and Network. But at least it's decoupled from the Service manager.
=end

require "SBA/System.rb"
if DISTR==1
    require 'socket'
end # DISTR
# The SBA Transceiver is the interface between the Service Manager and the Network.
# It transfers data from the Network tx_fifo to a local rx_fifo 
# and from the local tx_fifo to the Network rx_fifo
class SBA_Transceiver
    include SBA

#H    	Base::System* sba_system_ptr;
#H      Base::Tile* sba_tile_ptr;    
    attr_reader :service,:address #t Service;ServiceAddress;
    attr_accessor 	:rx_fifo,:tx_fifo #t RX_Packet_Fifo;TX_Packet_Fifo
#skipcc    
=begin #constructor
#if DISTR==1
    UDPSocket rx_socket, tx_socket;
#endif    
    
    Transceiver(Base::System* sba_s_,Base::Tile* sba_t_,Service& s_, ServiceAddress addr_) :    
    sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_),
    service(s_), address(addr_) 
    {
    #if DISTR==1        
        rx_socket.bind(INADDR_ANY, GWPORT + service);
    #endif    
    };
=end #constructor

=begin
Constructor
=end

#endskipcc    
#skip    
    def initialize(sba_system,service)
        @sba_system=sba_system
        @service=service        
        @verbose=(VERBOSE==1)
        @rx_fifo=SBA_RX_Packet_Fifo.new(self)
        @tx_fifo=SBA_TX_Packet_Fifo.new() 
        @address=service
if DISTR==1
        @port = GWPORT + @service # service is in this case the Node ID
        @rx_socket = UDPSocket.new        
        @rx_socket.bind(nil, @port)
        @tx_socket=UDPSocket.new
end # DISTR    
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
        receive_packets_from_network()
end # VM
if DISTR==1
        receive_packets()
end # DISTR

        # If there are any packets in the TX FIFO, dispatch them
        transmit_packets()	
    end #  of run()
    
    
    #==============================================================================
    
    # This is the main receiver module. 
    # It simply transfers all packets from the local node switch's tx_fifo
    # to the local rx_fifo
if VM==0
    def receive_packets_from_network() 
    #system    
#iv
#        puts "#{@service} TRX:receive_packets_from_network()" 
#        puts @sba_system.network.tx_fifo[@address].status
#            print "\n",@service," (addr:#{@address}) TRX RX:\n" 
#            print @service,":","Receiving ",@sba_system.network.tx_fifo[@address].length," packet(s) from network ...\n" 
#ev        
        while @sba_system.network.tx_fifo[@address].status==1                
            # receive a packet
            tmp_packet=@sba_system.network.tx_fifo[@address].shift #t Packet_t 
            @rx_fifo.push(tmp_packet);  
        end # while there are packets to be received.         
    end # of receive_packets_from_network
end # VM

# WV07042011
=begin
    I want to revisit the USE_THREADS system to make it more like the DISTR system. Currently we have this:

    # THREADS: we need a blocking if @transceiver.rx_fifo.has_packets, then we set @status to true, then we go into a while loop, while (@status) do work 
    # the problem is that the ServiceCore can put packets in the ServiceManager tx_queue, but the status does not reflect this

    What I want is a blocking @transceiver.receive_packets(), I guess that is easily done by blocking on the rx_fifo 
=end

if DISTR==1
    def receive_packets() 
    #system    
#iv
    if @verbose #skip
        puts "Node #{@service} receiving on port #{@port}" 
    end #skip 
#ev        
        #skip        
        tmp_packet_bytes, sender= @rx_socket.recvfrom(MAX_PACKET_SZ*NBYTES) #t SocketBuffer
        format="C"
        if WORDSZ==32
            format="L"
        else # WORDSZ==64
            format="Q"
#        else #skip
#            raise "WORDSZ not set?" #skip
        end # WORDSZ
        tmp_packet_header=tmp_packet_bytes.unpack(format*3) # FIXME: what is this for 64-bit?
        #endskip
        packet_length=getLength(tmp_packet_header) #t uint
        # Now use this to unpack with correct length
        #skip
        tmp_packet=tmp_packet_bytes.unpack(format*(3+packet_length)) # WV: IIRC, length is the number of words in the payload
        #endskip            
        @rx_fifo.push(tmp_packet)
    end # of receive_packets
end # DISTR
if USE_THREADS==1
    def receive_packets() 
    #system    
#iv
    if @verbose #skip
        puts "Node #{@service} receiving" 
    end #skip 
#ev     
        @rx_fifo.wait_for_packets()
    end # of receive_packets    
end # USE_THREADS    
    # -----------------------------------------------------------------------------
    
    def transmit_packets() 
    #system
#iv        
        if @verbose #skip
        puts "#{@service} TRX:transmit_packets(): FIFO status: #{@tx_fifo.status()}" #skip
        end #skip
        #C++ cout << "FIFO length: "<< tx_fifo.length() << endl;
#ev            
        if @tx_fifo.status()==1 # packets in FIFO 
        #iv
            print "\n",@service," (addr:#{@address}) TRX TX:","Sending: \n" if @verbose #skip
            #ev
            while @tx_fifo.status()==1
                packet=@tx_fifo.shift #t Packet_t
                service_id= getTo(getHeader(packet)) #t Service                
				dest=service_id #t ServiceAddress
#iv
                    if @verbose #skip
                puts "HEADER:"
                puts getTo(getHeader(packet)) #s/get/(int)get/
             
                print "PACKET:\n",ppPacket(packet),"\n"
                puts "service id: #{service_id}; dest addr: #{dest}"
                print "#{ getType(getHeader(packet))} #{ getReturn_as(getHeader(packet))} to #{dest} (addr:#{dest})\n" #skip
#C++                 cout << "" <<(int)getType(getHeader(packet))<< " " <<getReturn_as(getHeader(packet))<< " to " <<sba_system.service_by_address(dest)<< " (addr:" <<dest<< ")\n";
                    end #skip
#ev
if VM==0    
                @sba_system.network.rx_fifo[@address].push(packet)
else # VM==1
if DISTR==0    
                if dest==@sba_system.gw_address # GW
#                 puts "TRANSMITTING from #{@service} to GW (#{dest})"
                    @sba_system.gw_instance.transceiver.rx_fifo.push(packet)              
                else 
                    if service_id < SBA_BRIDGE_ADDR #skip
#                    puts "TRANSMITTING from #{@service} to #{dest}"
                        @sba_system.nodes[dest].transceiver.rx_fifo.push(packet) #C++ sba_system.nodes[dest]->transceiver.rx_fifo.push(packet);
                    else  #skip
#                     puts "TRANSMITTING from #{@service} to BRIDGE because #{service_id} >= #{SBA_BRIDGE_ADDR}"  #skip                     
                        warn "Found HW service" #skip
                        @sba_system.bridge.transceiver.rx_fifo.push(packet) #skip
                    end  #skip
                end
else # DISTR
    # packet is a list of words of NBYTES bytes. so we use pack to create a string
    # skip
    format="C"
    if WORDSZ==32
        format="L"
    else # WORDSZ==64
        format="Q"
#    else
#        raise "WORDSZ not set?"
    end # WORDSZ
    #endskip
    payload_length=getLength_p(packet) #t uint
    #skip
                packet_buf=packet.pack(format*(3+payload_length))
    #endskip
                host='localhost' #C++ uint32_t host=calcDestIP(dest); // assumes base_ip        
                    
    puts "Node #{@service} sending to host #{host}, port #{GWPORT+dest}" if @verbose #skip
    
                @tx_socket.send(packet_buf, 0, host, GWPORT+dest)    
end # DISTR                                
end # VM                
            end
        end
    end # of transmit_packets
end # of SBA_Transceiver class

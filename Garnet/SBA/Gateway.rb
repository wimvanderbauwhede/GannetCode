# Gateway.rb  
# 
# :title: Gannet Service-based SoC project - Gateway class
#
#  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: Gateway.rb 2557 2009-05-13 13:05:38Z socgroup $

=begin #inc
#include <fstream> //skiph
#ifdef WV_SYSTEMC
#include <systemc.h> //skipcc
#endif
#include "Base/Tile.h" //skipcc
#include "GatewayTile.h" //skiph
#include "Base/System.h" //skipcc
#include "System.h" //skiph
#include "ServiceManagerObjects.h" //skipcc
#include "TaskDescription.h" //skipcc
#include "ServiceConfiguration.h" //skipcc
#include "Gateway.h" //skiph
#include "Interface.h"
=end #inc

require "SBA/TaskDescription.rb"
require "SBA/Interface.rb"
class SBA_Gateway 
    include  SBA
    
    #H    	Base::System* sba_system_ptr;
    #H    	Base::Tile* sba_gwtile_ptr;    
    
    attr_accessor :service,:address, #t Service;ServiceAddress;
    :vmif, #t Interface;
    :tasks_stack #t Stack<MAX_NTASKS>;
#    attr_reader :task_counter,:core_status,:tasks, #t Counter;Core_Status;TaskDescList
    attr_reader :core_status,:tasks, #t Core_Status;TaskDescList
    :rx_fifo,:request_fifo,:tx_fifo #t Packet_Fifo;Packet_Fifo;Packet_Fifo

    #skipcc
=begin #constructor
 Gateway(Base::System* sba_s_, Base::Tile* sba_t_,Service s_,ServiceAddress addr_,TaskDescList& td_) 
		:  sba_system_ptr(sba_s_), sba_gwtile_ptr(sba_t_),
		 service(s_),
        address(addr_),
        vmif(sba_s_, sba_t_),
        tasks_stack(0),
//		 task_counter(0),
		 core_status(CS_idle),
		 tasks(td_)
		  {		    
		  };
=end #constructor
    #endskipcc
    
    #skip    
    # object constructor
    def initialize(sba_system,gw_tile,service,address,tasks) #t (void;Service;TaskDescList&)
        @sba_system=sba_system   
        @gw_tile=gw_tile
        @service= service
        @tasks=tasks #t TaskDescList
        # just to have a clear overview of the datastructures:
        @rx_fifo=[]
#if DATA==1        
#        @request_fifo=[]
#endif // DATA        
        @tx_fifo=[]
#        @task_counter=0
        @address=address
        @tasks_stack = SBA_Stack.new(MAX_NTASKS,0)
        @vmif = SBA_Interface.new(self,sba_system)        
        @core_status=CS_idle # other states: ready|busy|done
        @v=(VERBOSE==1)
#        @results_status=[]
    end
    #endskip
    
    # ------------------------------------------------------------------------------
    #
    # Main methods
    #
    
    # At every timestep, this function checks for events:
    #   -is there a packet for me?
    #   -has service processing finished? 
    def run()
    #tile
        # -Parse the task description. This is the most important part of the gateway
        parse_task_description()
        if @gw_tile.finished == false
            # If there are any packets in the TX FIFO, dispatch them
if VM==0            
            transmit_packets()
end # VM
            # -Check if there are new packets and receive them (i.e. put in FIFO)        
            #WV16122010: this now blocks on the read for threads!
            receive_packets()
            # -Check the data fifo
if VM==0            
            store_result_packets()
end # VM            
            # This is for use by the DATA service and also (later, maybe,) neighbours
            #		dispatch_data_packets(sba_system)		
        end
    end # of run()
    # ==============================================================================
    def parse_task_description()
    #tile
#    #system
    #iv
#        puts "Gateway: parse_task_description()"
#        puts "status: #{@core_status}; io_mech: #{@sba_system.io_mech}"
    #ev
            # @tasks is a list of pairs; each pair is a task_description_packet and an associative list called data_packets.
            # the latter has the structure {Symbol.Name => value}
            # so: [[Symbol,...],[{Symbol.Name => value},...]]
#            task_id=0 #t uint # dummy            
if VERBOSE
    puts "Gateway: Interface receive()" 
end # VERBOSE
                ntdcs=@vmif.receive(@core_status) #t uint    
                if ntdcs!=0 and @tasks_stack.size>0 # ntdcs = 0 means no action
                    @core_status = CS_busy
                    task_id=@tasks_stack.pop #t uint
                    @vmif.iodescs[task_id]=ntdcs
                    tdc=@vmif.tdcs.shift #t Bytecode                        
                    task_description=SBA_TaskDescription.new(tdc,task_id)                    
                    task_description_packet_list=task_description.Packets #t Packet_List                
                    # allocate space for result
                    #C++ Word_List nullwl;
                    #C++ nullwl.push_back((Word)0);
                    @gw_tile.result_store.mput(task_id,[NULL]) #s/.NULL./nullwl/
                    # push task packet on TX fifo                		              
                    if QUIT==1
                       # puts "QUIT==1" 
                       # puts task_description_packet_list 
                        exit(1)                
                    end # QUIT              
#                    puts   task_description_packet_list.inspect if @v #skip
                    # Put all packets in the TX FIFO
                    # I guess we could put them straight into the gwtile.tx_fifo
                    for task_description_packet in task_description_packet_list #t Packet_List
                        #iv
                        if @v #skip
                        puts "GATEWAY SENDS PACKET:" 
                        puts ppPacket(task_description_packet) 
                        end #skip 
#                       puts "GATEWAY parse_task_description: pushing packet on tx_fifo"
                        #ev            
if VM==0                                   
                        @tx_fifo.push(task_description_packet)
else # VM==1                        
                        transmit_packet(task_description_packet)
end # VM                        
                        
                    end                                                                                         
                end         
        
    end # of parse_task_description	
    # ------------------------------------------------------------------------------
    
    def receive_packets() 
        #tile
        #iv
#        puts "GW: receive_packets()" if @v #skip 
        print_ok=0 #C++ uint print_ok=0;         
        if @gw_tile.transceiver.rx_fifo.status()==1         
            print_ok=1 
            print "\nGateway: ",@service," (#{@address}):\n" if @v #skip 
            #skip                        
            print "Gateway:","Receiving ",@gw_tile.transceiver.rx_fifo.length," packet(s) RX ...\n" if @v
            #endskip                        
            print "Gateway: TYPE: " if @v #skip
        end
            #ev
        
        while @gw_tile.transceiver.rx_fifo.has_packets() # .status()==1            
            # receive a packet
            rx_packet= @gw_tile.transceiver.rx_fifo.shift #t Packet_t 
            #iv            
#            print getType(getHeader(rx_packet))," " #C++ cout << (int)getType(getHeader(rx_packet)) << " ";
            #ev            
            # check packet type          
            
            if  getType(getHeader(rx_packet)) == P_data
                # if DATA or RESULT, store in data fifo
                puts "if DATA or RESULT, store in data fifo" if @v #skip
                @rx_fifo.push(rx_packet)
if VM==1                
                store_result_packets()
                if @gw_tile.finished
                    break
                end
end # VM                
#if DATA==1                
            elsif  getType(getHeader(rx_packet)) == P_request
                # A request packet contains a list of variable names
                # The answer to a request is a stream of data packets
                @request_fifo.push(rx_packet)
#endif        
            else  
                #  means trouble
                
#iv
                puts ppPacket(rx_packet)
#ev                
                puts "Gateway can only receive data/result: #{ getType(getHeader(rx_packet))}" #C++ cerr << "Gateway can only receive data/result" <<  getType(getHeader(rx_packet)) << endl;//.to_uint 
                exit #C++ exit(0);
            end 
            
        end # while there are packets to be received. Alternatively, we could receive one packet per cycle. See which works best?
        
        
    end # of receive_packets
    # -----------------------------------------------------------------------------
    
    def store_result_packets()  # store DATA packets
    #tile
    #system
        puts "GW: store_result_packets()" if @v #skip
            while @rx_fifo.length>0		
                # this should also check the subtask list and update the packet status
                data_packet=@rx_fifo.shift #t Packet_t
                label= getReturn_as(getHeader(data_packet)) #t Word
                result=getPayload(data_packet) #t Word_List
                
                task_id=getTask(label) #t uint # is OK, Return_as must have task/subtask information
                #iv
                print "\n",@service,":","Storing value #{ppPayload(result)} for #{task_id} ...\n"  if @v #skip
#                # C++ cout << "\nStoring value "<< ppPayload(result) << " for " << task_id << "...\n"; 
                #ev
                # For static allocation, I don't have .has, so we either need a different mechanism or we ignore the issue ;-)
                    #iv
#                        puts "Gateway: Interface send()"
                    #ev
                        @vmif.send(result,task_id) #sysc vmif_send(result,task_id);
if DISTR==1
                        exit(0)
end # DISTR                                                
                        @tasks_stack.push(task_id)
                        stack_full = MAX_NTASKS #t uint
                        if @tasks_stack.size==stack_full
                            @core_status = CS_idle
#                            puts "io_mech: #{@sba_system.io_mech}"
                            if @sba_system.io_mech==1 #skipsysc
#                            raise "MOVE TO VMIF" #-> can VMIF access @gw_tile?
                                @gw_tile.finished=true #skipsysc                                           
                            end #skipsysc                                
#iv
#                            puts @tx_fifo.length() #skipsysc
#                            puts @gw_tile.transceiver.tx_fifo.length() #skipsysc
#                            puts @gw_tile.transceiver.rx_fifo.length() #skipsysc
#ev                                          
                        end                     
            end # while
    end # of store_result_packets
    # -----------------------------------------------------------------------------
    #WV16122010: we now use transmit_packet(packet) in parse_task_description()
    # so transmit_packet() is obsolete, as is the @tx_fifo
    # As I forgot how this gets done in SystemC, I'll ifdef it
#ifndef SYSC    
    def transmit_packet(packet) #t void (Packet_t)
    
        #tile
        #iv
        if @tx_fifo.length>0 
            print "\n","Gateway:","Sending: \n"        if @v #skip              
            puts "\nGateway: TX Fifo contains #{tx_fifo.size()} packets" if @v #skip
        end
        #ev
        #iv
        if @v #skip
                print " packet <#{ getType(getHeader(packet))}> <#{getReturn_as(getHeader(packet))}> to TX for #{getTo(getHeader(packet))}\n" #C++ cout << " packet <"<<(int) getType(getHeader(packet)) <<"> <"<<getReturn_as(getHeader(packet))<<"> to TX for "<< (int)getTo(getHeader(packet))<<"\n";
        end #skip 
        #ev
                @gw_tile.transceiver.tx_fifo.push(packet)
#        #iv
#                puts "running TRX" if @v #skip
#        #ev
if DISTR==1
                @gw_tile.transceiver.transmit_packets() # to avoid deadlock, VM-specific # was run()
else # DISTR
    # could this work with transmit_packets as well?
                @gw_tile.transceiver.run() # to avoid deadlock, VM-specific # was run()
end # DISTR                    
    end # of transmit_packet
#endif // SYSC     
if VM==0    
    def transmit_packets() 
    #tile
       
        #iv
        if @v #skip
        if @tx_fifo.length>0 
            print "\n","Gateway:","Sending: \n"                     
            puts "\nGateway: TX Fifo contains #{tx_fifo.size()} packets"
        end
        end #skip
        #ev
        # FIXME: tx_fifo should be a single-packet buffer!
            while @tx_fifo.length>0
                packet = @tx_fifo.shift #t Packet_t
        #iv
                if @v #skip
                print " packet <#{ getType(getHeader(packet))}> <#{getReturn_as(getHeader(packet))}> to TX for #{getTo(getHeader(packet))}\n" #C++ cout << " packet <"<<(int) getType(getHeader(packet)) <<"> <"<<getReturn_as(getHeader(packet))<<"> to TX for "<< (int)getTo(getHeader(packet))<<"\n";
                end #skip 
        #ev
                @gw_tile.transceiver.tx_fifo.push(packet)
        #iv                
                puts "running TRX" if @v #skip
        #ev
                @gw_tile.transceiver.run() # to avoid deadlock, VM-specific
            end
            #iv
#            print "\n" 
            #ev
        
    end # of transmit_packets
end # VM    
#if DATA==1
#    # most likely obsolete    
#    def allocate_data_packet_store(data,value) #C++ void Gateway::allocate_data_packet_store(uint data,Word value) {
#        #C++ System& sba_system=*((System*)sba_system_ptr);
#        @sba_system.data_packet_store[data]=value	
#        #	@data_packet_store[data]=value
#    end # of allocate_data_packet_store
#  
#    # =======
#    # This is a purely-for-testing way of reading data from a file.
#    # The One True Way is to receive data packets.
#    #skip    
#    def read_data(data_file) 
#        data={}
#        File.open(data_file,"r") do |file|
#            nlines=file.gets
#            while line=file.gets
#             (key,value)= line.split(" ")
#                data[key.to_i]=value.to_i
#            end
#        end        
#        return data
#    end
#    #endskip    
#endif // DATA  


#if DATA==1
=begin #C++
    map<uint,Word> Gateway::read_data(string data_file) { //H
          map<uint,Word> data;
          ifstream inFile;
          inFile.open(data_file.c_str());
          if (inFile.fail()) {
              cerr << "Can't open file "<<data_file<<" for reading." << endl;
              exit(1);
          }
          uint ndata,dkey;
          Word dvalue;
          inFile >>ndata;
          for (uint i=0;i<ndata;i++) {
                inFile >> dkey >> dvalue;
                data[dkey]=dvalue;
            }
            return data;
         }
=end #C++    
#endif // DATA
    
end # of SBA_Gateway class

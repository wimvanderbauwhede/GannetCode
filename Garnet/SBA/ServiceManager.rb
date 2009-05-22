#
# Gannet Service-based SoC project - Service Manager class
#
#
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
#
# ==============================================================================
#
#  Gannet Service-based SoC project - Service Manager class
#
# ==============================================================================
#
# $Id: ServiceManager.rb 2566 2009-05-15 10:27:08Z socgroup $

=begin #inc
#include "Base/System.h" //skipcc
#include "Base/Tile.h" //skipcc
#include "System.h" //skiph
#include "Tile.h" //skiph
#include "ServiceManagerObjects.h" //skipcc
#include "ServiceManager.h" //skiph
#ifdef CYCLES
#include "../build/cycle.h"
#endif
=end #inc

require "SBA/ServiceManagerObjects.rb"

# The Service Manager is the core of the SBA. This is the module that does all bookkeeping and subtask "parsing"
# Essentially, it queues subtasks, and parses the "current" subtask according to very simple rules:
#  * Subtask (S|F) => delegate
#  * Data (D|V|A|U|L) => request
#  * Built-in or Quoted (B|Q) => store
# The Quoted kind is essential as it allows to defer evaluation to the cores.
#
class SBA_ServiceManager
    include SBA

    #H    	Base::System* sba_system_ptr;
    #H    	Base::Tile* sba_tile_ptr;

    # service is mainly used for debugging
    attr_reader :service,:address, #t Service;ServiceAddress;
    :service_id, #skip
    :debug_service, #t Service;
    :debug_all, #t bool;
    #    :fifo, #t bool;
    :service_core_ram, #t MemAddress;
    :scid #skip
    attr_accessor :data_fifo,:subtask_fifo,:request_fifo, #t Packet_Fifo;Word_List;Packet_Fifo;
    :subtask_reference_fifo,:subtask_code_fifo,:tx_fifo, #t Packet_Fifo;Packet_Fifo;Packet_Fifo;
    :pending_subtasks_fifo, #t Subtasks;
    :status, #skip
    :subtask_list, #t Subtask_List;
    :results_store, #skip
    :symbol_table,:code_status, #t Symbol_Table;CodeStatus_Table;
    :subtasks_address_stack, #t Stack<SUBTASKS_SZ>;
    :data_address_stack, #t Stack<DATA_SZ-NREGS>;
    :current_subtask, #skip
    :arg_addresses, #skip
    :core_status, #skip
    :core_return_type, #skip
    :ack_ok, #skip
    :opcode,:n_args, #skip
    :register_set, #skip
    :request_table #skip

    #        @service_core_ram=[] #@code_offset+@code_stacksize

    #skipcc

=begin #constructor
#if MULTI_THREADED_CORE==1
    Word_List results_store[NCORE_THREADS];
    MemAddresses arg_addresses[NCORE_THREADS];
    Subtask current_subtask[NCORE_THREADS];
    Core_Status core_status[NCORE_THREADS];
    uint opcode[NCORE_THREADS];
    uint n_args[NCORE_THREADS];
    uint scid[NCORE_THREADS];
    Packet_Type core_return_type[NCORE_THREADS];
    bool ack_ok[NCORE_THREADS];
    Service service_id[NCORE_THREADS];
#else
    Word_List results_store;
    MemAddresses arg_addresses;
    Subtask current_subtask;
    Service service_id;    
    Core_Status core_status;
    uint scid;
    Packet_Type core_return_type;
    uint opcode;
    uint n_args;
    bool ack_ok;
#endif
   bool status;
    
RegisterEntry register_set[NREGS];
//Requests request_table[NREGS]; 
RequestTable request_table;
    
ServiceManager(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_,ServiceAddress& addr_)
			: sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_), service(s_), address(addr_),
			debug_service(0), debug_all(true),
#ifdef SBA_USE_ADDRESS_STACKS
			subtasks_address_stack(SUBTASKS_OF),
			data_address_stack(DATA_OF+NREGS),
#endif	
#if MULTI_THREADED_CORE==0
            current_subtask(0), 
            service_id(s_),
            core_status(CS_idle),scid(0),core_return_type(P_data),            
#endif			
            status(true)
             {
#if MULTI_THREADED_CORE==1        
                for (uint i=0;i<NCORE_THREADS;i++) {
                    results_store[i].clear();
                    arg_addresses[i].clear();
                    current_subtask[i]=0;
                    core_status[i]=CS_idle;
                    core_return_type[i]=P_data;
                    service_id[i]=s_; // WV25112008: for now. This should be more flexible to support if/return/+/== 
/*                    scid[i]= // the rule is: loop over once all scids; build a list of

*/
                }                        
#endif
            };
=end #constructor  

    #endskipcc
    #skip
    def initialize(sba_tile,service)
        @debug_all=(DEBUG_ALL==1)
        @debug_service=0
        @v=(VERBOSE==1)
        @debug_subtasks={} #skip
        @sba_tile=sba_tile
        @service=service
        @service_id=service
        @status=false # VM only!
        # RX FIFOs
        @data_fifo=[]
        @subtask_code_fifo=[]
        @subtask_reference_fifo=[]
        @request_fifo=[]
        # TX FIFO
        @tx_fifo=[]
        # Task processing datastructures
        @subtask_fifo=[] # this is a FIFO containing code addresses

        # New memory layout, assuming separate small RAMs
        # Symbol table stores the symbol but the Task field is used for the data status (DS)
        @symbol_table={} # Word symbol_table[DATA_SZ]; // must be initialised to 0
        # Code status is (Present,Activation Requested) so 0..3
        # If we use the same address space for code and subtasks, this can go into the subtask list
        @code_status={} # CS_t code_status[CODE_SZ]; // must be initialised to 0
        # Memory organisation
        if VM==1
            @subtasks_address_stack=SBA_Stack.new(SUBTASKS_SZ,SUBTASKS_OF)
        end # VM
        @subtask_list=SBA_Subtask_List.new()
        @data_address_stack=SBA_Stack.new(DATA_SZ-NREGS,DATA_OF+NREGS)

        # WV: use direct addressing instead of the stack/lookup based system
        # all code packets have a maximum size, so use fixed-size mem blocks
        # addresses are worked out at compile time except for lambda functios
        # the latter use a limited set of addresses (e.g. 16) starting from 1
        # The first word of the chunk might be reserved for status information

        #WV16082008: To support a "multi-threaded" core, we need the status per thread.
        # We also need an arg_addresses queue per thread, and an opcode.
        # We need a results_store per thread.
        # For HW, we do, or we need at least an input mux for the FIFO
        # Also, we must know to what subtask the result belongs.
        # We must also have a list of the "current" subtask per thread

        @pending_subtasks_fifo=[] # this is a FIFO holding uint8
        @arg_addresses=[] # of course, this is not a FIFO but a fixed-size array
        @results_store=[] # NONE

        if MULTI_THREADED_CORE==1
            @opcode=[]
            @n_args=[]
            @scid=[]
            @current_subtask=[]
            @core_status=[] # other states: ready|busy|done|managed
            @core_return_type=[] # can be subtask, data, maybe even builtin
            @service_core_ram=[] # @code_offset+@code_stacksize
            @service_id=[]
            if VM==1
                @ack_ok=[]
            end # VM

            tmp_tid=0
            for scid in services[service].keys
                next if scid=='Addr'
                nthreads=service[service][scid][2]
                for i in 1..nthreads
                    @scid[tmp_tid]=scid
                    tmp_tid+=1
                end
            end

            for i in 0..NCORE_THREADS-1
                if VM==1
                    @ack_ok[i]=1
                end # VM
                @n_args[i]=0
                @arg_addresses[i]=[]
                @results_store[i]=[]
                @current_subtask[i]=0
                @core_status[i]=CS_idle # other states: ready|busy|done|managed
                @core_return_type[i]=P_data # can be subtask, data, maybe even builtin
                @service_core_ram[i]=DATA_SZ+2+SERVICE_CORE_RAM_SZ*i # @code_offset+@code_stacksize
                @service_id[i]=service # WV25112008: for now. This should be more flexible to support if/return/+/==
            end

        else # MULTI_THREADED_CORE==0
            if VM==1
                @ack_ok=1
            end # VM
            @scid=0;
            @current_subtask=0
            @core_status=CS_idle # other states: ready|busy|done|managed
            @core_return_type=P_data # can be subtask, data, maybe even builtin
            @service_core_ram=DATA_SZ+2 # @code_offset+@code_stacksize
            @service_id=service
        end # MULTI_THREADED_CORE

        # With the new ServiceCore register @state_register, there should be no need for this initial value
        #        @sba_tile.data_store.mput(@service_core_ram,[MAX_WORD])
        @register_set=[]
        @request_table=SBA_RequestTable.new()
        for reg in 1..NREGS
            @register_set.push(SBA_RegisterEntry.new(0,0,0,0))
#            @request_table.push([])
        end

    end
    #endskip
    # -----------------------------------------------------------------------------
    #
    # Main methods
    #
    # At every timestep, the run() function checks for events:
    # - is there a packet for me?
    # - has my service processing finished?
    # WV 08/11/2007: TODO: for VM performance, every call should be inside an if block
    def run()
        #      puts "Running ServiceManager #{@service_id}"
        #ifdef CYCLES_DETAILED
        #C++      ticks t0=getticks();
        #endif

        #ifdef CYCLES
        #C++      ticks t1=getticks();
        #endif
        #tile
        # - Check if there are new packets and receive them (i.e. put in FIFO)
        #ifdef CYCLES_DETAILED
        #C++      ticks t1_1=getticks();
        #endif
        if @sba_tile.transceiver.rx_fifo.status==1
            receive_packets()
        end
        #ifdef CYCLES_DETAILED
        #C++      ticks t1_2=getticks();
        #endif
        # On receipt of a subtask code packet
        if @subtask_code_fifo.length>0
            store_subtask_code()
        end

        # This is for use by the DATA service and also, later, neighbours
        if @request_fifo.length>0
            dispatch_data_packets()
        end
        #ifdef CYCLES_DETAILED
        #C++      ticks t1_3=getticks();
        #endif

        # On receipt of a subtask reference packet
        if @subtask_reference_fifo.length>0
            activate_subtask()
        end
        #ifdef CYCLES_DETAILED
        #C++      ticks t1_4=getticks();
        #endif

        # - Check the data fifo and store any packet in there
        if @data_fifo.length>0
            store_data()
        end
        #ifdef CYCLES_DETAILED
        #C++      ticks t1_5=getticks();
        #endif

        # - Check the subtask fifo
        # - Parse the subtask & take resulting actions. This is the most important part of the wrapper
        if @subtask_fifo.length>0
            parse_subtask()
        end
        # - Check if the subtask being processed has finished
        #ifdef CYCLES_DETAILED
        #C++      ticks t1_6=getticks();
        #endif
        if MULTI_THREADED_CORE==1
            multi_threaded_core_control()
        else # MULTI_THREADED_CORE==0
            core_control()
        end # MULTI_THREADED_CORE

        #ifdef CYCLES_DETAILED
        #C++      ticks t1_7=getticks();
        #endif


        #ifdef CYCLES_DETAILED
        #C++      ticks t1_8=getticks();
        #endif


        #ifdef CYCLES_DETAILED
        #C++      ticks t1_9=getticks();
        #endif

        # If there are any packets in the TX FIFO, dispatch them
        #        if @tx_fifo.length>0
        #          transmit_packets()
        #        end

        #ifdef CYCLES_DETAILED
        #C++      ticks t12=getticks();
        #endif

        # the second part is because of local loopback. Seems to mess up threads though
        #WV23122008: still not OK, gives deadlock for recursive lambdas.
        @status = ( (@pending_subtasks_fifo.length>0) || (@core_status != CS_idle) || ((@data_fifo.length>0) || (@request_fifo.length>0) || (@subtask_reference_fifo.length>0)) )
        # || (@tx_fifo.length>0) )
        #iv
        if @debug_all or @service==@debug_service
        puts "ServiceManager #{@service_id} #{@status} = p:#{(@pending_subtasks_fifo.length>0)} || CS:#{(@core_status != CS_idle)} || f:#{((@data_fifo.length>0) || (@request_fifo.length>0) || (@subtask_reference_fifo.length>0))}" #skip
        end
        #ev
        # || #{@tx_fifo.length>0}"

        #	@status = (true && ((@sba_tile.transceiver.rx_fifo.length>0) || (@pending_subtasks_fifo.length>0) || (@subtask_fifo.length>0) or (@subtask_list.subtasks.length>0) or (@data_fifo.length>0) or (@request_fifo.length>0) || (@subtask_code_fifo.length>0) || (@subtask_reference_fifo.length>0) || (@tx_fifo.length>0) || (@core_status != CS_busy))) #skip

        #C++ //status=true;
        #ifdef CYCLES_DETAILED
        #C++      ticks t22=getticks();
        #endif
        # puts "Service #{@service} STATUS: #{@status}"
        if MEM==1
            puts "#{@service} MEM USAGE: #{DATA_SZ-@data_address_stack.size()}" # if DATA_SZ!=@data_address_stack.size()
        end # MEM
        #ifdef CYCLES
        #C++      ticks t2=getticks();
        #endif // CYCLES
        #ifdef CYCLES_DETAILED
        #C++      std::cout << "CYCLES[sanity]: "<<service<<": "<<  elapsed(t1,t0) <<"\n";
        #C++      std::cout << "CYCLES[tile]: "<<service<<": "<<  elapsed(t1_1,t1) <<"\n";
        #C++      std::cout << "CYCLES[receive_packets]: "<<service<<": "<<  elapsed(t1_2,t1_1) <<"\n";
        #C++      std::cout << "CYCLES[store_subtask_code]: "<<service<<": "<<  elapsed(t1_3,t1_2) <<"\n";
        #C++      std::cout << "CYCLES[activate_subtask]: "<<service<<": "<<  elapsed(t1_4,t1_3) <<"\n";
        #C++      std::cout << "CYCLES[store_data]: "<<service<<": "<<  elapsed(t1_5,t1_4) <<"\n";
        #C++      std::cout << "CYCLES[parse_subtask]: "<<service<<": "<<  elapsed(t1_6,t1_5) <<"\n";
        #C++      std::cout << "CYCLES[core_control]: "<<service<<": "<<  elapsed(t1_7,t1_6) <<"\n";
        #C++      std::cout << "CYCLES[prepare_subtask]: "<<service<<": "<<  elapsed(t1_8,t1_7) <<"\n";
        #C++      std::cout << "CYCLES[dispatch_data_packets]: "<<service<<": "<<  elapsed(t1_9,t1_8) <<"\n";
        #C++      std::cout << "CYCLES[transmit_packets]: "<<service<<": "<<  elapsed(t12,t1_9) <<"\n";
        #C++      std::cout << "CYCLES[status]: "<<service<<": "<<  elapsed(t22,t12) <<"\n";
        #endif // CYCLES_DETAILED
        #ifdef CYCLES
        #C++      std::cout << "CYCLES[run]: "<<service<<": "<<  elapsed(t2,t1) <<"\n";
        #endif
    end #  of run()
    # =============================================================================
    # This is the main receiver module for the service manager.
    # As such, it should probably be an object and not a method for the SBA_ServiceManager object.
    # At the moment, I use a FIFO per packet type. In practice, all these FIFOs would be emulated in RAM.
    # Still, having a FIFO per type would facilitate processing, so I probably keep the concept
    # WV 07/11/2007: TODO: the if construct should be a separate function so we can call it to bypass the TX fifo
    # It should be a case anyway, or maybe even better: a dispatch table!
=begin
Packet_Fifo fifo[4];// 5 types of packets but code and subtask go in same fifo

void demux(Packet packet) {
    uint ptype=getType(getHeader(packet))
    // This transforms 1 2 3 4 5 into 0 0 1 2 3
    uint index=ptype+(unsigned int)(ptype==1)-2;
    fifo[index].push_back(packet)
}
=end
    def demux_packets_by_type(packet) #t void (Packet_t&)
        # @status=true
        #        pheader = getHeader(packet) #t Header_t
        #        ptype = getType(pheader) #t Packet_type_t
        #        case ptype
        packet_type=getType(getHeader(packet)) #t Packet_type_t
#iv
if @debug_all or @service==@debug_service
    #C++    cout << service <<" demux_packets_by_type(): Got packet of type "<<(int)packet_type<<"\n";  
        puts "#{@service} demux_packets_by_type(): Got packet of type #{packet_type}" #skip
end           
#ev        
        case packet_type
        when P_data
            @data_fifo.push(packet)
        when P_code
            @subtask_code_fifo.push(packet)
        when P_subtask
            @subtask_code_fifo.push(packet)
        when P_request
            @request_fifo.push(packet)
        when P_reference
            @subtask_reference_fifo.push(packet)
            #C++ break;}
        else #C++   default:
            raise "Packet Type #{packet_type} not recognised"
        end
    end # of demux_packets_by_type

    def receive_packets()
        #ifdef CYCLES_INT
        #C++      ticks t1=getticks();
        #endif
        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} receive_packets(): #{@sba_tile.transceiver.rx_fifo.length};" #sysc
        end
        #ev
        while @sba_tile.transceiver.rx_fifo.length>0
            rx_packet= @sba_tile.transceiver.rx_fifo.shift #t Packet_t
            #iv                        
            puts "#{@service} receive_packets(): received packet from TRX" #sysc
            #ev
            #            puts "PACKET:",rx_packet.inspect #skip
            # clear the link
            # check packet type
            demux_packets_by_type(rx_packet)
=begin
            if   getType(getHeader(rx_packet))== P_data            
                @data_fifo.push(rx_packet)
            elsif  getType(getHeader(rx_packet)) == P_subtask 
                # if TASK, store in subtask fifo
                @subtask_code_fifo.push(rx_packet)    
            elsif  getType(getHeader(rx_packet)) == P_request
                # A request packet contains a list of variable names
                # The answer to a request is a stream of data packets
                @request_fifo.push(rx_packet)
            elsif  getType(getHeader(rx_packet)) == P_reference            
                @subtask_reference_fifo.push(rx_packet)
            elsif  getType(getHeader(rx_packet)) == P_code
                @subtask_code_fifo.push(rx_packet)
            else 
              raise "Packet Type #{ getType(getHeader(rx_packet))} not recognised"
            end # leave the door open for other types
=end
        end # while there are packets to be received. Alternatively, we could receive one packet per cycle. See which works best?
        #ifdef CYCLES_INT
        #C++      ticks t2=getticks();
        #C++      std::cout << "CYCLES_INT[receive_packets]: "<<service<<": "<<  elapsed(t2,t1) <<"\n";
        #endif
    end # of receive_packets

    
    
    # ------------------------------------------------------------------------------------
    # Helper for code activation
# Helper for code activation
def activate_subtask_helper(task_address,tservice_id,packet,is_subtask_packet) #t void (CodeAddress;Name_t;Word_List&;bool)
#    #tile
    #iv
    if (@service!=tservice_id)
        puts "#{@service} activate_subtask_helper(): service_id #{tservice_id} <> @service #{@service}"
    end
    #ev
    if VM==1
        #iv
        puts "#{@service} SUBTASK STACK SIZE: #{@subtasks_address_stack.size}"
        #ev
        if @subtasks_address_stack.size==0
            raise "#{@service} SUBTASK STACK (#{SUBTASKS_SZ}) OVERFLOW"  #C++  std::cerr << "SUBTASK STACK OVERFLOW\n"; exit(0);
        end
        subtask_address=@subtasks_address_stack.pop #t CodeAddress
        puts "NEW SUBTASK for code #{task_address}: #{subtask_address} #{SUBTASKS_SZ-@subtasks_address_stack.size}" if @v #skip
        puts "SUBTASK requested by \n#{ppPacket(packet)}" if @v #skip
    else # VM==0
        subtask_address=task_address #t CodeAddress
    end # VM
    #iv
    if @debug_all or @service==@debug_service
        puts "#{@service} activate_subtask_helper(): address: #{subtask_address}"
    end
    #ev
    if is_subtask_packet
    @subtask_list.add(subtask_address)
    end

    if VM==1
        subtask_word=(subtask_address << 16) + task_address #t Word
    else # VM==0
        subtask_word=task_address #C++ Word subtask_word=(Word)task_address;
    end # VM
    puts "#{@service} activate_subtask_helper(): <#{task_address}> #{packet.inspect}" if @v #skip
    puts ppPacket(packet) if @v #skip
    puts @code_status.inspect if @v #skip
    # really, this should be redundant as we should send task packets, not code & ref
    # WV23072008: and with direct mem transfers there should never be a race condition!
    if VM==1
        # s/push/push_back/
        @subtask_fifo.push(subtask_word) 
    else # VM==0
        if @code_status[task_address]>=2 # code is present
            @subtask_fifo.push(subtask_word) 
            @code_status[task_address]=2 # reset "request activation"
        else # code is not present, request activation
        #iv
            puts "DEFER ACTIVATION #{task_address}:#{code_status[task_address]}" #sysc
            #ev
            @code_status[task_address]=1
        end
    end # VM
    if is_subtask_packet
    @subtask_list.return_as(subtask_address,getReturn_as_p(packet))
    @subtask_list.return_to(subtask_address,getReturn_to_p(packet))
    @subtask_list.to(subtask_address,getReturn_to_p(packet))
    @subtask_list.redir(subtask_address, getRedir_p(packet))
    @subtask_list.ack_to(subtask_address, getAck_to_p(packet))
    @subtask_list.code_address(subtask_address,task_address)
    @subtask_list.service_id(subtask_address,tservice_id)
    # FIXME sysc should support service_id!
    @service_id=tservice_id #skipsysc
    end        
end # of activate_subtask_helper

    
    
    def activate_subtask_helper_ORIG(task_address,tservice_id,packet) #t void (CodeAddress;Name_t;Word_List&)
        
        #iv
        if (@service!=service_id)
            puts "#{@service} activate_subtask_helper(): service_id #{tservice_id} <> @service #{@service}"
        end
        #ev
        if VM==1
            #iv
            puts "#{@service} SUBTASK STACK SIZE: #{@subtasks_address_stack.size}"
            #ev
            if @subtasks_address_stack.size==0
                raise "#{@service} SUBTASK STACK (#{SUBTASKS_SZ}) OVERFLOW"  #C++  std::cerr << "SUBTASK STACK OVERFLOW\n"; exit(0);
            end
            subtask_address=@subtasks_address_stack.pop #t CodeAddress
            puts "NEW SUBTASK for code #{task_address}: #{subtask_address} #{SUBTASKS_SZ-@subtasks_address_stack.size}" if @v #skip
            puts "SUBTASK requested by \n#{ppPacket(packet)}" if @v #skip
        else # VM==0
            subtask_address=task_address #t CodeAddress
        end # VM
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} activate_subtask_helper(): address: #{subtask_address}"
        end
        #ev
        @subtask_list.add(subtask_address)
        #       @subtask_list.return_as(subtask_address,getReturn_as(header))
        #       @subtask_list.return_to(subtask_address,getReturn_to(header))
        #       @subtask_list.to(subtask_address,getReturn_to(header))
        #       @subtask_list.redir(subtask_address, getRedir(header))
        #       @subtask_list.ack_to(subtask_address, getAck_to(header))
        #WV23072008: doesn't seem a jot faster ...
        @subtask_list.return_as(subtask_address,getReturn_as_p(packet))
        @subtask_list.return_to(subtask_address,getReturn_to_p(packet))
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} activate_subtask_helper(): setting TO: #{getReturn_to_p(packet)} for #{subtask_address}" #skip
            #C++ cout << service <<" activate_subtask_helper(): setting TO: "<<(int)getReturn_to_p(packet)<<" for "<<subtask_address<<endl;
        end
        #ev
            
        @subtask_list.to(subtask_address,getReturn_to_p(packet))
        @subtask_list.redir(subtask_address, getRedir_p(packet))
        @subtask_list.ack_to(subtask_address, getAck_to_p(packet))

        @subtask_list.code_address(subtask_address,task_address)
        @subtask_list.service_id(subtask_address,tservice_id)
        @service_id=tservice_id
        if VM==1
            subtask_word=(subtask_address << 16) + task_address #t Word
        else # VM==0
            subtask_word=task_address #C++ Word subtask_word=(Word)task_address;
        end # VM
        puts "#{@service} activate_subtask_helper(): <#{task_address}> #{packet.inspect}" if @v #skip
        puts ppPacket(packet) if @v #skip
        puts @code_status.inspect if @v #skip
        # really, this should be redundant as we should send task packets, not code & ref
        # WV23072008: and with direct mem transfers there should never be a race condition!
        # WV21042009: nevertheless...
        if VM==1
            @subtask_fifo.push(subtask_word) #s/push/push_back/
        else # VM==0
            if @code_status[task_address]>=2 # code is present
                @subtask_fifo.push(subtask_word) #s/push/push_back/
                @code_status[task_address]=2 # reset "request activation"
            else # code is not present, request activation
                #iv
                puts "DEFER ACTIVATION #{task_address}:#{code_status[task_address]}"
                #ev
                @code_status[task_address]=1
            end
        end # VM
    end # of activate_subtask_helper_ORIG
    # -----------------------------------------------------------------------------

    def store_subtask_code()
        #ifdef CYCLES_INT
        #C++      ticks t1=getticks();
        #endif
        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "\n#{@service} store_subtask_code()"
        end
        #ev

        # fifo.length() is an expensive op, I think.
        fifo_len = @subtask_code_fifo.length #t uint
        for i in 1..fifo_len #t uint
            # subtask code is data, but no memory has been allocated for it
            #        while @subtask_code_fifo.length>0
            # Get packet & read header
            # I think the current shift is too expensive. Maybe I can try an "unguarded" shift ...
            # s/shift/unguarded_shift()/
            subtask_code_packet=@subtask_code_fifo.shift #t Packet_t
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} store_subtask_code(): packet:"
                puts ppPacket(subtask_code_packet)
            end
            #ev

            subtask_code=getPayload(subtask_code_packet) #t Word_List
            #            subtask_code=getPayload(subtask_code_packet) # WRONG: need copy! C++ Word_List subtask_code = subtask_code_packet.payload();
            #iv
            if @debug_all or @service==@debug_service
                puts "subtask code:"
                puts ppPayload(subtask_code)
            end
            #ev
            #            code_label= getReturn_as(getHeader(subtask_code_packet)) #t Word
            code_label= getReturn_as(getHeader(subtask_code_packet)) #C++ Word code_label= subtask_code_packet[2];
            #            puts "Service: #{@service} : CODE_LABEL: #{code_label}"
            code_address=getCodeAddress(code_label) #t CodeAddress
            tservice_id=getName(code_label) #t Name_t
            @code_status[code_address]=(@code_status[code_address]==1)?1:0 # WARNING: so even if code was there, it will be overwritten
            @sba_tile.code_store.mput(code_address,subtask_code)            
            @code_status[code_address]=@code_status[code_address]|2            

            is_subtask_packet = (getPacket_type(getHeader(subtask_code_packet))==P_subtask) #t bool
            if is_subtask_packet or @code_status[code_address]&1==1
                #iv
                puts "#{@service} store_subtask_code(): activate #{code_address} now!" #sysc
                #ev
                @code_status[code_address]=@code_status[code_address]&2
                #            if getPacket_type_p(subtask_code_packet)==P_subtask
                # it's a Subtask packet or activation was requested, activate right away
                activate_subtask_helper(code_address,tservice_id,subtask_code_packet,is_subtask_packet)
            end
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} store_subtask_code(): stored #{code_label} at #{code_address}" #sysc
                puts ppPayload(@sba_tile.code_store.mget(code_address)) if @v #skip
            end
            #ev
        end
        #iv
        print "\n"
        #ev
        #ifdef CYCLES_INT
        #C++      ticks t2=getticks();
        #C++      std::cout << "CYCLES_INT[store_subtask_code]: "<<service<<": "<< elapsed(t2,t1) <<"\n";
        #endif
    end # of store_subtask_code()

    # ------------------------------------------------------------------------------------
    def activate_subtask()
        #ifdef CYCLES_INT
        #C++      ticks t1=getticks();
        #endif

#        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} activate_subtask()"
        end
        #ev
        # Activating a subtask means:
        #        fifo_len = @subtask_reference_fifo.length() #t uint
        #        for i in 1..fifo_len #t uint
        while @subtask_reference_fifo.length>0
            # Get packet & extract code reference label from payload
            subtask_ref_packet=@subtask_reference_fifo.shift #t Packet_t
            #            subtask_ref_packet=@subtask_reference_fifo.shift #t Packet_t #s/shift/unguarded_shift()/
            #            ref_label_symbol_l=getPayload(subtask_ref_packet) #C++ Word_List ref_label_symbol_l = subtask_ref_packet.payload();
            ref_label_symbol_l=getPayload(subtask_ref_packet) #t Word_List

            #            subtask_header=getHeader(subtask_ref_packet) #t Header_t
            ref_label_symbol=ref_label_symbol_l[0] #t Word
            # ref_label_symbol fields:
            # K_R:*:0:0:task:code_address:service so we can reconstruct this easily
            # TODO: task must be added to the code address to have different code pages per task!
            # code_address=((getTask()<<FS_Task)>>FS_Subtask)+getSubtask()
            # for this, the Symbol must be Task:Subtask:Name

            #iv
            puts ppSymbol(ref_label_symbol)
            #ev
            puts "Service: #{@service} : REF_LABEL: #{ref_label_symbol}" #skip
            code_address=getCodeAddress(ref_label_symbol) #t CodeAddress
            tservice_id=getName(ref_label_symbol) #t Name_t
            activate_subtask_helper(code_address,tservice_id,subtask_ref_packet,true)
        end # of while
        #ifdef CYCLES_INT
        #C++      ticks t2=getticks();
        #C++      std::cout << "CYCLES_INT[activate_subtask]: "<<service<<": "<<  elapsed(t2,t1) <<"\n";
        #endif
    end # of activate_subtask()
    # ------------------------------------------------------------------------------------
    # store DATA packets on arrival. Only DATA resulting from requests and results form subtasks should ever arrive here.
    # With direct addressing, the label contains the memory address, so no need for a lookup. But we need a separate, small
    # memory to store the status information -- or we need a word per block of data with this information
    def store_data()
        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} store_data()"
        end
        #ev
        while @data_fifo.length>0
            # this should also check the subtask list and update the packet status
            data_packet=@data_fifo.shift #t Packet_t
            #iv
            if @debug_all or @service==@debug_service
                puts ppPacket(data_packet) #C++ cout << ppPacket(data_packet) <<"\n";
            end
            #ev

            label= getReturn_as(getHeader(data_packet)) #t Word
            data_address=getSubtask(label) & F_DataAddress #t MemAddress

            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} store_data() ADDRESS: #{data_address}"
            end
            #ev
            if getStatus(@symbol_table[data_address])!=DS_present
                #iv
                if @debug_all or @service==@debug_service
                    print "#{@service} store_data() STATUS: "
                    puts getStatus(@symbol_table[data_address]) #C++ cout << (int)getStatus(symbol_table[data_address])<<"\n";
                end
                #ev

                if getLength_p(data_packet)==0
                    @symbol_table[data_address]=setStatus(@symbol_table[data_address],DS_present)
                else # Length!=0
                    # 0 (absent) or 2 (requested)
                    # 1

                    data_packet_payload=getPayload(data_packet) #t Payload_t
                    
                    # To support partial assignment to a field inside a tuple
                    # this will need some work:
                    # we actually need to merge the existing content with the new content
                    # simplistically:
                    fsize=0 #t uint
                    if (data_address<DATA_OF+NREGS)
                        fsize=@register_set[data_address].fsize       
                    end
                    if fsize==0
                        @sba_tile.data_store.mput(data_address,data_packet_payload)
                    else
                        offset=@register_set[data_address].offset #t uint
                        current_content = @sba_tile.data_store.mget(data_address) #t Word_List
                        for i in 0 .. fsize-1 #t uint
                            
                            #ifdef STATIC_ALLOC
                            #C++ current_content.at(i+offset,data_packet_payload[i]);
                            #else
                            current_content[i+offset]=data_packet_payload[i]
                            #endif
                        end                    
                        @sba_tile.data_store.mput(data_address,current_content)
                    end
                    
                    @symbol_table[data_address]=setStatus(@symbol_table[data_address],DS_present)
                    #FIXME: should I update the status of the reg_table as well?
                    if (data_address<DATA_OF+NREGS)
                        @register_set[data_address].status=RDS_present                                 
                    end                    
                    
                    #iv
                    if @debug_all or @service==@debug_service
                    puts "#{@service} store_data() : stored payload #{data_packet_payload.inspect} at address #{data_address}" #skip
                        puts "#{@service} store_data() address #{data_address} STATUS: "
                        puts getStatus(@symbol_table[data_address]) #C++ cout << (int)getStatus(symbol_table[data_address])<<"\n";
                    end
                    #ev
                end # if Length==0
                subtask=getSubtask(@symbol_table[data_address]) #t Subtask
                #iv
                                if @debug_all or @service==@debug_service
                                    puts "#{@service} store_data() SUBTASK: #{subtask}"
                                end
                #ev          
                #sysc            subtask_list.lock();                      
                if @subtask_list.status(subtask)!=STS_deleted
                    @subtask_list.decr_nargs_absent(subtask)
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} store_data() SUBTASK #{subtask} in list"
                    end
                    #ev

                    if @subtask_list.nargs_absent(subtask)==0
                        @subtask_list.status(subtask,STS_pending)
                        @pending_subtasks_fifo.push(subtask)
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "#{@service} store_data() SUBTASK #{subtask} pushed onto pending_subtasks_fifo"
                        end
                        #ev                        
                    else
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "#{@service} store_data() SUBTASK #{subtask} still missing #{@subtask_list.nargs_absent(subtask)} arg(s)"
                        end
                        #ev                                                
                    end
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} store_data() done for #{data_address} of subtask #{subtask}" #sysc
                    end
                    #ev  
            end # not STS_deleted
                #sysc            subtask_list.unlock();
                #WV25112008: I think this is obsolete. The compiler rewrites CACHE calls as VAR calls
                # But maybe it is still possible to call a VAR before it's there? I think not:
                # (S0 (S1 (cache 'c1 (S2 ...)) (cache 'c2 (S3 ...)) (S4 c1) c2))
                # =>
                # (S0 (S2-ret '(S3-ret '(S1 c1 c2 (S4 c1) c2) (S3-var<c2> ...)) (S2-var<c1> ...)))
                # or, maybe better, if we can make it work:
                # (S0 (S1-ret '(S1 c1 c2 (S4 c1) c2) (S2-var<c1> ...) (S3-var<c2> ...)))

                # Here, check if K_C and were there pending requests?
                #WV26112008: I think this belongs in core_control, or rather in a helper function
                #                    if getKind(label)==K_C # WV: FIXME: look at factoring this out with dispatch_data_packets()
                #                        reg_addr=getReg(label) #t uint
                #                        @register_set[reg_addr].status=RDS_present
                #                        if @request_table[reg_addr].length!=0
                #                        #FIXME: STATIC_ALLOC needs <Word>, dyn alloc needs Requests
                #                            for request_label in @request_table[reg_addr] #t Requests
                #                                # create data packet & return it
                #                                data_packet_header=mkHeader(P_data,0,0,data_packet_payload.length,getName(request_label), NA,0,request_label)
                #                                data_packet=mkPacket(data_packet_header,data_packet_payload)
                #                                # now push it out
                #                                if getTo(data_packet_header) != @service
                #                                    @tx_fifo.push(data_packet)
                #                                else
                #                                    @data_fifo.push(data_packet)
                #                                end
                #                            end
                #                        end
                #                    end
                #                else # DS_present; but it might be streaming data
                #                    # if present and not streaming, ignore data
            end # if status !=1
                       
        end # of while        
    end # of store_data
    # -----------------------------------------------------------------------------
    # Dispatch data packets in answer to a request packet
    # WV: This is only used by ASSIGN and similar services. Should be # ifdef'ed!
    # Even better: we don't need this, the ASSIGN core should handle it
    # It means the core must know how to wait for packets to arrive without blocking
    # Anyway, even if the ServiceManager handles this, we should use a lookup
    # mechanism instead of the "unshift" approach
    # We'll do this:
    # ASSIGN still uses READ;
    # CACHE, BUFFER and ACC can each support 8 requests per register
    # (See notes for the new register approach; we 'll have 8 registers as well)
    # So what we need is: Fifo<Word,8> request_table[8]
    # request_table[reg].push(request_packet)
    def dispatch_data_packets()  # In answer to a request packet
        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} dispatch_data_packets()"
        end
        #ev
        pending_requests_fifo=SBA_Packet_Fifo.new
        #        pending_requests_fifo=[] #C++ Packet_Fifo pending_requests_fifo;
        while @request_fifo.length>0
            # get Request packet and look at requested data
            request=@request_fifo.shift #t Packet_t
            puts ppPacket(request) if @v #skip
            packet_label= getReturn_as(getHeader(request)) #t Word
            var_label=getPayload(request)[0] #t Word
            # tuple support
            offset=0 #t uint
            fsize=0 #t uint
            if getExt(var_label)==1
                var_label_ext=getPayload(request)[1] #t Word
                offset=getOffset(var_label_ext) 
                fsize=getSize(var_label_ext)
            end
            
            data_address=0 #t MemAddress

            has_label=0 #t bool
            data_status=DS_absent #t DS_t
            #C++ uint mode;
=begin
# Now for the new approach, assuming we have NREGS registers which allow NREQS calls.
# data_address will actually be the address of the register; the symbol will also encode
# some extra info, e.g. is this a STREAM or a GET or an ACC or a CACHE request.
# For now we assume it's a CACHE request; I think an ACC is identical but the store is different
# The register stores the following: address of the data, status bits, info indicating type if necessary
so we have: 
=end
            if getKind(var_label)==K_D
                reg_address=getReg(var_label) #t uint
                mode=getMode(var_label)
                data_address=reg_address
                #iv
                if @debug_all or @service==@debug_service
                    puts "dispatch_data_packets(): Mode: CACHE: reg address: #{reg_address} mode #{mode}"
                end
                #ev
#                if @register_set[reg_address].status==RDS_present # can be RDS_absent or RDS_present, only used for streaming
                if getStatus(@symbol_table[reg_address])==DS_present
                    has_label=1                    
                    #iv
                    if @debug_all or @service==@debug_service
                        print "dispatch_data_packets(): Mode: CACHE: data address #{data_address}"
                        puts " status #{data_status}" #C++ cout << " status "<<(int)data_status << endl;                        
                    end
                    #ev
                else
                    #                    data_status=getStatus(@symbol_table[reg_address])
                    @request_table.push(reg_address,packet_label)
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "dispatch_data_packets(): Mode: CACHE: queue request for reg #{reg_address} (#{@register_set[reg_address].data_address})"
                    end
                    #ev
                end
                data_status=getStatus(@symbol_table[reg_address])
                # If mode==M_stream, here's where we restart the task
                # WV25112008: I'm unsure if we require data_status==DS_present
                if mode==M_stream                
                    if @register_set[reg_address].status==RDS_present
                        restart_subtask(reg_address)
                        @register_set[reg_address].status=RDS_absent
                    end
                end

            elsif getKind(var_label)==K_L
                # For a K_L variable, the address is stored in a lookup table
                # We store K_L symbols keyed by their Name
                if @sba_tile.lookup_table.count(getName(var_label))==1
                    has_label=1
                    word=@sba_tile.lookup_table.read(getName(var_label)) #t Word
                    data_address= getSubtask(word)
                    data_status=getStatus(word) #C++ data_status=(Data_Status)getStatus(word);
                end
            else
                # I think this never happens
                raise "WHY #{getKind(var_label)}?"
                data_address=getSubtask(var_label)
            end
            
            print  "PRESENT?",data_address,":",(has_label==1)," and ",(data_status==DS_present),"\n" if @v #skip
#            puts "dispatch_data_packet(): has_Label=#{has_label}"
#            puts "dispatch_data_packet(): data_status=#{data_status}" #C++ cout << "dispatch_data_packet(): data_status="<<(int)data_status<<"\n";
            if has_label==1 # and data_status==DS_present
                # Data is present
                #WV21042009: (r2526) removed all references to STRUCTURED_STORAGE                 
                # packetise
                payload_length=0 #t Length_t
                if getReturn_to(getHeader(request))!=getName(packet_label)
                    raise "#{getReturn_to(getHeader(request))}!=#{getName(packet_label)}" #skip
                end
                #C++ Word_List tdata;
                puts "dispatch_data_packet(): data_address=#{data_address}" #skip
                tdata=getField(@sba_tile.data_store.mget(data_address),offset,fsize)
                payload_length=tdata.length
                puts "dispatch_data_packet(): payload_length=#{payload_length}" #skip
                packet_header=mkHeader(P_data,0,0,payload_length, getReturn_to(getHeader(request)), NA,0,packet_label)
                packet_payload=tdata #t Word_List
                packet=mkPacket(packet_header,packet_payload)
#iv
                puts ppPacket(packet)
#ev
                # A shortcut: we don't want to go via the network unless we have to
                # This should actually almost never happen
                if getTo(packet_header) != @service
                    @sba_tile.transceiver.tx_fifo.push(packet)
                else
                    @data_fifo.push(packet)
                end

            else
#if SYSC_FIXME==0                
                # Most likely it's not yet there.
                if getKind(var_label)==K_L
                    pending_requests_fifo.push(request)
                end
#endif // SYSC_FIXME                
            end # of request there or not
        end # of while
#if SYSC_FIXME==0
        while pending_requests_fifo.length()>0
            pending_request =  pending_requests_fifo.shift #t Packet_t
            @request_fifo.unshift(pending_request)
        end
#endif // SYSC_FIXME       
        #        pending_requests_fifo.clear()
    end # of dispatch_data_packets

    # -----------------------------------------------------------------------------
    # Parse subtask packets.
    # This is the key subroutine for parsing the SBA. It implements the actual parsing rules and actions. It is called from
    # parse_subtask. It's factored out purely for ease of maintenance.

    def parse_subtask()
        #tile
        #WV23072008: replace by single call to length + for loop and maybe unguarded shift
        while @subtask_fifo.length>0
            subtask_word=@subtask_fifo.shift #C++ Word subtask_word=(Word)subtask_fifo.front();subtask_fifo.pop_front();
            if VM==1
                parent_subtask=(subtask_word & 0xFFFF0000)>>16 #t Subtask
                code_address= subtask_word & 0x0000FFFF #t MemAddress
            else # VM==0
                parent_subtask=subtask_word #C++ Subtask parent_subtask=(Subtask)(subtask_word & 0x0000FFFF);
                code_address=subtask_word #C++ MemAddress code_address=(MemAddress)(subtask_word & 0x0000FFFF);
            end # VM

            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} parse_subtask(#{parent_subtask}): parsing #{parent_subtask} (#{code_address})"
            end
            #ev
            # Note the .dup: without it, the .shift empties the code memory!
            subtask=@sba_tile.code_store.mget(code_address).dup #t Word_List
            puts  "#{code_address}: #{subtask.inspect}" if @v #skip
            # remove first elt, i.e. the actual service
            service_symbol=subtask.shift #C++ Word service_symbol=subtask.front(); subtask.pop_front();
            if getExt(service_symbol)==1
                service_symbol_ext=subtask.shift #C++ Word service_symbol_ext=subtask.front(); subtask.pop_front();
                offset=getOffset(service_symbol_ext) #t uint
                fsize=getSize(service_symbol_ext) #t uint
                @subtask_list.offset(parent_subtask,offset)
                @subtask_list.fsize(parent_subtask,fsize)              
            end
            # In case of aliases, we must consider the first argument. So in that case we should store it.
            @subtask_list.called_as(parent_subtask,service_symbol)
            #        @subtask_list.nargs(parent_subtask,subtask.length) # WV: This is of course a HACK, should be taken from Subtask field of service_symbol
            @subtask_list.nargs(parent_subtask,getNArgs(service_symbol))
            nargs_absent=getNArgs(service_symbol) #t uint   
            @subtask_list.nargs_absent(parent_subtask,nargs_absent)

            #WV22062008 Logic for ACC & BUFFER
            mode=getMode(service_symbol) #t uint
            puts "MODE: #{mode}" if @v #skip
            #C++ uint reg_addr=0;
            if mode!=M_normal # ACC,BUFFER=1
                reg_addr=getReg(service_symbol)
                # we need two bits in Subtask_List for this. Use mode, but 1 extra field
                @subtask_list.mode(parent_subtask,mode)
                @subtask_list.reg(parent_subtask,reg_addr)
                # now allocate memory for the result -- unless it was already allocated
                if  @register_set[reg_addr].status==RDS_absent
                    # WV25112008: I've decided that the registered addresses are the lower addresses of the memory. So the data memory offset is now DATA_OF+NREGS
                    # and the data memory size DATA_SZ-NREGS
                    # Note that code_address is only used for STREAM, but writing doesn't cost anything
                    # We could get code_address from the Subtask_list at the cost of a few cycles
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "parse_subtask(): Mode #{mode}: reg #{reg_addr}"
                    end
                    #ev
                    @register_set[reg_addr]=SBA_RegisterEntry.new(reg_addr,code_address,parent_subtask,RDS_absent) #skip
                    #C++ register_set[reg_addr].data_address=reg_addr;register_set[reg_addr].code_address=code_address;register_set[reg_addr].subtask_address=parent_subtask;
                end
            end # if mode!=0
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} parse_subtask(#{parent_subtask}): NARGS:#{@subtask_list.nargs(parent_subtask)}"
                puts "#{@service} parse_subtask(#{parent_subtask}): Subtask length: #{subtask.length}"
                puts "#{@service} parse_subtask(#{parent_subtask}): stack size #{@data_address_stack.size()}"
            end
            #ev           

            #WV23072008: replace by for ?
            i=0 #t uint
            while subtask.length>0
                elt=subtask.shift #C++ Word elt=subtask.front(); subtask.pop_front();
                if @data_address_stack.size==0
                    raise "#{@service} parse_subtask(#{parent_subtask}): ADDRESS STACK (#{DATA_SZ}) OVERFLOW for subtask #{parent_subtask}"
                end
                #C++ MemAddress data_address;
                if getKind(elt)!=K_C
                    data_address=@data_address_stack.pop
                else
                    data_address= getReg(elt)
                    # tuple support
                    if getExt(elt)==1
                        elt_ext = subtask.shift #C++ Word elt_ext=subtask.front(); subtask.pop_front();
                        offset=getOffset(elt_ext) #t uint
                        fsize=getSize(elt_ext) #t uint
                        @register_set[data_address].offset=offset
                        @register_set[data_address].fsize=fsize                        
                    end
                end
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} parse_subtask(#{parent_subtask}): data address #{data_address} for #{ppSymbol(elt)}"
                    puts "#{@service} parse_subtask(#{parent_subtask}): stack size/pointer #{@data_address_stack.size()}"
                end
                #ev
                arg_symbol=elt #t Word
                arg_symbol=setSubtask(arg_symbol,parent_subtask)
                arg_symbol=setStatus(arg_symbol,DS_absent)
                @symbol_table[data_address]=arg_symbol
                #ifndef STATIC_ALLOC
                @subtask_list.arguments(parent_subtask).push(data_address) #s/push/push_back/
                #else
                #C++ subtask_list.arguments(parent_subtask)[i]=data_address;
                #endif
                i=i+1
                # -------------------- Identify Symbol by Kind and Quoted -----------------
                # R|C|D|U|L
                # Actually, only R really matters.
                #            getKind(elt) = getKind(elt) #t Kind_t
                if  getQuoted(elt)==0
                    # if the packet is not in store, create a request packet and put it in the TX queue
                    #iv
                    if @debug_all or @service==@debug_service
                        if not ((getKind(elt)==K_R or getKind(elt)==K_C) or (getKind(elt) == K_D or getKind(elt) == K_L) )
                            raise "Should be obsolete: #{ppSymbol(elt)}" #C++ std::cerr << "Should be obsolete: " << ppSymbol(elt) << "\n";exit(0);
                        end
                    end
                    #ev
                    # Creating a new, unique var name
                    var_label = elt #t Word
                    var_label = setName(var_label,@service)
                    var_label = setSubtask(var_label,data_address)
                    arg_symbol=setStatus(arg_symbol,DS_requested)
                    @symbol_table[data_address]=arg_symbol
                    store=getName(elt) #t uint
                    packet_type=P_reference #t Packet_type_t

                    if getKind(elt)== K_L  or getKind(elt)== K_U # not sure if K_U ever occurs
                        store=S_LET # getKind(elt) # K_L is only used for LET, so K_L==S_LET
                        packet_type=P_request
                        #                    var_label = setSubtask(var_label,data_address)
                    elsif getKind(elt)== K_D
                        #                    reg_addr=getReg(elt) #t uint
                        #                    if getKind(elt)== K_D
                        packet_type=P_request
                        #                        mode=getMode(elt) #t uint
                        #                        reg_mode_data_address=(reg_addr << FS_Reg)+(mode << FS_Mode)+data_address #t Word
                        #                        var_label = setSubtask(var_label,reg_mode_data_address) # WV: Mode/Reg info. Required?
                        #                        var_label = setSubtask(var_label,data_address)
                        #                        var_label = setKind(var_label,K_R)
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "parse_subtask(): Mode: K_D REQ: subtask #{getSubtask(elt)}"
                            puts "parse_subtask(): Mode: K_D REQ (#{getMode(elt)}) from reg #{reg_addr}"
                        end
                        #ev

                        #                   else
                        #                        var_label = setSubtask(var_label,data_address) # WV: for K_R. Ugly
                        #                   end
                    end # Kind selection

                    payload_length=1 #t Length_t
                    prio=0 #t Prio_t
                    redir=0 #t Redir_t
                    ack_to=0 #t Word
                    requestpacket_header= mkHeader(packet_type,prio,redir,payload_length,store,@service,ack_to,var_label)
                    requestpacket_payload=[elt] #C++ Word_List requestpacket_payload; requestpacket_payload.push_back(elt);
                    request_packet=mkPacket(requestpacket_header,requestpacket_payload)
                    # als VM==1
                    if store != @service
                        #WV23072008: push directly onto request fifo at destination!
                        @sba_tile.transceiver.tx_fifo.push(request_packet)
                        #iv
                        #                puts "#{@service} parse_subtask(#{parent_subtask}): Sending packet via NoC: #{store}<>#{@service}"
                        #ev
                    else
                        #iv
                        #                puts "#{@service} parse_subtask(#{parent_subtask}): Bypassing NoC: #{store}<>#{@service}"
                        #ev
                        # WV: This should be smarter: there is no need to wrap the request in a 4-word packet for local requests.
                        # Instead, all we need is the address allocated for the results and the address for the code
                        # So we can join these in a single word and push that word onto a separate fifo and set a flag somewhere ("direct_access")
                        # This fifo will be read by activate_subtask based on that flag.
                        # So: direct_access_fifo.push(getSubtask(elt) << 16 + data_address)
                        # ( we could of course push [getSubtask(elt),data_address] in Ruby but in C++ this is almost certainly slower)
                        demux_packets_by_type(request_packet)
                    end
                    # anders # VM==0
                    #WV26112008 check with Waqar: I think the SystemC model should also be able to bypass the TX fifo
                    #                @tx_fifo.push(request_packet)
                    # einde # VM


                elsif getQuoted(elt)==1
                    # unquote
                    #                elt=setQuoted(elt,0) # Essential, and not nice at all!
                    #                var_label=setQuoted(var_label,0)
                    # now save unquoted symbol
                    #                var_label=setKind(var_label,K_Q)

                    #C++ Word_List elt_val;
                    elt_val=[elt] #C++ elt_val.push_back(elt);
                    if (getKind(elt) == K_B or getKind(elt) == K_Q) and getExt(elt)==1
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "Quoted Extended Builtin"
                        end
                        #ev
                        for i in 1..getSubtask(elt) #C++ for (int i=0;i<getSubtask(elt);i++) {
                            elt_sym=subtask.shift #C++ Word elt_sym=subtask.front(); subtask.pop_front();
                            elt_val.push(elt_sym) #s/push/push_back/
                        end
                    else
                        #WV20022008:                     elt=setQuoted(elt,0) # Essential, and not nice at all!
                        # I don't see why unquoting is essential, let the service cores deal with it
                        #                    elt_val=[elt] #C++ elt_val.push_back(elt);
                    end
#iv
                    puts "#{@service} parse_subtask(#{parent_subtask}): storing #{ppPayload(elt_val)} in address #{data_address}"
#ev                                        
                    @sba_tile.data_store.mput(data_address,elt_val)
                    @symbol_table[data_address]=setStatus(@symbol_table[data_address],DS_present)
                    @subtask_list.decr_nargs_absent(parent_subtask)
                    nargs_absent=nargs_absent-1
                end # of quoted or not
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} parse_subtask(#{parent_subtask}): subtask length is #{subtask.length}"
                end
                #ev
            end # while def-subtask is not fully parsed

            # Now we can update the subtask status. You never know if all packets are already there!
#sysc            subtask_list.lock();
            if  @subtask_list.status(parent_subtask)==STS_new and @subtask_list.nargs_absent(parent_subtask)==0
 
                    # WV 15042009: in the SystemC model nargs_absent can be updated in store_data() leading to
                # the subtask being pushed twice. So we use an internal counter instead of the subtask list field as condition
#WV20052009 The real issue is this: between parse_subtask() reading the status and setting it to STS_pending,
# it is possible for store_data() to read the status and still find STS_new() and so proceed to push the subtask
# In the other direction, beween the time store_data() reads the status and sets it to STS_pending, 
# parse_subtask() can read it and find STS_new(). So we need a locking mechanism here. What we need is that 
# between reading and writing, the process keeps the entry locked.   
                
                                                
#                if  @subtask_list.nargs_absent(parent_subtask)==0
#                if nargs_absent==0                    
                    @subtask_list.status(parent_subtask,STS_pending)
                    @pending_subtasks_fifo.push(parent_subtask)                    
#                end
            end
#sysc               subtask_list.unlock();            
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} parse_subtask(#{parent_subtask}): end parsing #{parent_subtask}" #sysc
            end
            #ev
        end # of while subtask fifo not empty
    end # of parse_subtask

    # -----------------------------------------------------------------------------
    #if MULTI_THREADED_CORE==0

    # WV19062008
    # I want MemAddresses to be an Array
    # So I can simply assign addresses=@subtask_list.arguments(@current_subtask) #t const MemAddresses&
    #
    # helper function for core_control()
    #ifndef STATIC_ALLOC
    def build_value_address_list() #t MemAddresses # sba_tile
        # tile
        addresses=[] #t MemAddresses #s/=..//
        nargs=@subtask_list.nargs(@current_subtask) #t uint
        #iv
                puts "#{@service} build_value_address_list(): #{nargs}<>#{@subtask_list.arguments(@current_subtask).size()}"
        #ev                

        if nargs>0
            args=@subtask_list.arguments(@current_subtask) #t Subtask_Argument_List&
            for address in args #t MemAddresses                
                addresses.push(address) #s/push/push_back/
                nargs=nargs-1
                if nargs==0
                    break
                end
            end
        end
        return addresses
    end # of build_value_address_list
    #endif

    # -----------------------------------------------------------------------------
    def send_ack()
        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} ACK: send_ack()"
        end
        #ev
        to=getName( @subtask_list.ack_to(@current_subtask) ) #t Service
        return_to=@service #t Service
        return_as=@subtask_list.ack_to(@current_subtask) #t Word
        # Create the result packet
        payload_length=1 #t Length_t
        prio=0 #t Prio_t
        # We don't want to redir the ACK!
        redir=0 #t Redir_t
        ack_to=@subtask_list.ack_to(@current_subtask) #t Word
        #        puts "#{@service} ACK: send_ack() to #{to} as #{ppSymbol(ack_to)}, return_as #{ppSymbol(return_as)}" #skip
        packet_header=mkHeader(P_data,prio,redir,payload_length,to,return_to,ack_to,return_as)
        packet_payload=[return_as] #C++ Word_List packet_payload; packet_payload.push_back(return_as);
        packet=mkPacket(packet_header,packet_payload)
        # Put on the TX fifo
        # als VM==1
        if to != @service
            #iv
            puts "#{@service} send_ack(): Sending packet via NoC: #{to}<>#{@service}"
            #ev
            @sba_tile.transceiver.tx_fifo.push(packet)
        else
            demux_packets_by_type(packet)
        end
        # anders # VM==0
        #                        @tx_fifo.push(packet)
        # einde # VM
        puts ppPacket(packet)  if @v #skip
        puts getSubtask(getReturn_as(getHeader(packet)))  if @v #skip
    end # of send_ack


    def restart_subtask(regaddr) #t void (MemAddress)
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} restart_subtask(#{regaddr})"
        end
        #ev
        subtask_address=@register_set[regaddr].subtask_address #t MemAddress
        # don't forget to reset the Subtask Status
        #WV25112008: will this work for VM? Will the subtask not get reclaimed?
        # We must stop streaming subtasks from being cleaned up, i.e. S-buf tasks should not be cleaned up
        # unless of course we call activate_subtask_helper(), but that would be slow
        # This means we do need to distinguish between BUF and VAR/ACC/CACHE
        @subtask_list.status(subtask_address,STS_new)
        @subtask_list.arguments(subtask_address,[]) #skip

        if VM==1
            # get the code address and the subtask address
            # combine into a single word
            code_address= @register_set[regaddr].code_address #t MemAddress
            subtask_word=(code_address & 0x0000FFFF) + ( (subtask_address << 16) & 0xFFFF0000) #t Word
            # push onto subtask fifo
#iv            
            puts "restart_subtask VM=1: pushing #{subtask_address}"
#ev            
            @subtask_fifo.push(subtask_word)
        else # VM==0
            # push onto subtask fifo
#iv                    
            puts "restart_subtask VM=0: pushing #{subtask_address}"
#ev            
            @subtask_fifo.push(subtask_address)
        end # VM
    end # of restart_subtask
    # -----------------------------------------------------------------------------
    # refactoring of ServiceCore run() and dispatch_result_packets()


    def core_control()
        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} core_control(): core_status = #{@core_status}"
        end
        #ev
#sysc while (true) {        
        redir=0 #t uint
        # Core is idle, there are pending tasks => take the first pending task;
        # set core status to ready
        if @core_status == CS_idle and @pending_subtasks_fifo.length>0 #skipsysc
#sysc        if (core_status == CS_idle) {
            #iv
            puts "#{@service} core_control(): set core_status to CS_Ready"
            #ev
            @current_subtask=@pending_subtasks_fifo.shift
            if @subtask_list.status(@current_subtask)==STS_pending
#iv
            puts "#{@service} core_control(): set to status for subtask #{current_subtask} to STS_processing"
#ev				
                @subtask_list.status(@current_subtask,STS_processing)
            end
            @core_status = CS_ready
        
        
        else  # if (core_status != CS_idle)
       #iv
            #sysc OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " 
            #sysc <<name()<<" core_control(): Waiting for ServiceCore (core_status=="<<core_status<<")\n";
       #ev
            #sysc if (core_status != CS_done) { 
            #sysc      wait(core_status.value_changed_event());
            #sysc  }
       #iv
            #sysc OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            #sysc <<name()<<" core_control(): done waiting (core_status=="<<core_status<<")\n";
       #ev
       end  # CS_idle      
        
        
        # Core is ready => get addresses, clear results store,
        # set core status to busy
        if @core_status == CS_ready
            @n_args=@subtask_list.nargs(@current_subtask)
            #ifndef STATIC_ALLOC
            @arg_addresses=build_value_address_list()
            #else
            #C++ arg_addresses=subtask_list.arguments(current_subtask);
            #C++ arg_addresses.size(n_args);
            #endif
			called_as=subtask_list.called_as(current_subtask) #t Symbol_t
            @opcode=getName(called_as) & F_Opcode
            @scid=(getName(called_as) & F_SCId) >> FS_SCId
            #iv
            if @v #skip
                puts "#{@service} CORE CONTROL: #{@arg_addresses.inspect}" #skip
            else #skip
                puts "#{@service} CORE CONTROL: #{@arg_addresses[0]}"
            end #skip
            #ev
            @results_store=[] #C++ results_store.clear();
            @core_status=CS_busy
        end   # CS_ready
        # Core is busy => ServiceCore will set the core status to CS_done before handing control to the core.
        # However, the the core can set the status to "busy" or "managed"
        # Note that the ServiceManager takes no action at all while the core status is CS_busy

        # if core status is done => create TX packet & dispatch
        if @core_status == CS_done
#iv
            puts "#{@service} core_control(): ServiceCore is done!"
#ev
 			
            if @subtask_list.status(@current_subtask)==STS_processing
                @subtask_list.status(@current_subtask,STS_processed)
            end
            # get the results from the results store and push them out on the network
            to=@subtask_list.to(@current_subtask) #t Service
            return_to=@subtask_list.return_to(@current_subtask) #t Service
            return_as=@subtask_list.return_as(@current_subtask) #t Word
            # Create the result packet
            #
            prio=0 #t uint
            redir=@subtask_list.redir(@current_subtask)
#            return_to=@subtask_list.return_to(@current_subtask)
            ack_to=@subtask_list.ack_to(@current_subtask) #t Word
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} core_control(): REDIR: #{redir}; ACK_TO: #{ack_to}"
                puts "#{@service} core_control(): RETURN_TO: #{return_to} (#{@current_subtask})"
                puts "#{@service} core_control(): RETURN_AS: #{return_as} (#{@current_subtask})"
            end
            #ev
            #WV22062008 Here we need to check the mode to determine on the action
            # The Subtask_List must store the register address!
            #C++ Length_t payload_length;
            #C++ Word_List packet_payload;
            mode=@subtask_list.mode(@current_subtask) #t uint
            offset=@subtask_list.offset(@current_subtask) #t uint
            fsize=@subtask_list.fsize(@current_subtask) #t uint
            if mode==M_normal
                results=@results_store #C++ Word_List results = (Word_List)results_store;
                puts results.inspect #skip
                packet_payload=getField(results,offset,fsize)
                payload_length=packet_payload.length
            else
                # mode!=M_normal, so we cache the result
                reg_addr = @subtask_list.reg(@current_subtask) #t uint
                reg_symbol=mkSymbol(K_D,0,0,0,0,((mode << FS_Mode)+(reg_addr << FS_Reg)),@service)

                #iv
                if @debug_all or @service==@debug_service
                    puts "core_control(): Mode #{mode}: buffering "
                    if (getKind(@results_store[0])==K_B)
                        puts "\tK_B: #{@results_store[1]}"
                    else
                        puts ppPayload(@results_store)                       
                    end
                    puts " in reg #{reg_addr}" 
                end
                #ev
                #WV26112008: At first I thought we should return the register as a symbol
                # but that is not possible as the data store address is in use for the result
                # so we should simply return an empty packet; store_data will set the status to Present
                #                packet_payload=[reg_symbol] #C++ packet_payload.push_back(reg_symbol);
                #                payload_length=1
                packet_payload=[] #skip
                payload_length=0
                #WV22062008 now get the address and store the result
                #                res_addr=@register_set[reg_addr].data_address #t MemAddress
                #                @sba_tile.data_store.mput(res_addr,@results_store)
                #WV22042009 for tuple support we need to be able to specify 
                # on the one hand the offset and fsize of the field in the results_store
                # this should be stored in the Subtask_List, it's actually part of the packet header
                # By default it's offset=0,fsize=0 (means everything!)
                # on the other hand the offset (and fsize) of the field in the data_store, if we buffer the result
                
                #  Word_List results(results_store.read_all());
                results=@results_store #C++ Word_List results=(Word_List)results_store;
                @sba_tile.data_store.mput(reg_addr,getField(results,offset,fsize))
                @register_set[reg_addr].status=RDS_present
                reg_status_symbol=setStatus(reg_symbol,DS_present) #t Word
                @symbol_table[reg_addr]=reg_status_symbol
                puts "REG: #{@results_store.inspect}"      #skip
                # Now check if there are pending requests
                #iv
                if @debug_all or @service==@debug_service
                    puts "core_control(): request table for #{reg_addr} contains #{@request_table.table[reg_addr].inspect}" #skip
                end
                #ev      
                if @request_table.size(reg_addr)>0 
                    # there are pending requests, handle them
                    # i.e. dispatch the data packets
                    # FIXME: we should factor this out as a function                                    
                    packet_label=@request_table.shift(reg_addr) #t Word
                    # packetise
                    data_address=@register_set[reg_addr].data_address #t MemAddress
                    tdata=@sba_tile.data_store.mget(data_address) #t Word_List
                    payload_length=tdata.length #t Length_t
                    packet_header=mkHeader(P_data,0,0,payload_length, getName(packet_label), NA,0,packet_label)
                    packet=mkPacket(packet_header,tdata)
                    # A shortcut: we don't want to go via the network unless we have to
                    # This should actually almost never happen
                    if getTo(packet_header) != @service
                        @sba_tile.transceiver.tx_fifo.push(packet)
                    else
                        @data_fifo.push(packet)
                    end
                end
            end # mode!=0
            packet_header=mkHeader(@core_return_type,prio,redir,payload_length,to,return_to,ack_to,return_as)
            packet=mkPacket(packet_header,packet_payload)
            # Put on the TX fifo
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} core_control(): PACKET:\n #{ppPacket(packet)}"
            end
            #ev
            if to != @service
                @sba_tile.transceiver.tx_fifo.push(packet)
            else
                demux_packets_by_type(packet)
            end
        end # of CS_done

        # Core is done or being "managed", i.e. not quite done but not blocking either
        # Typically for LET with quoted args etc.
        # This allows the core to send packets that are different from the default result packet
        # or send nothing at all
        if @core_status == CS_done or @core_status == CS_managed
            redir=@subtask_list.redir(@current_subtask)
if VM==1             
            puts "#{@service} core status: #{@core_status} ACK? ack_ok #{@ack_ok}; redir: #{redir} => #{@ack_ok*redir}" if @v #skip
end # VM           
            # Now let's check if we need to send an ACK
            #            ack_to_name=getName( @subtask_list.ack_to(@current_subtask) ) #t Service

            if redir==1  # ack_to_name!=to
                if VM==1
                    if @ack_ok==1
                        send_ack()
                    end
                else # VM==0
                    send_ack()
                end # VM
            end # of redir

            # We clean up now, after the dispatch
            # the core can avoid cleanup by setting the subtask status to STS_blocked
                # in case of ASSIGN, the subtask is removed but the memory is not cleaned up (obviously) by setting STS_cleanup from the core
                # in case of recursion we want the opposite: clean up memory but don't remove the subtask
            
#            mode=@subtask_list.mode(@current_subtask) #t uint
            sts=@subtask_list.status(@current_subtask) #t Subtask_Status
#iv
            puts "#{@service} CLEAN-UP #{@current_subtask}? STS=#{sts}<>3,4,6"
#ev             
            puts "#{@service} CLEAN-UP #{@current_subtask}? #{sts}==#{STS_processed} or #{sts}==#{STS_cleanup}" if @v #skip
            if (sts==STS_processed or sts==STS_cleanup or sts==STS_inactive) # and mode!=M_stream # 3=Done/Processed; 4=CleanedUp; 5=Blocking
                if sts==STS_processed or sts==STS_inactive
                    #iv                
                                        puts "#{@service} CLEAN-UP: clean_up() #{@current_subtask}"
                    #ev                       
                    clean_up()
                    if sts==STS_processed
                        @subtask_list.status(@current_subtask,STS_cleanup) # Cleaned-up
                        sts=STS_cleanup
                    end
                end
                if sts==STS_processed or sts==STS_cleanup
                    # everything is de-allocated, now remove the subtask
#iv                
                    puts "#{@service} CLEAN-UP: remove #{@current_subtask}"
#ev                                 
                    @subtask_list.remove(@current_subtask) # this sets the status to STS_deleted
                              
                
if VM==1
                    # push the address back onto the subtask address stack:
                    @subtasks_address_stack.push(@current_subtask)
else # VM==0
                    # nothing happens as addressing is static
end # VM
                    @current_subtask=0
                end
            elsif sts==STS_blocked # subtask status is 5, reset to 0! (surely it can't be 1 or 2)
#iv                
                puts "#{@service} CLEAN-UP: reset #{@current_subtask} status to STS_new"
#ev                
                @subtask_list.status(@current_subtask,STS_new)
            else
#iv                
                puts "#{@service} NO CLEAN-UP #{@current_subtask}"
#                if (@subtask_list.status(@current_subtask)==STS_processed or @subtask_list.status(@current_subtask)==STS_cleanup) and mode==M_stream
#                    puts "#{@service} status #{@current_subtask} = #{@subtask_list.status(@current_subtask)}"
#                    puts "#{@service} CLEAN-UP #{@current_subtask} for BUF"
#                    # We want the subtask entry to persist but the addresses for data should have been reclaimed.
                if @core_status == CS_managed
                    clean_up()
                end
#                    
#                end
#ev                
            end

            puts "STS for <#{@current_subtask}>: #{@subtask_list.status(@current_subtask)}" unless (@current_subtask==0 or not @v) #skip
            # Clear the results store
            @results_store=[] #s/=../.clear()/
            @core_status= CS_idle
        end # of CS_done || CS_managed
#sysc  } // end of while(true)        
    end # of core_control()
    # -----------------------------------------------------------------------------

    #else

    def multi_threaded_core_control()
        #tile
        #WV02092008: the simple but inefficient way first: loop over threads
        for tid in 0..NCORE_THREADS-1 #t uint
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} multi_threaded_core_control(): core_status[#{tid}] = #{@core_status[tid]}"
            end
            #ev

            redir=0 #t uint
            # Core is idle, there are pending tasks => take the first pending task;
            # set core status to ready
            if @core_status[tid] == CS_idle and @pending_subtasks_fifo.length>0
                #iv
                puts "#{@service} multi_threaded_core_control() thread #{tid}: set to CS_Ready"
                #ev

                @current_subtask[tid]=@pending_subtasks_fifo.shift
                if @subtask_list.status(@current_subtask[tid])==STS_pending
                    @subtask_list.status(@current_subtask[tid],STS_processing)
                end
                @core_status[tid] = CS_ready
            end  # CS_idle
            # Core is ready => get addresses, clear results store,
            # set core status to busy
            if @core_status[tid] == CS_ready
                @n_args[tid]=@subtask_list.nargs(@current_subtask[tid])
                #ifndef STATIC_ALLOC
                @arg_addresses[tid]=multi_threaded_build_value_address_list(tid)
                #else
                #C++ arg_addresses[tid]=subtask_list.arguments(current_subtask[tid]);
                #C++ arg_addresses[tid].size(n_args[tid]);
                #endif
                @opcode[tid]=getName(@subtask_list.called_as(@current_subtask[tid])) & F_Opcode
                #            @scid=(getName(@subtask_list.called_as(@current_subtask[tid])) & F_SCId) >> FS_SCId
                #iv
                if @v #skip
                    puts "#{@service} CORE CONTROL thread #{tid}: #{@arg_addresses[tid].inspect}" #skip
                else #skip
                    puts "#{@service} CORE CONTROL thread #{tid}: #{@arg_addresses[tid][0]}"
                end #skip
                #ev
                @results_store[tid]=[] #C++ results_store[tid].clear();
                @core_status[tid]=CS_busy
            end   # CS_ready
            # Core is busy => ServiceCore will set the core status to CS_done before handing control to the core.
            # However, the the core can set the status to "busy" or "managed"
            # Note that the ServiceManager takes no action at all while the core status is CS_busy

            # if core status is done => create TX packet & dispatch
            if @core_status[tid] == CS_done
                if @subtask_list.status(@current_subtask[tid])==STS_processing
                    @subtask_list.status(@current_subtask[tid],STS_processed)
                end
                # get the results from the results store and push them out on the network
                to=@subtask_list.to(@current_subtask[tid]) #t Service
                return_to=@subtask_list.return_to(@current_subtask[tid]) #t Service
                return_as=@subtask_list.return_as(@current_subtask[tid]) #t Word
                # Create the result packet
                #
                prio=0 #t uint
                redir=@subtask_list.redir(@current_subtask[tid])
                return_to=@subtask_list.return_to(@current_subtask[tid])
                ack_to=@subtask_list.ack_to(@current_subtask[tid]) #t Word
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} multi_threaded_core_control() thread #{tid}: REDIR: #{redir}; ACK_TO: #{ack_to}"
                    puts "#{@service} multi_threaded_core_control() thread #{tid}: RETURN_TO: #{return_to} (#{@current_subtask[tid]})"
                    puts "#{@service} multi_threaded_core_control() thread #{tid}: RETURN_AS: #{return_as} (#{@current_subtask[tid]})"
                end
                #ev
                #WV22062008 Here we need to check the mode to determine on the action
                # The Subtask_List must store the register address!
                #C++ Length_t payload_length;
                #C++ Word_List packet_payload;
                mode=@subtask_list.mode(@current_subtask[tid]) #t uint
                if mode==M_normal
                    packet_payload=@results_store[tid]
                    payload_length=packet_payload.length
                else
                    # mode!=M_normal, so we cache the result
                    reg_addr = @subtask_list.reg(@current_subtask[tid]) #t uint
                    reg_symbol=mkSymbol(K_D,0,0,0,0,((mode << FS_Mode)+(reg_addr << FS_Reg)),@service)
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "multi_threaded_core_control(): Mode #{mode}: returning #{ppSymbol(reg_symbol)}"
                    end
                    #ev
#                    packet_payload=[reg_symbol] #C++ packet_payload.push_back(reg_symbol);
#                    payload_length=1
                    packet_payload=[] #skip
                    payload_length=0                    
                    #WV22062008 now get the address and store the result
#                    res_addr=@register_set[reg_addr].data_address #t MemAddress
#                    @sba_tile.data_store.mput(res_addr,@results_store[tid])
                    @sba_tile.data_store.mput(reg_addr,@results_store[tid])                    
                    @register_set[reg_addr].status=RDS_present
                    reg_status_symbol=setStatus(reg_symbol,DS_present) #t Word
                    @symbol_table[reg_addr]=reg_status_symbol                    
                    # Now check if there are pending requests
                    if @request_table.size(reg_addr)>0
                        # there are pending requests, handle them
                        # i.e. dispatch the data packets
                        # FIXME: we should factor this out as a function
                        packet_label=@request_table.shift(reg_addr) #t Word
                        # packetise
                        data_address=@register_set[reg_addr].data_address #t MemAddress
                        tdata=@sba_tile.data_store.mget(data_address) #t Word_List
                        payload_length=tdata.length #t Length_t
                        packet_header=mkHeader(P_data,0,0,payload_length, getName(packet_label), NA,0,packet_label)
                        packet=mkPacket(packet_header,tdata)
                        # A shortcut: we don't want to go via the network unless we have to
                        # This should actually almost never happen
                        if getTo(packet_header) != @service
                            @sba_tile.transceiver.tx_fifo.push(packet)
                        else
                            @data_fifo.push(packet)
                            #@status=true
                        end
                    end
                end # mode!=M_normal
                packet_header=mkHeader(@core_return_type[tid],prio,redir,payload_length,to,return_to,ack_to,return_as)
                packet=mkPacket(packet_header,packet_payload)
                # Put on the TX fifo
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} multi_threaded_core_control(): PACKET:\n #{ppPacket(packet)}"
                end
                #ev
                # als VM==1
                if to != @service
                    @sba_tile.transceiver.tx_fifo.push(packet)
                else
                    demux_packets_by_type(packet)
                end
                # anders # VM==0
                #                        @tx_fifo.push(packet)
                # einde # VM
            end # of CS_done

            # Core is done or being "managed", i.e. not quite done but not blocking either
            # Typically for LET with quoted args etc.
            # This allows the core to send packets that are different from the default result packet
            # or send nothing at all
            if @core_status[tid] == CS_done or @core_status[tid] == CS_managed
                redir=@subtask_list.redir(@current_subtask[tid])
                puts "#{@service} multi_threaded_core_control() core status: #{@core_status[tid]} ACK? ack_ok #{@ack_ok}; redir: #{redir} => #{@ack_ok*redir}" if @v #skip
                # Now let's check if we need to send an ACK
                #            ack_to_name=getName( @subtask_list.ack_to(@current_subtask) ) #t Service

                if redir==1  # ack_to_name!=to
                    if VM==1
                        if @ack_ok[tid]==1
                            multi_threaded_send_ack(tid)
                        end
                    else # VM==0
                        multi_threaded_send_ack(tid)
                    end # VM
                end # of redir

                # We clean up now, after the dispatch
                # core can avoid cleanup by setting the subtask status to STS_blocked
                puts "#{@service} multi_threaded_core_control() CLEAN-UP #{@current_subtask[tid]}? STS=#{@subtask_list.status(@current_subtask[tid])}" if @v #skip
                puts "#{@service} multi_threaded_core_control() CLEAN-UP #{@current_subtask[tid]}? #{@subtask_list.status(@current_subtask[tid])}==#{STS_processed} or #{@subtask_list.status(@current_subtask[tid])}==#{STS_cleanup}" if @v #skip
                mode=@subtask_list.mode(@current_subtask) #t uint                
                if @subtask_list.status(@current_subtask[tid])==STS_processed or @subtask_list.status(@current_subtask[tid])==STS_cleanup and mode!=M_stream # 3=Done/Processed; 4=CleanedUp; 5=Blocking
                    if @subtask_list.status(@current_subtask[tid])==STS_processed
                        multi_threaded_clean_up(tid)
                        @subtask_list.status(@current_subtask[tid],STS_cleanup) # Cleaned-up
                    end
                    # everything is de-allocated, now remove the subtask
                    @subtask_list.remove(@current_subtask[tid])
                    if VM==1
                        # push the address back onto the subtask address stack:
                        @subtasks_address_stack.push(@current_subtask[tid])
                    else # VM==0
                        # nothing happens as addressing is static
                    end # VM
                    @current_subtask[tid]=0
                elsif @subtask_list.status(@current_subtask[tid])==STS_blocked # subtask status is 5, reset to 0! (surely it can't be 1 or 2)
                    @subtask_list.status(@current_subtask[tid],STS_new)
                else
                    puts "#{@service} multi_threaded_core_control() NO CLEAN-UP #{@current_subtask[tid]}" if @v #skip
                end

                puts "#{@service} multi_threaded_core_control() STS for <#{@current_subtask}>: #{@subtask_list.status(@current_subtask)}" unless (@current_subtask[tid]==0 or not @v) #skip
                # Clear the results store
                @results_store[tid]=[] #s/=../.clear()/
                @core_status[tid]= CS_idle
            end # of CS_done || CS_managed
        end # of loop over threads
    end # of multi_threaded_core_control()
    # -----------------------------------------------------------------------------
    # WV19062008
    # I want MemAddresses to be an Array
    # So I can simply assign addresses=@subtask_list.arguments(@current_subtask) #t const MemAddresses&
    #
    # helper function for core_control()

    #ifndef STATIC_ALLOC
    def multi_threaded_build_value_address_list(i) #t MemAddresses (int)
        # tile
        addresses=[] #t MemAddresses #s/=..//
        if @subtask_list.nargs(@current_subtask[i])>0
            args=@subtask_list.arguments(@current_subtask[i]) #t Subtask_Argument_List&
            for address in args #t MemAddresses
                addresses.push(address) #s/push/push_back/
            end
        end
        return addresses
    end # of multi_threaded_build_value_address_list
    #endif

    # -----------------------------------------------------------------------------

    def multi_threaded_send_ack(i) #t void (int)
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} ACK: send_ack()"
        end
        #ev
        to=getName( @subtask_list.ack_to(@current_subtask[i]) ) #t Service
        return_to=@service #t Service
        return_as=@subtask_list.ack_to(@current_subtask[i]) #t Word
        # Create the result packet
        payload_length=1 #t Length_t
        prio=0 #t Prio_t
        # We don't want to redir the ACK!
        redir=0 #t Redir_t
        ack_to=@subtask_list.ack_to(@current_subtask[i]) #t Word
        #        puts "#{@service} ACK: send_ack() to #{to} as #{ppSymbol(ack_to)}, return_as #{ppSymbol(return_as)}" #skip
        packet_header=mkHeader(P_data,prio,redir,payload_length,to,return_to,ack_to,return_as)
        packet_payload=[return_as] #C++ Word_List packet_payload; packet_payload.push_back(return_as);
        packet=mkPacket(packet_header,packet_payload)
        # Put on the TX fifo
        # als VM==1
        if to != @service
            @sba_tile.transceiver.tx_fifo.push(packet)
        else
            demux_packets_by_type(packet)
        end
        # anders # VM==0
        #        @tx_fifo.push(packet)
        # einde # VM
        puts ppPacket(packet) #skip
        puts getSubtask(getReturn_as(getHeader(packet))) #skip
    end # of multi_threaded_send_ack()

    # -----------------------------------------------------------------------------
    # This method does the actual memory management after the core finishes a subtask. It's called from run_service_core.
    # The principle is simple reference counting.
    # This is an essential block as without it memory would never get freed.
    # Still, I feel it's far from being finished. Needs some proper analysis.
    def multi_threaded_clean_up(i) #t void (int)
        #tile
        # Clean up arguments
        args=@subtask_list.arguments(@current_subtask[i]) #t Subtask_Argument_List&
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service}: clean_up:  ADDRESSES: #{args.inspect}" #C++ cout << service<<": clean_up(): ADDRESSES: " << args.size()<<"\n";
        end
        #ev
        for arg_address in args #t Word_List
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service}: clean_up:  ADDRESS: #{arg_address}"
            end
            #ev
            if getStatus(@symbol_table[arg_address])==DS_present
                @symbol_table[arg_address]=setStatus(@symbol_table[arg_address],DS_cleared)
                @data_address_stack.push(arg_address)
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service}: clean_up: REMOVED #{arg_address}"
                end
                #ev
                # NO NEED?                           @symbol_table.delete(arg_address) #s/delete/erase/

                @sba_tile.data_store.remove(arg_address)
            end # if DS_present
        end # of for
    end # of multi_threaded_clean_up

    #endif
    # -----------------------------------------------------------------------------

    def transmit_packets()
        raise "OBSOLETE: write directly to TRX fifo"
        #ifdef CYCLES_INT
        #C++      ticks t1=getticks();
        #endif

        #tile
        #iv
        if @debug_all or @service==@debug_service
            puts "#{@service} transmit_packets()"
        end
        #ev
        while @tx_fifo.length>0
            packet=@tx_fifo.shift #t Packet_t
            @sba_tile.transceiver.tx_fifo.push(packet)
        end
        #ifdef CYCLES_INT
        #C++      ticks t2=getticks();
        #C++      std::cout << "CYCLES_INT[transmit_packets]: "<<service<<": "<<  elapsed(t2,t1) <<"\n";
        #endif

    end # of transmit_packets
    # -----------------------------------------------------------------------------
    # Check if there's a subtask ready to be processed and the core is ready. If so, push pending subtasks on the pending_subtasks list

    #    #WV16082008: Is this not obsolete?
    #
    #    def prepare_subtask
    # #ifdef CYCLES_INT
    # #C++      ticks t1=getticks();
    # #endif
    #        t_subtasks = @subtask_list.subtasks #t Subtasks
    #        for pending_subtask in t_subtasks #t Subtasks
    # #iv
    # if @debug_all or @service==@debug_service
    #        puts "#{@service} prepare_subtask(): status of #{pending_subtask}=#{@subtask_list.status(pending_subtask)}"
    # end
    # #ev
    #            if @subtask_list.status(pending_subtask)==STS_pending # ready for processing
    #                # now check if the task is already there
    #                c=1 #t uint
    #                for item in @pending_subtasks_fifo #t Subtasks
    # #iv
    # if @debug_all or @service==@debug_service
    #                    puts "prepare_subtask(): Subtask in pending_subtasks_fifo: #{item}"
    # end
    # #ev
    #                     c=c*((item!=pending_subtask)?1:0)
    #                end
    #                if c==1
    #                    # if not, add it
    #                    #iv
    #                    if @debug_all or @service==@debug_service
    #                    puts "#{@service} prepare_subtask(): Push #{pending_subtask} onto pending_subtasks_fifo"
    #                    end
    #                    #ev
    #                    @pending_subtasks_fifo.push(pending_subtask) #;
    #                end
    #            end
    #        end
    # #ifdef CYCLES_INT
    # #C++      ticks t2=getticks();
    # #C++      std::cout << "CYCLES_INT[prepare_subtask]: "<<service<<": "<<  elapsed(t2,t1) <<"\n";
    # #endif
    #    end # of prepare_subtask

    # -----------------------------------------------------------------------------
    # This method does the actual memory management after the core finishes a subtask. It's called from run_service_core.
    # The principle is simple reference counting.
    # This is an essential block as without it memory would never get freed.
    # Still, I feel it's far from being finished. Needs some proper analysis.
    #if MULTI_THREADED_CORE==0
    def clean_up()
        #tile
        # Clean up arguments
#        args=@subtask_list.arguments(@current_subtask) #t Subtask_Argument_List&
        nargs=arg_addresses.size() #t uint
        #iv
        if @debug_all or @service==@debug_service
            if @v #skip
                puts "#{@service}: clean_up #{@current_subtask}:  ADDRESSES: #{arg_addresses.inspect},nargs:#{nargs}" #skip
            else #skip
                puts "#{@service}: clean_up #{@current_subtask}:  ADDRESSES: #{nargs}" #sysc
            end #skip
        end
        #ev
        if nargs>0
        for iter_ in 0..nargs-1 #t uint
            arg_address=arg_addresses[iter_] #t MemAddress
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service}: clean_up:  ADDRESS: #{arg_address}"
            end
            #ev
            if getStatus(@symbol_table[arg_address])==DS_present and arg_address>NREGS+DATA_OF
                @symbol_table[arg_address]=setStatus(@symbol_table[arg_address],DS_cleared)
                @data_address_stack.push(arg_address)
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service}: clean_up(): stack size/pointer #{@data_address_stack.size()}"
                    puts "#{@service}: clean_up: REMOVED #{arg_address}"
                end
                #ev
                # NO NEED?                           @symbol_table.delete(arg_address) #s/delete/erase/

                @sba_tile.data_store.remove(arg_address)

            end # if DS_present
        end # of for        
        end
        
    end # of clean_up
    #endif
end # of SBA_ServiceManager class

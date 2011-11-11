#
# :title: Gannet Service-based SoC project - Service Manager class
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

# WV10012011: remove support for multi-threaded core. See -r 4232 or earlier for the code
# WV20110609: for the old configuration behaviour revert to before r4987 and set NEW=0 and NEWER=0

require "SBA/ServiceManagerObjects.rb"

# The Service Manager is the core of the SBA. This is the module that does all
# bookkeeping and subtask "parsing".
# It queues subtasks, and parses the "current" subtask according to very simple
# rules:
#  * Subtask (S|F) => delegate
#  * Data (D|V|A|U|L) => request
#  * Built-in or Quoted (B|Q) => store
# The Quoted kind is essential as it allows to defer evaluation to the cores.

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
    :scid,:sclid #skip
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

    #skipcc

=begin #constructor

    Word_List results_store;
    MemAddresses arg_addresses;
    Subtask current_subtask;
    Service service_id;
    Core_Status core_status;
    uint scid,sclid;
    Packet_Type core_return_type;
    uint opcode;
    uint n_args;
    bool ack_ok;
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
            current_subtask(0),
            service_id(s_),
            core_status(CS_idle),scid(0),sclid(0),core_return_type(P_data),
            status(true)
             {
                for (uint reg=1;reg<NREGS;reg++) {
                    MemAddress reg_addr=reg+DATA_OF;
                    Word reg_symbol=mkSymbol(K_D,0,0,0,0,((M_cache << FS_Mode)+(reg_addr << FS_Reg)),service);
                    symbol_table[reg_addr]=reg_symbol;
                }
            };
=end #constructor

    #endskipcc
    #skip
    # Service Manager constructor
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
        if VM==1
            @ack_ok=1
        end # VM
        @scid=0;
        @sclid=0;
        @current_subtask=0
        @core_status=CS_idle # other states: ready|busy|done|managed
        @core_return_type=P_data # can be subtask, data, maybe even builtin
        @service_core_ram=DATA_SZ+2 # @code_offset+@code_stacksize
        @service_id=service

        # With the new ServiceCore register @state_register, there should be no need for this initial value
        @register_set=[]
        @request_table=SBA_RequestTable.new()
        for reg in 1..NREGS
            @register_set.push(SBA_RegisterEntry.new(0,0,0,0))
            reg_addr=reg+DATA_OF
            reg_symbol=mkSymbol(K_D,0,0,0,0,((M_cache << FS_Mode)+(reg_addr << FS_Reg)),@service)
            @symbol_table[reg_addr]=reg_symbol
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
        if @sba_tile.transceiver.rx_fifo.status()==1
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
        core_control()

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
            #        puts "ServiceManager #{@service_id} #{@status} = p:#{(@pending_subtasks_fifo.length>0)} || CS:#{(@core_status != CS_idle)} || f:#{((@data_fifo.length>0) || (@request_fifo.length>0) || (@subtask_reference_fifo.length>0))}" if @v #skip
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
        #C++      std::cout << "CYCLES[sanity]: "<<service<<": "<< elapsed(t1,t0) <<"\n";
        #C++      std::cout << "CYCLES[tile]: "<<service<<": "<< elapsed(t1_1,t1) <<"\n";
        #C++      std::cout << "CYCLES[receive_packets]: "<<service<<": "<< elapsed(t1_2,t1_1) <<"\n";
        #C++      std::cout << "CYCLES[store_subtask_code]: "<<service<<": "<< elapsed(t1_3,t1_2) <<"\n";
        #C++      std::cout << "CYCLES[activate_subtask]: "<<service<<": "<< elapsed(t1_4,t1_3) <<"\n";
        #C++      std::cout << "CYCLES[store_data]: "<<service<<": "<< elapsed(t1_5,t1_4) <<"\n";
        #C++      std::cout << "CYCLES[parse_subtask]: "<<service<<": "<< elapsed(t1_6,t1_5) <<"\n";
        #C++      std::cout << "CYCLES[core_control]: "<<service<<": "<< elapsed(t1_7,t1_6) <<"\n";
        #C++      std::cout << "CYCLES[prepare_subtask]: "<<service<<": "<< elapsed(t1_8,t1_7) <<"\n";
        #C++      std::cout << "CYCLES[dispatch_data_packets]: "<<service<<": "<< elapsed(t1_9,t1_8) <<"\n";
        #C++      std::cout << "CYCLES[transmit_packets]: "<<service<<": "<< elapsed(t12,t1_9) <<"\n";
        #C++      std::cout << "CYCLES[status]: "<<service<<": "<< elapsed(t22,t12) <<"\n";
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
#        #iv
#        if @debug_all or @service==@debug_service
#            puts "#{@service} receive_packets(): #{@sba_tile.transceiver.rx_fifo.length};"  #sysc
#        end
#        #ev
        while @sba_tile.transceiver.rx_fifo.length>0
            rx_packet= @sba_tile.transceiver.rx_fifo.shift #t Packet_t
            #iv
            if @v #skip
                puts "#{@service} receive_packets(): received packet from TRX"  #sysc
            end #skip
            #ev
            #   puts "PACKET:",rx_packet.inspect,ppPacket(rx_packet) if @v #skip
            #ifdef SC_VERBOSE
            #sysc          int to=(rx_packet.front()>>8)&0xFF;
            #sysc          int return_to=rx_packet.front()&0xFF;
            #sysc          int ctrl=(rx_packet.front()>>26)&0x7;
            #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") To:"<<to<<"; Return-to:"<<return_to<<";Ctrl:"<<ctrl<<endl;
            #endif
            # clear the link
            # check packet type
            demux_packets_by_type(rx_packet)

        end # while there are packets to be received. Alternatively, we could receive one packet per cycle. See which works best?
        #ifdef CYCLES_INT
        #C++      ticks t2=getticks();
        #C++      std::cout << "CYCLES_INT[receive_packets]: "<<service<<": "<< elapsed(t2,t1) <<"\n";
        #endif
    end # of receive_packets

    # ------------------------------------------------------------------------------------
    # Helper for code activation
    def activate_subtask_helper(task_address,tservice_id,packet,is_subtask_packet) #t void (CodeAddress;Name_t;Word_List&;bool)
        #iv
        if @debug_all or @service==@debug_service
        if (@service!=tservice_id)
            puts "#{@service} activate_subtask_helper(): service_id #{tservice_id} <> service #{@service}"
        end
        end
        #ev
        if VM==1
            #iv
            if @debug_all or @service==@debug_service
            puts "#{@service} SUBTASK STACK SIZE: #{@subtasks_address_stack.size}"
            end
            #ev
            if @subtasks_address_stack.size==0
                raise "#{@service} SUBTASK STACK (#{SUBTASKS_SZ}) OVERFLOW"  #C++ std::cerr << service << " SUBTASK STACK ("<< SUBTASKS_SZ <<") OVERFLOW\n"; exit(0);
            end
            subtask_address=@subtasks_address_stack.pop #t CodeAddress
if VERBOSE==1
            puts "#{@service} pop #{subtask_address} off SUBTASK STACK" 
            puts "#{@service} NEW SUBTASK for code #{task_address}: #{subtask_address} #{SUBTASKS_SZ-@subtasks_address_stack.size}" 
            puts "#{@service} SUBTASK requested by \n#{ppPacket(packet)}" 
end # VERBOSE            
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
        if VERBOSE==1        
        puts "#{@service} activate_subtask_helper(): <#{task_address}>"        
        puts ppPacket(packet) 
        end # VERBOSE
#        puts @code_status.inspect if @v #skip
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
        if @debug_all or @service==@debug_service
                puts "DEFER ACTIVATION #{task_address}:#{code_status[task_address]}"
        end
# #sysc
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
#            puts "#{@service_id} <> #{tservice_id}"
            @service_id=tservice_id #skipsysc
        end
    end # of activate_subtask_helper

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
            if is_subtask_packet or (@code_status[code_address]&1)==1
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} store_subtask_code(): ACTIVATE #{code_address} NOW!" #sysc
                end
                #ev
                @code_status[code_address]=@code_status[code_address]&2
                #            if getPacket_type_p(subtask_code_packet)==P_subtask
                # it's a Subtask packet or activation was requested, activate right away
                activate_subtask_helper(code_address,tservice_id,subtask_code_packet,is_subtask_packet)
            end
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} store_subtask_code(): stored #{code_label} at #{code_address}" #sysc
                puts ppPayload(@sba_tile.code_store.mget(code_address)) 
            end
            #ev
        end
        #iv
        print "\n" if @v #skip
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
            if @debug_all or @service==@debug_service
            puts ppPacket(subtask_ref_packet) #skip
            end
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
            if @v #skip
            puts "Service: #{@service} : REF_LABEL: #{ref_label_symbol}"
            puts ppSymbol(ref_label_symbol)
            end #skip
            #ev
            
# #skip
            code_address=getCodeAddress(ref_label_symbol) #t CodeAddress
            tservice_id=getName(ref_label_symbol) #t Name_t
            activate_subtask_helper(code_address,tservice_id,subtask_ref_packet,true)
        end # of while
        #ifdef CYCLES_INT
        #C++      ticks t2=getticks();
        #C++      std::cout << "CYCLES_INT[activate_subtask]: "<<service<<": "<< elapsed(t2,t1) <<"\n";
        #endif
    end # of activate_subtask()

    # ------------------------------------------------------------------------------------
    # store DATA packets on arrival. Only DATA resulting from requests and results form subtasks should ever arrive here.
    # With direct addressing, the label contains the memory address. But we need a separate, small
    # memory to store the status, @symbol_table.
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
            # the 1st bit of the Ctrl field is used to encode ACK, purely for debugging
            ctrl=getCtrl_p(data_packet) #t uint
            #iv
            if @debug_all or @service==@debug_service
            puts  "#{@service} store_data(): Ctrl=#{ctrl}"
            if (ctrl&1)==1
                puts  "#{@service} store_data(): got ACK"
            end
            end
            #ev

            label= getReturn_as(getHeader(data_packet)) #t Word            
            data_address=getDataAddress(label) #t MemAddress
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} store_data() ADDRESS: #{data_address}"
            end
            #ev

            data_symbol=@symbol_table[data_address] #t Word
            data_status=getStatus(data_symbol) #t uint
            #iv
            if @debug_all or @service==@debug_service
                print "#{@service} store_data() STATUS: "
                puts data_status
            end
            #ev

            # we could do data_status&1!=1
            if data_status!=DS_present and data_status!=DS_eos

                # (buf 'b1 (S1 ...)) => binds res of S1 to b1 @S1, i.e. ( S1.buf<b1> ...); there's no need here for S1.buf to return an empty packet.
                # (S1 ... (cache 'c1 (S2 ...)) ... ) => binds res of S2 to c1 @S1.
                # This simply replaces the normal allocation with registered allocation, so again no need for S1 or S2 to do anything special
                # So in conclusion, the Cache control bit in the packet header is not needed.

                #                if getLength_p(data_packet)!=0
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

                #iv
                if @debug_all or @service==@debug_service
                    #                        puts "#{@service} store_data() : stored payload #{data_packet_payload.inspect} at address #{data_address}" if @v #skip
                    puts "#{@service} store_data() address #{data_address} STATUS: #{data_status}"
                end
                #ev
                #                end # if Length!=0

                subtask=getSubtask(data_symbol) #t Subtask
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} store_data() SUBTASK: #{subtask}"
                end
                #ev

                #FIXME: should I update the status of the reg_table as well?
                if (data_address<DATA_OF+NREGS)
                    @register_set[data_address].status=RDS_present
                end

                skip=0 #t uint
                eos=0 #t uint
                eosctrl=ctrl&6 #t uint
                #               puts "#{@service} store_data() CTRL: #{eosctrl}"
                if eosctrl==6
                    # a "skip" packet
                    skip=1
                elsif eosctrl==2
                    # an "eos" packet
                    eos=1
                    @symbol_table[data_address]=setStatus(data_symbol,DS_eos)
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} store_data() EOS on #{data_address}"
                    end
                    #ev
                else
                    @symbol_table[data_address]=setStatus(data_symbol,DS_present)
                end

                #sysc            subtask_list.lock();
                if @subtask_list.status(subtask)!=STS_deleted
                    @subtask_list.decr_nargs_absent(subtask)
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} store_data() SUBTASK #{subtask} in list"
                        puts "#{@service} store_data() nargs_absent=#{@subtask_list.nargs_absent(subtask)} eos=#{eos} skip=#{skip}"
                    end
                    #ev
                    if @subtask_list.nargs_absent(subtask)==0 or eos==1 or skip==1
                        # if EOS, there's no point in waiting for the other arguments
                        if eos==0 and skip==0
                            @subtask_list.status(subtask,STS_pending)
                        elsif eos==1
                            #iv
                            if @debug_all or @service==@debug_service
                                puts "#{@service} store_data() EOS: setting subtask status from #{@subtask_list.status(subtask)} to STS_eos"
                            end
                            #ev
                            # for EOS
                            @subtask_list.status(subtask,STS_eos)
                        elsif skip==1
                            #iv
                            if @debug_all or @service==@debug_service
                                puts "#{@service} store_data() SKIP: setting subtask status from #{@subtask_list.status(subtask)} to STS_skip"
                            end
                            #ev
                            # for SKIP
                            @subtask_list.status(subtask,STS_skip)
                        else #skip
                            raise "BOOM" #skip
                        end
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
            end # if status != DS_present or DS_eos
        end # of while
    end # of store_data

    # -----------------------------------------------------------------------------
    # Dispatch data packets in answer to a request packet.
    # CACHE, BUFFER and ACC can each support 8 requests per register
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
            data_status=DS_absent #t uint
            #C++ uint mode;
=begin
# Now for the new approach, assuming we have NREGS registers which allow NREQS calls.
# data_address will actually be the address of the register; the symbol will also encode
# some extra info, e.g. is this a STREAM or a GET or an ACC or a CACHE request.
# For now we assume it's a CACHE request; I think an ACC is identical but the store is different
# The register stores the following: address of the data, status bits, info indicating type if necessary
so we have:
=end
            eosreq=2 #t uint
            eos=0 #t uint
            skip=0 #t uint
            #            send_empty_packet=0 #t uint
            if getKind(var_label)==K_D
                reg_address=getReg(var_label) #t uint
                # TODO: for syncing multiple consumers of a stream, we must provide a token and the number of consumers
                # I think 32 tokens and 8 consumers is a nice number: 5+3 bits
                # but for 64-bit, make sure to have more
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} dispatch_data_packets(): Mode: CACHE: symbol: #{var_label} reg address: #{reg_address}"
                end
                #ev
                data_address=reg_address
                mode=getMode(var_label)
                reg_symbol=@symbol_table[reg_address] #t Word
                data_status=getStatus(reg_symbol)
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} dispatch_data_packets(): Mode: CACHE: reg address: #{reg_address} mode #{mode} status #{data_status}"
                    #ifdef SC_VERBOSE
                    #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " STREAM request\n";
                    #endif
                end
                #ev
                if mode==M_eos
                    #iv
                    ds_eos=DS_eos #t uint
                    puts "#{@service} dispatch_data_packets(): EOS REQUEST #{reg_address}: #{data_status}<>#{ds_eos} #{(data_status==DS_eos)}"
                    #ev
                    has_label=1
                    if data_status==DS_eos
                        eosreq=1
                    else
                        eosreq=0
                    end
                    #ifdef SC_VERBOSE
                    #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " EOS request for "<<reg_address<<":"<<(int)data_status<<"=>"<<eos<<"\n";
                    #endif
                    puts eos #skip
                else

                    if data_status==DS_present
                        has_label=1
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "#{@service} dispatch_data_packets(): Mode: CACHE: reg address #{reg_address} status DS_present"
                        end
                        #ev
                        #WV03122009: we should only restart the subtask if the data was present. If the data was not present,
                        # restarting should be delayed until it is present.# Otherwise the old data might be returned
                        if mode==M_stream
                            #iv
                            if @debug_all or @service==@debug_service
                                puts "#{@service} dispatch_data_packets(): Mode: STREAM: #{reg_address} (#{@register_set[reg_address].data_address})"
                            end
                            #ev
                            #                            if getStatus(reg_symbol)!=DS_eos
                            @symbol_table[reg_address]=setStatus(reg_symbol,DS_requested)

                            #ifdef SC_VERBOSE
                            #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " EOS: set status for "<<reg_address<<" to DS_requested =>"<<eos<<"\n";
                            #endif
                            restart_subtask(reg_address)
                        end
                        #                        end
                    elsif data_status==DS_eos
                        eos=1
                        has_label=1
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "#{@service} dispatch_data_packets(): Mode: CACHE: reg address #{reg_address} status DS_eos"
                        end
                        #ev
                    elsif data_status==DS_requested or data_status==DS_absent
                        # If this is a get() call, queue; if it's a stream() call, send empty packet!
                        if mode==M_stream
                            skip=1
                            has_label=1
                            # send empty packet
                            #                            send_empty_packet=1
                        else
                            # data is absent (or requested), queue request
                            @request_table.push(reg_address,packet_label) #nosubs
                            #iv
                            if @debug_all or @service==@debug_service
                                puts "#{@service} dispatch_data_packets(): Mode: CACHE: queue request for reg address #{reg_address} (#{@register_set[reg_address].data_address})"
                            end
                            #ev
                        end
                    end

                    # If mode==M_stream, here's where we restart the task
                    # WV25112008: I'm unsure if we require data_status==DS_present
                    #                    if mode==M_stream
                    #                        #iv
                    #                        if @debug_all or @service==@debug_service
                    #                            puts "#{@service} dispatch_data_packets(): Mode: STREAM: #{reg_address} (#{@register_set[reg_address].data_address})"
                    #                        end
                    #                        #ev
                    #                        # if the status is EOS, don't restart the subtask!
                    #                        if data_status!=DS_eos
                    #                            if @register_set[reg_address].status==RDS_present
                    #                                restart_subtask(reg_address)
                    #                                @register_set[reg_address].status=RDS_absent
                    #                            end
                    #                        end
                    #                    end
                end

            elsif getKind(var_label)==K_L
                # For a K_L variable, the address is stored in a lookup table
                # We store K_L symbols keyed by their Name
                # WV27112009 for EOS support, K_L must support DS_eos
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

            #            print  "#{@service} PRESENT?",data_address,":",(has_label==1)," and (",(data_status==DS_present)," or ",(data_status==DS_eos),")\n" if @v #skip
            #            puts "dispatch_data_packet(): has_Label=#{has_label}"
            #            puts "dispatch_data_packet(): data_status=#{data_status}" #C++ cout << "dispatch_data_packet(): data_status="<<(int)data_status<<"\n";
            if has_label==1 # or eosreq==1 or send_empty_packet==1
                # Data is present (can be EOS)
                #WV21042009: (r2526) removed all references to STRUCTURED_STORAGE
                # packetise
                payload_length=0 #t Length_t
                if getReturn_to(getHeader(request))!=getName(packet_label)
                    raise "#{getReturn_to(getHeader(request))}!=#{getName(packet_label)}" #skip
                end
                tdata=[] #C++ Word_List tdata;
                #iv
#                puts "#{@service} dispatch_data_packet(): data_address=#{data_address}, eosreq=#{eosreq}"
                #ev
                ctrl=0 #t uint
                if eosreq==2 and eos==0 and skip==0 # send_empty_packet==0
                    tdata=getField(@sba_tile.data_store.mget(data_address),offset,fsize)
                elsif eosreq!=2
                    tdatasym=mkBool(eosreq)
                    #iv
                    puts "#{@service} dispatch_data_packets(): EOS REQUEST: #{ppSymbol(tdatasym)} (#{eosreq})"
                    #ev
                    tdata.push(tdatasym)
                elsif eos==1
                    #iv
                    puts "#{@service} dispatch_data_packets(): data status for #{data_address} is EOS"
                    #ev
                    ctrl=2
                elsif skip==1
                    #iv
                    puts "#{@service} dispatch_data_packets(): data status for #{data_address} is SKIP"
                    #ev
                    ctrl=6
                end
                payload_length=tdata.length

                #                if eos==1 # because the return of the request is not an EOS
                #                    puts "#{@service} dispatch_data_packets(): data status for #{data_address} is EOS" #skip
                #                    ctrl=2
                #                    #FIXME But must send empty packet as well!!!
                #                    # how can it be that the buffer is not empty but yet has EOS set?
                #                    # if one of the args of a buffered call is EOS, we return EOS via CS_skip
                #                end
                puts "#{@service} dispatch_data_packet(): payload_length=#{payload_length}" if @v #skip
                packet_header=mkHeader(P_data,ctrl,0,payload_length, getReturn_to(getHeader(request)), NA,0,packet_label)
                packet_payload=tdata #t Word_List
                packet=mkPacket(packet_header,packet_payload)
                #iv
#                puts "#{@service} dispatch_data_packet(): PACKET: #{ppPacket(packet)}" #skip
                #ev

                #ifdef SC_VERBOSE
                #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from dispatch_data_packet()\n";
                #endif
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
    # This is the key subroutine for parsing the SBA. It implements the actual parsing rules and actions.
    def parse_subtask()
        #tile
        #WV23072008: replace by single call to length + for loop and maybe unguarded shift
        #      puts "SUBTASK FIFO:", @subtask_fifo.inspect #skip
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
            
            # Extended service symbol for tuple/struct support (not fully implemented)
            if getExt(service_symbol)==1
                service_symbol_ext=subtask.shift #C++ Word service_symbol_ext=subtask.front(); subtask.pop_front();
                offset=getOffset(service_symbol_ext) #t uint
                fsize=getSize(service_symbol_ext) #t uint
                @subtask_list.offset(parent_subtask,offset)
                @subtask_list.fsize(parent_subtask,fsize)
            end
            # In case of aliases, we must consider the first argument. So in that case we should store it.
            @subtask_list.called_as(parent_subtask,service_symbol)
            @subtask_list.nargs(parent_subtask,getNArgs(service_symbol))
            nargs_absent=getNArgs(service_symbol) #t uint
            @subtask_list.nargs_absent(parent_subtask,nargs_absent)
            #WV22062008 Logic for ACC & BUFFER
            mode=getMode(service_symbol) #t uint
            puts "#{@service} parse_subtask(): MODE: #{mode}" if @v #skip
            #C++ uint reg_addr=0;
            if mode!=M_normal # ACC,BUFFER=1
                reg_addr=getReg(service_symbol)
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} parse_subtask(): Mode #{mode}: reg #{reg_addr}"
                end
                #ev
                # we need two bits in Subtask_List for this. Use mode, but 1 extra field
                @subtask_list.mode(parent_subtask,mode)
                @subtask_list.reg(parent_subtask,reg_addr)
                # now allocate memory for the result -- unless it was already allocated
                #WV03122009: I think RDS_ status is no longer required, we can use the symbol_table for the status
                # also we should always clear the register set
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
            else

=begin                
        Local results:
        
        - for each subtask, check if it's return node id is the same as the current node id
        - if that is the case, use the return value address as the address for the result
        
        - compute as usual
        - in core_control
        return_as = @subtask_list.return_as(parent_subtask)
        if getSNId(return_as )==@service            
            data_address=getDataAddress(return_as ) #t MemAddress                
            @subtask_list.result_address(parent_subtask,data_address)
        else
            # allocate as before
        end                
=end                
                
            # Allocate memory for the result. Store in the "result" subtask field
            # Could it be that the memory was already allocated? Yes.
                if @subtask_list.result_address(parent_subtask)==0
                # not yet allocated
                    if @data_address_stack.size==0
                        raise "#{@service} parse_subtask(#{parent_subtask}): ADDRESS STACK (#{DATA_SZ}) OVERFLOW for subtask #{parent_subtask}"                    
                    end
                    @subtask_list.result_address(parent_subtask,@data_address_stack.pop())
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
            argidx=0 #t uint
            while subtask.length>0
                elt=subtask.shift #C++ Word elt=subtask.front(); subtask.pop_front();
                if @data_address_stack.size==0
                    raise "#{@service} parse_subtask(#{parent_subtask}): ADDRESS STACK (#{DATA_SZ}) OVERFLOW for subtask #{parent_subtask}"
                end
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} parse_subtask(#{parent_subtask}): STATUS: #{@subtask_list.status(parent_subtask)}"
                end
                #ev
                # WV11012001: If the elt is a single-word K_B (getKind(elt)==K_B && getExt(elt)==0)
                # then we can pass it as if it was an address, so no need to pop an address
                #C++ MemAddress data_address;
                # WV11012011 PASS-BY-VALUE
                # for this to work properly we need a bit in the subtask record to indicate if the arg is an address (0) or a value (1)
                pass_by_value = 0 #t uint
                argmode=0 #t unsigned char
                if getKind(elt)!=K_C                    
                    #WV20110612: I think we can do this for K_L, K_A as well, maybe for
                    # K_C and K_D as well but I forgot what these do.
                    if getKind(elt)==K_B && getExt(elt)==0
                        argmode=1
                        pass_by_value = 1
                    elsif getKind(elt)==K_B && getExt(elt)==1 && getNSymbols(elt)==1
                        argmode=2
                        pass_by_value = 1
                    end    
                    if pass_by_value==0                        
                        data_address=@data_address_stack.pop
                    end
                    #                    puts "#{@service} parse_subtask(#{parent_subtask}): pop ADDRESS #{data_address} off STACK" #skip
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
                #WV03122009: no need to write to symbol table here, it's done inside the next if-then
                #                @symbol_table[data_address]=arg_symbol
                #ifndef STATIC_ALLOC
                if argmode==0 or pass_by_value==0
                    @subtask_list.arguments(parent_subtask).push(data_address)
                elsif argmode==1 and pass_by_value==1
                    @subtask_list.arguments(parent_subtask).push(elt)
                elsif argmode==2 and pass_by_value==1
                    raise "BOOM!"
                    @subtask_list.arguments(parent_subtask).push(subtask.shift)                    
                end 
                # actual argument passing mode is 2 bits but we use the next 6 bits for Kind and Datatype
                argmode+=((getKind(elt)&0x7)<<5)+((getDatatype(elt)&0x7)<<2)               
                @subtask_list.argmodes(parent_subtask).push(argmode) #skip
                #C++ subtask_list.argmodes(parent_subtask,argidx,argmode);
                #else
                #FIXME: not OK for pass-by-value!
                #C++ subtask_list.arguments(parent_subtask)[argidx]=data_address;
                #endif
                argidx=argidx+1
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
                    var_label = setName(var_label,@service) # this seems very redundant, might as well be "0"
                    var_label = setSubtask(var_label,data_address)
                    arg_symbol=setStatus(arg_symbol,DS_requested)
                    @symbol_table[data_address]=arg_symbol
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} parse_subtask(#{parent_subtask}): LABEL for #{ppSymbol(elt)} is #{ppSymbol(arg_symbol)}"
                    end
                    #ev
                    store=getSNId(elt) #t uint
                    packet_type=P_reference #t Packet_type_t

                    if getKind(elt)== K_L
#                        raise "FIXME!"
# WV20110609: try to comment out next line, should work!
#                        store=S_LET # FIXME: with the new FQN approach, there is not such thing as S_LET
                        # We can have a LET at every node
                        # So I think I should store the variable "name" in the Subtask and the node of the LET in the Name
                        # Basically, I think we should be able to do this based on the node of ASSIGN
                        # provided the assign lexically preceeds any access to a variable
                        # As a temporary fix, I will assume a single LET service and alias it to S_LET
                        packet_type=P_request
                        #                    var_label = setSubtask(var_label,data_address)
                    elsif getKind(elt)== K_D
                        packet_type=P_request
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "#{@service} parse_subtask(#{parent_subtask}): Mode: K_D REQ: subtask #{getSubtask(elt)}"
                            puts "#{@service} parse_subtask(#{parent_subtask}): Mode: K_D REQ (#{getMode(elt)}) from reg #{reg_addr}"
                        end
                        #ev
                    elsif getKind(elt)== K_A # K_A should never occur: a lambda is not parsed here!
                        raise "parse_subtask(): Error: encountered K_A" #skip
                    end # Kind selection

                    payload_length=1 #t Length_t
                    ctrl=0 #t Ctrl_t
                    redir=0 #t Redir_t
                    ack_to=0 #t Word
                    requestpacket_header= mkHeader(packet_type,ctrl,redir,payload_length,store,@service,ack_to,var_label)
                    requestpacket_payload=[elt] #C++ Word_List requestpacket_payload; requestpacket_payload.push_back(elt);
                    request_packet=mkPacket(requestpacket_header,requestpacket_payload)
                    # als VM==1
                    #ifdef SC_VERBOSE
                    #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from parse_subtask()\n";
                    #endif
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
                        # TODO: This should be smarter: there is no need to wrap the request in a 4-word packet for local requests.
                        # Instead, all we need is the address allocated for the results and the address for the code
                        # So we can join these in a single word and push that fifo and set a flag somewhere ("direct_access")
                        # This fifo will be read by activate_subtask based on that flag.
                        # So: direct_access_fifo.push(getSubtask(elt) << 16 + data_address)
                        # ( we could of course push [getSubtask(elt),data_address] in Ruby but in C++ this is almost certainly slower)
                        demux_packets_by_type(request_packet)
                    end
                    # anders # VM==0
                    #WV26112008 check with Waqar: I think the SystemC model should also be able to bypass the TX fifo
                    #                @tx_fifo.push(request_packet)
                    # einde # VM

                else # if it's not 0 it must be 1
                    #                elsif getQuoted(elt)==1
                    # unquote
                    #                elt=setQuoted(elt,0) # Essential, and not nice at all!
                    #                var_label=setQuoted(var_label,0)
                    # now save unquoted symbol
                    #                var_label=setKind(var_label,K_Q)
                    if pass_by_value==0
                        #C++ Word_List elt_val;
                        elt_val=[elt] #C++ elt_val.push_back(elt);
                        if  getKind(elt) == K_Q
                            raise "K_Q should be obsolete!"
                        end
                        # if (getKind(elt) == K_B or getKind(elt) == K_Q) and getExt(elt)==1
                        if (getKind(elt) == K_B)
                            if getExt(elt)==1
                                #iv
                                if @debug_all or @service==@debug_service
                                    puts "Quoted Extended Builtin"
                                end
                                #ev
                                if getNSymbols(elt)==1                                    
                                    elt_val.push(subtask.shift)
                                else
                                    for i in 1..getNSymbols(elt) #C++ for (int i=0;i<getNSymbols(elt);i++) {
                                        elt_sym=subtask.shift #C++ Word elt_sym=subtask.front(); subtask.pop_front();
                                        elt_val.push(elt_sym)
                                    end
                                end
                            else
                                # NEW
                            end
                        else
                            #WV20022008:                     elt=setQuoted(elt,0) # Essential, and not nice at all!
                            # I don't see why unquoting is essential, let the service cores deal with it
                            #                    elt_val=[elt] #C++ elt_val.push_back(elt);
                        end
                        #iv
if @debug_all or @service==@debug_service
                        puts "#{@service} parse_subtask(#{parent_subtask}): storing #{ppPayload(elt_val)} (#{elt_val[0]}) in address #{data_address}"
end
                        #ev
                        @sba_tile.data_store.mput(data_address,elt_val)
                        #                    @symbol_table[data_address]=setStatus(@symbol_table[data_address],DS_present)
                        #WV03122009 reduce number of symbol table accesses
                        arg_symbol=setStatus(arg_symbol,DS_present)
                        @symbol_table[data_address]=arg_symbol
                        #ifdef SC_VERBOSE
                        #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " set status for "<< data_address<<" to DS_present <>EOS\n";
                        #endif
                        #iv
                        if @debug_all or @service==@debug_service
                            puts "#{@service} parse_subtask(#{parent_subtask}): LABEL for #{ppSymbol(elt)} is #{ppSymbol(@symbol_table[data_address])}"
                        end
                        #ev
                    end # of not pass_by_value
                    @subtask_list.decr_nargs_absent(parent_subtask)
                    nargs_absent=nargs_absent-1
                end # of quoted or not
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} parse_subtask(#{parent_subtask}): subtask length is #{subtask.length}; status is #{@subtask_list.status(parent_subtask)}"
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

                #                if @subtask_list.nargs_absent(parent_subtask)==0
                #                if nargs_absent==0
                @subtask_list.status(parent_subtask,STS_pending)
                @pending_subtasks_fifo.push(parent_subtask)
                #                end
            end
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} parse_subtask(#{parent_subtask}): subtask status #{@subtask_list.status(parent_subtask)}"
            end
            #ev

            #sysc               subtask_list.unlock();

            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} parse_subtask(#{parent_subtask}): end parsing #{parent_subtask}" #sysc
            end
            #ev
        end # of while subtask fifo not empty
    end # of parse_subtask

    # -----------------------------------------------------------------------------
    #ifndef STATIC_ALLOC

    # helper function for core_control()
    def build_value_address_list() #t MemAddresses
        # tile
        addresses=[] #t MemAddresses #s/=..//
        nargs=@subtask_list.nargs(@current_subtask) #t uint
        #iv
        if @v #skip
            puts "#{@service} build_value_address_list(): #{nargs}<>#{@subtask_list.arguments(@current_subtask).size()}"
        end #skip
        #ev

        if nargs>0
            args=@subtask_list.arguments(@current_subtask) #t Subtask_Argument_List&
            for address in args #t MemAddresses
                addresses.push(address)
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
        # Ctrl set to 1 to denote that this is an ACK, for debugging
        ctrl=1 #t Ctrl_t
        # We don't want to redir the ACK!
        redir=0 #t Redir_t
        ack_to=@subtask_list.ack_to(@current_subtask) #t Word
        #        puts "#{@service} ACK: send_ack() to #{to} as #{ppSymbol(ack_to)}, return_as #{ppSymbol(return_as)}" #skip
        packet_header=mkHeader(P_data,ctrl,redir,payload_length,to,return_to,ack_to,return_as)
        packet_payload=[return_as] #C++ Word_List packet_payload; packet_payload.push_back(return_as);
        packet=mkPacket(packet_header,packet_payload)
        # Put on the TX fifo
        # als VM==1
        if to != @service
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} send_ack(): Sending packet via NoC: #{to}<>#{@service}"
            end
            #ev
            #ifdef SC_VERBOSE
            #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from send_ack()\n";
            #endif
            @sba_tile.transceiver.tx_fifo.push(packet)
        else
            demux_packets_by_type(packet)
        end
        # anders # VM==0
        #                        @tx_fifo.push(packet)
        # einde # VM
        puts ppPacket(packet) if @v #skip
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
    # The FSM that handles all state transitions and communication between the
# Service Manager and the core
=begin
 Core is Idle, there are pending tasks =>
 * Take the first pending subtask
 * If Subtask status is Pending
     * set Subtask status to Processing
     * set Core status to Ready
 * If Subtask status is EOS or Skip
     * set Core status to EOS resp. Skip UNLESS the service is a control service!!!
     * set the Subtask status to Processed
     * set the return packet type to Data

 Core is Ready or EOS or Skip =>
    * get addresses, opcode, scid
    * clear results store,
    * Ready => set core status to Busy

 Core is Busy =>
    * ServiceCore will set the core status to Done before handing control to the core.
    * Core can set the status to Busy or Managed or DoneEOS

 Core is Done or DoneEOS or EOS or Skip =>
    * If Subtask status was Processing, set to Processed
    * Get the results from the results store and push them out on the network or buffer them

 Core is Done or Managed =>
    - Managed means not quite done but not blocking either
    - Typically for LET with quoted args etc.
    - This allows the core to send packets that are different from the default result packet or send nothing at all
    * Send an ACK if required
    * Clean up now, after the dispatch
    * Core can avoid clean-up by setting the Subtask status to Blocked
    - in case of ASSIGN, the subtask is removed but the memory is not cleaned up (obviously) by setting Subtask status to CleanedUp from the core
    - in case of recursion we want the opposite: clean up memory but don't remove the subtask
    * If Subtask status is Processed or Inactive
        * Clean-up
        * If Subtask status is Processed,set Subtask status to CleanedUp
    * If Subtask status is CleanedUp, everything is de-allocated, remove the subtask. This sets the status to Deleted
    * If Subtask status is CleanedUp or Inactive
        * The subtask should not be cleared but we want to reuse it, so put it back on the stack!
        * Only clean up if the subtask is not buffered, otherwise we get different subtasks with the same number
        * Reset the current Subtask (to 0)
    * If Subtask was Blocked, set to New
    * Clear the results store
    * set core status to Idle

=end

    def core_control()
        #tile
        #iv
        if @debug_all or @service==@debug_service
            #            puts "#{@service} core_control(): core_status = #{(@core_status!=0)?@core_status:CS_idle}" if @v #skip
        end
        #ev
        #sysc while (true) {
        #        redir=0 #t uint
        #        restart=0 #t uint
        # Core is idle, there are pending tasks => take the first pending task;
        # set core status to ready
        if @core_status == CS_idle and @pending_subtasks_fifo.length>0 #skipsysc
            #sysc        if (core_status == CS_idle) {
            #iv
            if @debug_all or @service==@debug_service
            puts "#{@service} core_control(): set core_status to CS_Ready"
            end
            #ev
            @current_subtask=@pending_subtasks_fifo.shift
            #iv
            if @debug_all or @service==@debug_service
            puts "#{@service} core_control(): shifted #{@current_subtask} off pending_subtasks_fifo" #sysc
            end
            #ev
            #sysc            subtask_list.lock();
            subtask_status=@subtask_list.status(@current_subtask) #t uint
            #iv
            if @debug_all or @service==@debug_service
            puts "#{@service} core_control(): subtask status #{subtask_status}"
            end
            #ev
            if subtask_status==STS_pending
                #iv
                if @debug_all or @service==@debug_service
                puts "#{@service} core_control(): set status for subtask #{@current_subtask} to STS_processing"
                end
                #ev
                @subtask_list.status(@current_subtask,STS_processing)
            elsif subtask_status==STS_eos or subtask_status==STS_skip
                #WV27112009: the problem is that a core like LET needs to run even if the data is EOS
                #FIXME: ugly hack
                # what about ds_DYNAMIC? clearly we need to label services as control/not control
                # A_S_CONFRUN does the following:
                # 1. store a qref and data from some service
                # 2. activate the qref, wait for the return val & run
                # in step 1., clearly an EOS would stop the processing.
                # but there is no need to run the core if the result is EOS

                # FIXME: with multi-service nodes, the node is not simply control or computation
                # That means the core_control should decide based on the type of the ServiceClass of the active subtask
                # i.e. we want
                # if @servicetype!=CONTROL
                # and this "@servicetype" is determined by parse_subtask, based on the SCId?
                # All this can only reasonably work in 64-bits, then I can use a bit in the the Task field to indicate Control or Data
                # As a temporary fix we assume a single LET or IF core and no other control services...
                if @service!=S_LET and @service!=S_IF 
                    if subtask_status==STS_skip
                        @core_status = CS_skip
                    else
                        @core_status = CS_eos
                    end
                    # if we skip the core, the assumption is that the core is returning
                    # an empty data packet, so we must make sure
                    # Notably ds_DYNAMIC sets the core to P_reference.
                    @core_return_type=P_data
                else
                    if @debug_all or @service==@debug_service
                    puts "EOS but control service #{@service}" #skip
                    end
                end
                @subtask_list.status(@current_subtask,STS_processed)
            else
                #iv
                if @debug_all or @service==@debug_service
                puts "#{@service} core_control(): status for subtask #{current_subtask} is #{@subtask_list.status(@current_subtask)}"
                end
                #ev
            end
            #sysc subtask_list.unlock();
            if @core_status != CS_eos and @core_status != CS_skip
                @core_status = CS_ready
            end

        else  # if (core_status != CS_idle)
            #ifdef SC_VERBOSE
            #sysc OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            #sysc <<name()<<" core_control(): Waiting for ServiceCore (core_status=="<<core_status<<")\n";
            #endif
            #sysc //if ((core_status != CS_done) && (core_status != CS_eos ) ) {
            #sysc if ( core_status == CS_busy ) {
            #sysc      wait(core_status.value_changed_event());
            #sysc  }
            #ifdef SC_VERBOSE
            #sysc OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " #sysc <<name()<<" core_control(): done waiting (core_status=="<<core_status<<")\n";
            #endif
        end  # CS_idle

        # Core is ready => get addresses, clear results store,
        # set core status to busy
        if @core_status == CS_ready or @core_status == CS_eos or @core_status == CS_skip
            @n_args=@subtask_list.nargs(@current_subtask)
            #ifndef STATIC_ALLOC
            @arg_addresses=build_value_address_list()
            #else
            #C++ arg_addresses=subtask_list.arguments(current_subtask);
            #C++ arg_addresses.size(n_args);
            #endif
            called_as=subtask_list.called_as(current_subtask) #t Symbol_t
            @opcode=getOpcode(called_as)
            @scid=getSCId(called_as)
            @sclid=getSCLId(called_as)
            #iv
            if @debug_all or @service==@debug_service            
                puts "#{@service} CORE CONTROL: SCLId:#{@sclid}, SCId:#{@scid}, Opcode:#{@opcode}, SId:#{@service_id}" 
                print "#{@service} CORE CONTROL: ["
                for arg_addr in @arg_addresses #t MemAddresses
                    print " #{arg_addr}"
                end
                puts " ]"                
            end
#                if @v #skip
#                puts "#{@service} CORE CONTROL: #{@arg_addresses[0]}"
#                end #skip
            
            #ev
            # push the result_address on the stack
            # FIXME! Either this or the other occurence (line 1989) is redundant! 
            results_address=@subtask_list.result_address(@current_subtask) #t MemAddress
            if results_address!=0
                @data_address_stack.push(results_address)    
                @subtask_list.result_address(@current_subtask,0)
            end    
            if @core_status==CS_ready
                @core_status=CS_busy
            end
        end   # CS_ready
        # Core is busy => ServiceCore will set the core status to CS_done before handing control to the core.
        # However, the the core can set the status to "busy" or "managed"
        # Note that the ServiceManager takes no action at all while the core status is CS_busy
        #        ctrl=0 #t uint
        #        empty_packet=0 #t uint
        #        if @core_status==CS_done_eos
        #            ctrl=2 # must be EOS
        #            if @core_return_type==P_data
        #                empty_packet=1
        #            end
        #            mode=@subtask_list.mode(@current_subtask) #t uint
        #            if mode!=M_normal
        #                reg_addr = @subtask_list.reg(@current_subtask) #t uint
        #                reg_symbol=@symbol_table[reg_addr] #t Word
        #                reg_symbol=setStatus(reg_symbol,DS_eos)
        #                @symbol_table[reg_addr]=reg_symbol
        #            else # end of un uncached stream: set EOS bit
        #
        #            end
        ##            @core_status=CS_done
        #        end
        # if core status is done => create TX packet & dispatch
        if (@core_status == CS_done) or (@core_status == CS_done_eos) or (@core_status == CS_eos) or (@core_status == CS_skip)
            #iv
            if @debug_all or @service==@debug_service   
            ppstatus=(@core_status == CS_done)?"CS_done":((@core_status == CS_skip)?"CS_skip":"CS_eos") #t string
            puts "#{@service} core_control():  ServiceCore status is #{ppstatus} for <#{@current_subtask}>"
            end
            #ev
            #ifdef SC_VERBOSE
            #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " core status is "<<core_status<<" <>EOS1\n";
            #endif
            if @subtask_list.status(@current_subtask)==STS_processing
                @subtask_list.status(@current_subtask,STS_processed)
            end
            # get the results from the results store and push them out on the network
            to = @subtask_list.to(@current_subtask) #t Service
            return_to=@subtask_list.return_to(@current_subtask) #t Service
            return_as=@subtask_list.return_as(@current_subtask) #t Word
            # Create the result packet
            ctrl=0 #t uint
            eos=0 #t uint
            skip=0 #t uint
            empty_packet=0 #t uint
            if (@core_status==CS_skip)
                skip=1
                ctrl=6
            elsif (@core_status==CS_eos) or (@core_status == CS_done_eos) 
                # I guess this is wrong: a service receiving and EOS should return an EOS, but a
                # service receiving an empty packet should return an empty packet
                # But how then do we know the EOS is reached? The service will have to set it explicitly
                #ifdef SC_VERBOSE
                #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") EOS core status:"<<core_status<<";"<<results_store.size()<<endl;
                #endif
                eos=1
                ctrl=2 # must be EOS
            end
            if (@core_status==CS_skip) or (@core_status==CS_eos) or (@core_status == CS_done_eos)
                if @core_return_type==P_data
                    empty_packet=1
                end
            end
            #            if @core_status != CS_done_eos
            #                @core_status = CS_done
            #            end
            #iv
            #            puts "EMPTY PACKET? #{empty_packet}"
            #ev
            redir=@subtask_list.redir(@current_subtask) #t uint
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
            packet_payload=[] #skip
            payload_length=0
            #            puts "MODE: #{mode}" #skip
            if mode==M_normal
                if empty_packet==0                   
                    results_address=@subtask_list.result_address(@current_subtask) #t MemAddress
                    results=@sba_tile.data_store.mget(results_address) #t Word_List                       
                    puts results.inspect if @v #skip
                    packet_payload=getField(results,offset,fsize)
                    payload_length=packet_payload.length
                else
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} core_control(): EOS on #{@current_subtask}"
                    end
                    #ev
                end
            else
                # mode!=M_normal, so we cache the result
                reg_addr = @subtask_list.reg(@current_subtask) #t uint
                # WV15122009: maybe this should be done in parse_subtask()
                #                reg_symbol=mkSymbol(K_D,0,0,0,0,((mode << FS_Mode)+(reg_addr << FS_Reg)),@service)
                reg_symbol=@symbol_table[reg_addr] #t Word
                #iv
                if @debug_all or @service==@debug_service
                    puts "#{@service} core_control(): Mode #{mode}: buffering; reg: #{reg_addr} <#{reg_symbol}> "                
                    puts " in reg #{reg_addr}"
                end
                #ev

                #WV26112008: At first I thought we should return the register as a symbol
                # but that is not possible as the data store address is in use for the result
                # so we should simply return an empty packet; store_data will set the status to Present
                #                packet_payload=[reg_symbol] #C++ packet_payload.push_back(reg_symbol);
                #                payload_length=1
                #                ctrl+=4
                #WV22062008 now get the address and store the result
                #                res_addr=@register_set[reg_addr].data_address #t MemAddress
                #                @sba_tile.data_store.mput(res_addr,@results_store)
                #WV22042009 for tuple support we need to be able to specify
                # on the one hand the offset and fsize of the field in the results_store
                # this should be stored in the Subtask_List, it's actually part of the packet header
                # By default it's offset=0,fsize=0 (means everything!)
                # on the other hand the offset (and fsize) of the field in the data_store, if we buffer the result

                #  Word_List results(results_store.read_all());

                #WV26112009: not quite sure about this: I think it's present although it's empty as this is the result
                @register_set[reg_addr].status=RDS_present
                #C++ Word reg_status_symbol;
                if empty_packet==0                   
                    results_address=@subtask_list.result_address(@current_subtask) #t MemAddress
                    results=@sba_tile.data_store.mget(results_address) #t Word_List                        
                    @sba_tile.data_store.mput(reg_addr,getField(results,offset,fsize))
                    reg_status_symbol=setStatus(reg_symbol,DS_present)
                    #ifdef SC_VERBOSE
                    #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " set status for reg "<< reg_addr<<" to DS_present <>EOS\n";
                    #endif
                elsif eos==1
                    # EOS, send empty packet
                    reg_status_symbol=setStatus(reg_symbol,DS_eos)
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service} core_control(): REG #{reg_addr} STATUS: EOS"
                        puts "#{@service} core_control(): EOS on #{@current_subtask}"
                    end
                    #ev
                    #ifdef SC_VERBOSE
                    #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " set status for reg "<< reg_addr<<" to DS_eos: EOS!\n";
                    #endif
                elsif skip==1
                    reg_status_symbol=setStatus(reg_symbol,DS_requested)
                end
                @symbol_table[reg_addr]=reg_status_symbol
                #ifdef SC_VERBOSE
                #sysc  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " reg status for "<< reg_addr<<" is "<<(int)getStatus(reg_status_symbol)  <<"\n";
                #endif
                # Now check if there are pending requests
                #iv
                if @debug_all or @service==@debug_service
                    #                    puts "#{@service} core_control(): request table for #{reg_addr} contains #{@request_table.table[reg_addr].inspect}" #skip
                end
                #ev
                if @request_table.size(reg_addr)>0  and empty_packet==0
                    # there are pending requests, handle them
                    # i.e. dispatch the data packets
                    # FIXME: we should factor this out as a function
                    #iv
                    if @v #skip
                        puts  "#{@service} core_control(): PENDING REQUESTS for #{reg_addr}: #{@request_table.size(reg_addr)}"
                    end #skip
                    #ev
                    n_pending_reqs= @request_table.size(reg_addr) #t uint
                    #ifdef SC_VERBOSE
                    #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") pending requests:"<< n_pending_reqs << "\n";
                    #endif
                    for req in 1..n_pending_reqs #t uint
                        packet_label=@request_table.shift(reg_addr) #t Word
                        # packetise
                        data_address=@register_set[reg_addr].data_address #t MemAddress
                        # WV06012010: FIXME: this copy takes time and is redundant!
                        # the payload should be transferred straight from memory
                        # The "push" does already take time so we're counting this twice
                        packet_payload=@sba_tile.data_store.mget(data_address) #t Word_List
                        payload_length=packet_payload.length
                        #                    restart=1 #FIXME: this should be taken from some bit in the packet label!!!
                        packet_header=mkHeader(P_data,ctrl,0,payload_length, getName(packet_label), NA,0,packet_label)
                        packet=mkPacket(packet_header,packet_payload)
                        # it turns out that here, the payload is empty although the length is set to 65 (64 pix + 1 debug word)
                        # Got it: packet_payload is local to the if!
                        # So this is a bug, but one that makes things work: sending an empty packet results in the service
                        # appending nothing and getting the next block. If I fix this, blocks will get duplicated
                        # The question is if this behaviour is actually not OK for streaming (for get it is obv. not OK!)
                        #ifdef SC_VERBOSE
                        #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send queued packet (length "<< (int)payload_length<<", actual "<< packet_payload.size()<<") from core_control()\n";
                        #endif
                        # A shortcut: we don't want to go via the network unless we have to
                        # This should actually almost never happen
                        if getTo(packet_header) != @service
                            @sba_tile.transceiver.tx_fifo.push(packet)
                        else
                            @data_fifo.push(packet)
                        end
                    end
                end
            end # mode!=M_normal
            packet_header=mkHeader(@core_return_type,ctrl,redir,payload_length,to,return_to,ack_to,return_as)
            packet=mkPacket(packet_header,packet_payload)
            # Put on the TX fifo
            #iv
            if @debug_all or @service==@debug_service
                puts "#{@service} core_control(): PACKET:\n #{ppPacket(packet)}"
            end
            #ev
            #ifdef SC_VERBOSE
            #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from core_control()\n";
            #endif
            if to != @service
                @sba_tile.transceiver.tx_fifo.push(packet)
            else
                demux_packets_by_type(packet)
            end
            @core_status = CS_done
        end # of CS_done || CS_done_eos || CS_eos || CS_skip

        # Core is done or being "managed", i.e. not quite done but not blocking either
        # Typically for LET with quoted args etc.
        # This allows the core to send packets that are different from the default result packet
        # or send nothing at all
        if @core_status == CS_done or @core_status == CS_managed # or @core_status == CS_done_eos
            redir=@subtask_list.redir(@current_subtask) #t uint
            if VM==1
                #            puts "#{@service} core status: #{@core_status} ACK? ack_ok #{@ack_ok}; redir: #{redir} => #{@ack_ok*redir}" if @v #skip
            end # VM
            #ifdef SC_VERBOSE
            #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") core status is "<<core_status<<" <>EOS2"<<endl;
            #endif
            # Now let's check if we need to send an ACK
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
            sts=@subtask_list.status(@current_subtask) #t Subtask_Status

            #            puts "#{@service} CLEAN-UP #{@current_subtask}? STS=#{sts}<>3,4,6" if @v #skip
            #            puts "#{@service} CLEAN-UP #{@current_subtask}? #{sts}==#{STS_processed} or #{sts}==#{STS_cleanup}" if @v #skip
            if (sts==STS_processed or sts==STS_cleanup or sts==STS_inactive) # and mode!=M_stream # 3=Done/Processed; 4=CleanedUp; 5=Blocking
                if sts==STS_processed or sts==STS_inactive
                    #iv
                    if @v #skip
                        puts "#{@service} CLEAN-UP: clean_up() #{@current_subtask}"
                    end #skip
                    #ev
                    clean_up()
                    if sts==STS_processed
                        @subtask_list.status(@current_subtask,STS_cleanup) # Cleaned-up
                        sts=STS_cleanup
                    end
                end
                if sts==STS_cleanup # sts==STS_processed or
                    # everything is de-allocated, now remove the subtask
                    #iv
                    if @v #skip
                        puts "#{@service} CLEAN-UP: remove #{@current_subtask}, set status to STS_deleted"
                    end #skip
                    #ev
                    @subtask_list.remove(@current_subtask) # this sets the status to STS_deleted
                end
                if sts==STS_cleanup or sts==STS_inactive # sts==STS_processed or
                    #WV22052009 I think the subtask should not be cleared but we want to reuse it, so put it back on the stack!
                    if VM==1
                        # push the address back onto the subtask address stack:
                        #iv
                        if @v #skip
                            puts "#{@service} CLEAN-UP: push #{@current_subtask} onto subtasks_address_stack"
                        end #skip
                        #ev
                        # only clean up if the subtask is not buffered, otherwise we get different subtasks with the same number
                        if @subtask_list.reg(@current_subtask)==0
                            @subtasks_address_stack.push(@current_subtask)
                        elsif @v #skip
                            #                            puts "#{@service} CLEAN-UP: SUBTASK #{@current_subtask} is buffered, can't clean" #skip
                        end

                    else # VM==0
                        # nothing happens as addressing is static
                    end # VM
                    @current_subtask=0
                end
            elsif sts==STS_blocked # subtask status is 5, reset to 0! (surely it can't be 1 or 2)
                #iv
                if @v #skip
                    puts "#{@service} CLEAN-UP: reset #{@current_subtask} status to STS_new"
                end #skip
                #ev
                @subtask_list.status(@current_subtask,STS_new)
            else
                #iv
                if @v #skip
                    puts "#{@service} CLEAN-UP: STS=#{sts}, NO CLEAN-UP for #{@current_subtask}"
                end #skip
                #                if (@subtask_list.status(@current_subtask)==STS_processed or @subtask_list.status(@current_subtask)==STS_cleanup) and mode==M_stream
                #                    puts "#{@service} status #{@current_subtask} = #{@subtask_list.status(@current_subtask)}"
                #                    puts "#{@service} CLEAN-UP #{@current_subtask} for BUF"
                #                    # We want the subtask entry to persist but the addresses for data should have been reclaimed.
                if @core_status == CS_managed
                    puts "#{@service} CLEAN-UP: STS=#{sts}, CS_managed: CLEAN-UP for #{@current_subtask}" if @v #skip
                    clean_up()
                end
                #
                #                end
                #ev
            end

            puts "STS for <#{@current_subtask}>: #{@subtask_list.status(@current_subtask)}" unless (@current_subtask==0 or not @v) #skip
            # Clear the results store

            # push the result_address on the stack
            if (@current_subtask!=0)
                results_address=@subtask_list.result_address(@current_subtask) #t MemAddress
                if results_address!=0
                    @data_address_stack.push(results_address)    
                    @subtask_list.result_address(@current_subtask,0)
                end
            end
                   
            #ifdef SC_VERBOSE
            #sysc          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") sets core status to CS_idle\n";
            #endif
            @core_status= CS_idle
        end # of CS_done || CS_managed

        # so maybe we can restart the subtask here?
        #        if restart==1
        #            reg_addr = @subtask_list.reg(@current_subtask) #t uint
        ##            restart_subtask(reg_addr)
        #            #WV03122009: somehow I get the correct image reconstruction in SystemC although the task is not restarted ...
        #            #ifdef SYSC
        #            puts "#{@service} core_control() RESTART: #{@current_subtask} reg #{reg_addr}" #sysc
        #            #endif //SYSC
        #        end
        #sysc  } // end of while(true)
    end # of core_control()
    # -----------------------------------------------------------------------------

    # -----------------------------------------------------------------------------

    # This method does the actual memory management after the core finishes a subtask. It's called from core_control.
    def clean_up()
        #tile
        # Clean up arguments
        nargs=@arg_addresses.size() #t uint
        #iv
        if @debug_all or @service==@debug_service
            if @v #skip
                puts "#{@service}: clean_up #{@current_subtask}:  ADDRESSES: #{@arg_addresses.inspect},nargs:#{nargs}" #skip
            else #skip
                puts "#{@service}: clean_up #{@current_subtask}:  ADDRESSES: #{nargs}" #sysc
            end #skip
        end
        #ev
        if nargs>0
            for iter_ in 0..nargs-1 #t uint
                if @subtask_list.argmodes(@current_subtask)[iter_]==0
                    arg_address=@arg_addresses[iter_] #t MemAddress
                    #iv
                    if @debug_all or @service==@debug_service
                        puts "#{@service}: clean_up:  ADDRESS: #{arg_address}"
                    end
                    #ev
                    #sysc //symbol_table.lock();
                    arg_symbol=@symbol_table[arg_address] #t Word
                    arg_status=getStatus(arg_symbol) #t uint
                    if arg_status==DS_present or  arg_status==DS_eos
                        if arg_address>NREGS+DATA_OF
                            arg_symbol=setStatus(arg_symbol,DS_cleared)
                            @data_address_stack.push(arg_address)
                            #iv
                            if @debug_all or @service==@debug_service
                                puts "#{@service}: clean_up #{@current_subtask}: push ADDRESS #{arg_address} onto STACK"
                                puts "#{@service}: clean_up(): stack size/pointer #{@data_address_stack.size()}"
                                puts "#{@service}: clean_up: REMOVED #{arg_address}"
                            end
                            #ev
                            @sba_tile.data_store.remove(arg_address)
                        else
                            # I think for a register address we still must set the status to 'absent'
                            # What about RDS_
                            arg_symbol=setStatus(arg_symbol,DS_absent)
                        end
                        @symbol_table[arg_address]=arg_symbol
                    end # if DS_present or DS_eos
                    #sysc //symbol_table.unlock();
                end
            end # of for
        end

    end # of clean_up

end # of SBA_ServiceManager class

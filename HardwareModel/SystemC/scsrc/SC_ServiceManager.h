/*
********************************************************************************
                 |
  File Name      | SC_ServiceManager.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 29-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Service Manager Class
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/

#ifndef SC_SERVICEMANAGER_H_
#define SC_SERVICEMANAGER_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
// DEFS
//------------------------------------------------------------------------------
//#define DATA_SZ = 10 // temp define. Should be in the Make / SCons?

//==============================================================================
//  CLASS: SERVICE MANAGER
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the service manager
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_ServiceManager : public sc_module
{
public:
    SC_Register         <SBA::Service >             service;                //!<
    SC_Register         <SBA::ServiceAddress>       address;                //!<
    SC_Register 		<SBA::Service> 				service_id;				//!< accessed by service_core
    SC_Register         <SBA::Service >             debug_service;          //!<
    bool                                            debug_all;              //!<
    bool                                            status;                 //!<
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if     <Packet_t>          transceiver_rx_fifo;//!< read/write port for transceiver rx fifo
    port_SC_Fifo_if     <Packet_t>          transceiver_tx_fifo;//!< read/write port for transceiver tx fifo
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;      //!< write/read port for data memory access
    port_SC_Stack_if 						data_address_stack;
    //port_SC_Memory_if   <ADDR_T, DATA_T>    data_store_2;   //!< 2nd write/read port for data memory access
    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store_1;   //!< 1st write/read port for code memory access
    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store_2;   //!< 2nd write/read port for code memory access
    port_SC_Memory_if<ADDR_T,RegisterEntry> register_set;     //!< w/r port for accessing register memory
    port_SC_SubtaskList_if                  subtask_list;     //!< w/r port for accessing subtask_list
    port_SC_RequestTable_if					request_table_1;
    port_SC_RequestTable_if					request_table_2;
    port_SC_LookupTable_if					lookup_table;
    // ---------------------------- EXPORTS ------------------------------------
    sc_export< SC_reg_out_if_d<SBA::Core_Status> >  xpwr_corestatus;    //!< export of core_status register is sent upwards to give access to service_core
    sc_export< SC_Deque_if <SBA::Word> >            xp_results_store;   //!< export of results_store for the service_core
    sc_export< SC_reg_out_if_d<bool> >              xp_ack_ok;          //!< export of ack_ok (arbitered) for service_core
    sc_export< SC_reg_out_if_d<SBA::Packet_Type> >  xp_core_return_type;//!< export of core_return_type for service_core
    sc_export< SC_reg_out_if_d<SBA::Subtask>     >  xp_current_subtask; //!< export of current_subtask for service core
    sc_export< SC_reg_out_if_d<uint            > >  xp_n_args;          //!< export of n_args          for service core
    sc_export< SC_reg_out_if_d<uint            > >  xp_opcode;          //!< export of opcode          for service core
    sc_export< SC_Deque_if<SBA::MemAddress> >       xp_arg_addresses;   //!< export of arg_addresses for service_core
    sc_export< SC_reg_out_if_d<Service> >  			xp_service_id;      //!< export of service_id for service core
    sc_export< SC_Memory_if <ADDR_T, SBA::Word> >   xp_symbol_table;
    sc_export< SC_Fifo_if<Packet_t> >         xp_subtask_reference_fifo;//!< read port for posting straight into subtask_reference_fifo
    sc_export< SC_Fifo_if<Packet_t> >         xp_transceiver_tx_fifo;//!< to route servicecore transceiver_tx_fifo through arbiter
    sc_export< SC_Memory_if <ADDR_T, DATA_T> >  xp_code_store; //!< to route servicecore transceiver_tx_fifo through arbiter
//    sc_export< SC_Stack_if >  xp_data_address_stack; // !< for access from ServiceCore

    // ---------------------------- Sub-Modules --------------------------------
    //SC_service_core                                     service_core;          //!< Interface to the core
    SC_receive_packets                          receive_packets;       //!< ...
    SC_store_subtask_code   <ADDR_T, DATA_T>    store_subtask_code;    //!< ...
    SC_activate_subtask     <ADDR_T, DATA_T>    activate_subtask; //!< ...
    SC_store_data           <ADDR_T, DATA_T>    store_data;    //!< ...
    SC_parse_subtask        <ADDR_T, DATA_T>    parse_subtask; //!< ...
    SC_core_control         <ADDR_T, DATA_T>    core_control;          //!< Control logic for core access
//    SC_prepare_subtask                          prepare_subtask;       //!< ...
    //SC_transmit_packets     <PACKET_T>                  transmit_packets;      //!< ...
    SC_dispatch_data_packets<ADDR_T, DATA_T>    dispatch_data_packets; //!< ...

    // ---------------------------- Data Containers, Registers, Arbiters -------
    //SC_Fifo             <PACKET_T, PACKET_FIFO_SZ>  tx_fifo;                //!< The Fifo stroring transmit packets
    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  data_fifo;              //!< ...
    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  subtask_code_fifo;      //!< ...
    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  request_fifo;           //!< ...
    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  subtask_reference_fifo; //!< ...
    SC_Fifo             <Word, PACKET_FIFO_SZ>  subtask_fifo;           //!< Used?
    SC_Fifo             <Subtask , PACKET_FIFO_SZ>  pending_subtasks_fifo;  //!<

    SC_Fifo_Arbiter      <Packet_t>                  arb_tx_fifo;            //!< Arbiter for write contention on tx_fifo
    SC_Fifo_Arbiter      <Packet_t>                  arb_data_fifo;          //!< Arbiter for write contention on data_fifo
    SC_Fifo_Arbiter      <Packet_t>                  arb_subtask_code_fifo;  //!< Arbiter for write contention on subtask_code_fifo
    SC_Fifo_Arbiter      <Packet_t>                  arb_request_fifo;       //!< Arbiter for write contention on request_fifo
    SC_Fifo_Arbiter      <Packet_t>                  arb_subtask_reference_fifo;//!< Arbiter for write contention on subtask_reference_fifo
    SC_Fifo_Arbiter      <Word>                  	arb_subtask_fifo;       //!< Arbiter for write contention on subtask_fifo
    // it seems only core_control is accessing this fifo; but still arbitering for ease of later connections
    SC_Fifo_Arbiter      <Subtask >                  arb_pending_subtasks_fifo;            //!< Arbiter for write contention on pending_subtasks_fifo

    // this stack seems to have one writer (core_control) and one reader (parse_subtask)
    // so I am presuming no contention, and hence directly giving each of them access to one of the stack's port
    // if this is not correct then will need to make and include and arbiter for the stack interface
//    SC_Stack            <DATA_SZ>                   data_address_stack;     // "
    // Following stack for VM only. So commented and not connected.
    //SC_Stack            <SUBTASKS_SZ>               subtasks_address_stack;
	SC_Stack_Arbiter     			                arb_data_address_stack;

    SC_Register         <Core_Status>               core_status;            //!< SC_Register of enumerated type for storing core_status
    SC_Register_Arbiter       <Core_Status>               arb_core_status;         //!< Arbiter managing contention on core_status

    SC_Register         <bool>                      ack_ok;                 //!< shared b/w core_control and service_core
    SC_Register_Arbiter       <bool>                      arb_ack_ok;             //!< Arbiter managing contention on ack_ok boolean

    SC_Register         <Packet_Type >              core_return_type;       //!< shared between core_control and service_core
    SC_Register_Arbiter       <Packet_Type >              arb_core_return_type;         //!< Arbiter managing contention on core_return_type

    SC_Register         <SBA::Subtask     >         current_subtask;
    SC_Register_Arbiter       <SBA::Subtask     >         arb_current_subtask;    //!< Arbiter managing contention on current_subtask

    SC_Register         <uint             >         n_args;
    SC_Register_Arbiter       <uint             >         arb_n_args;             //!< Arbiter managing contention on n_args

    SC_Register         <uint             >         opcode;
    SC_Register_Arbiter       <uint             >         arb_opcode;             //!< Arbiter managing contention on arb_core_return_type

    SC_Memory           <ADDR_T, uint>              code_status;            //!< Modelled as a small memory
    SC_Memory_Arbiter       <ADDR_T, uint>              arb_code_status;         //!< Arbiter for contention on code_status

    SC_Memory           <ADDR_T, SBA::Word>         symbol_table;           //!< Modelled as a small memory
    SC_Memory_Arbiter       <ADDR_T, SBA::Word>         arb_symbol_table;           //!< Arbiter for contention on symbol_table


//    SC_Memory           <ADDR_T, Requests>          request_table;          //!< Modelled as a small memory
//    SC_Memory_Arbiter       <ADDR_T, Requests>          arb_request_table;           //!< Arbiter for contention on request_table

    SC_Deque            <SBA::MemAddress>           arg_addresses;
    SC_Deque_Arbiter     <SBA::MemAddress>           arb_arg_addresses;

    SC_Deque            <SBA::Word>                 results_store;          //!< For this FIFO, one reader (core_control) and one writer(service_core)
    SC_Register         <SBA::MemAddress >          service_core_ram;       //!<

    SC_Memory_Arbiter       <ADDR_T, RegisterEntry>     arb_register_set;             //!< Arbiter for register_set
    SC_Subtask_List_Arbiter                                arb_subtask_list;             //!< Arbiter for subtask_list
    // service manager has access to only one port of data_storate (the other is for core), so use normal N-to-1 arbiter
    SC_Memory_Arbiter       <ADDR_T, DATA_T>            arb_data_store;               //!<
    // service manager has access to both ports of code_store, so use N-to-2 arbiter
    SC_Memory_Arbiter_dual  <ADDR_T, DATA_T>            arb_code_store;               //!< Arbiter for dual-ported code_store

    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_ServiceManager"; }

    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_ServiceManager);

    SC_ServiceManager(  sc_module_name      nm_ ,
                        SBA::Service        s_  ,
                        SBA::ServiceAddress a_  ):
        sc_module               (nm_),
        service                 ("service", s_)             ,
        address                 ("address", a_)             ,
        service_id				("service_id")				,
        debug_service           ("debug_service")           ,
        receive_packets         ("receive_packets")         ,
        store_subtask_code      ("store_subtask_code")      ,
        activate_subtask        ("activate_subtask")   ,
        store_data              ("store_data")      ,
//        prepare_subtask         ("prepare_subtask")         ,
        parse_subtask           ("parse_subtask")   ,
        core_control            ("core_control")            ,
        //transmit_packets        ("transmit_packets")        ,
        dispatch_data_packets   ("dispatch_data_packets")   ,

        //tx_fifo                 ("tx_fifo")                 ,
        data_fifo               ("data_fifo")               ,
        subtask_code_fifo       ("subtask_code_fifo")       ,
        request_fifo            ("request_fifo")            ,
        subtask_reference_fifo  ("subtask_reference_fifo")  ,
        subtask_fifo            ("subtask_fifo")            ,
        pending_subtasks_fifo   ("pending_subtasks_fifo")   ,

        arb_tx_fifo             ("arb_tx_fifo")             ,
        arb_data_fifo           ("arb_data_fifo")           ,
        arb_subtask_code_fifo   ("arb_subtask_code_fifo")   ,
        arb_request_fifo        ("arb_request_fifo")        ,
        arb_subtask_reference_fifo("arb_subtask_reference_fifo"),
        arb_subtask_fifo        ("arb_subtask_fifo")        ,
        arb_pending_subtasks_fifo("arb_pending_subtasks_fifo"),

//        data_address_stack      ("data_address_stack", DATA_OF),
        arb_data_address_stack      ("arb_data_address_stack"),
        core_status             ("core_status")             ,
        arb_core_status         ("arb_core_status")         ,
        ack_ok                  ("ack_ok")                  ,
        arb_ack_ok              ("arb_ack_ok")              ,
        core_return_type        ("core_return_type")        ,
        arb_core_return_type    ("arb_core_return_type")    ,
        current_subtask         ("current_subtask")         ,
        arb_current_subtask     ("arb_current_subtask")     ,
        n_args                  ("n_args")                  ,
        arb_n_args              ("arb_n_args"         )     ,
        opcode                  ("opcode")                  ,
        arb_opcode              ("arb_opcode"         )     ,
        code_status             ("code_status"  )           ,
        arb_code_status         ("arb_code_status")         ,
        symbol_table            ("symbol_table" )           ,
        arb_symbol_table        ("arb_symbol_table")        ,
        arg_addresses           ("arg_addresses")           ,
        arb_arg_addresses       ("arb_arg_addresses"),
        results_store           ("results_store")           ,
        service_core_ram        ("service_core_ram")        ,
//        request_table           ("request_table")           ,
        arb_register_set        ("arb_register_set")        ,
        arb_subtask_list        ("arb_subtask_list")        ,
        arb_data_store        ("arb_data_store")        ,
        arb_code_store        ("arb_code_store")
//        arb_request_table       ("arb_request_table")       ,
    {
        SC_THREAD(do_proc);
        //dont_initialize();
        // ----------------------------------
        // ************ Bindings ************
        // ----------------------------------

        // -- PROPAGATED EXPORTS -----
        xpwr_corestatus     .bind (arb_core_status.xpwr_master2);// propagate one export for access upwards to give access to service_core
        xp_results_store    .bind(results_store.xpwr_2);        // propagate one export for access (direct, without arbitration)
        xp_ack_ok           .bind(arb_ack_ok.xpwr_master2);     // propagate one export for access to ack_ok upwards for service_core access
        xp_core_return_type .bind(arb_core_return_type.xpwr_master2); // propagate one export for access to core_return_type upwards for service_core access
        xp_current_subtask  .bind(arb_current_subtask.xpwr_master2); //
        xp_n_args           .bind(arb_n_args.xpwr_master2);
        xp_opcode           .bind(arb_opcode.xpwr_master2);
        xp_arg_addresses    .bind(arb_arg_addresses.xpw_master1);
        xp_service_id		.bind(service_id.xpwr);	// prpagate up service_id's export, for service_core's access
        xp_symbol_table     .bind(arb_symbol_table.xpwr_master5);

        // -- route-throughs for ServiceCore
        xp_transceiver_tx_fifo.bind(arb_tx_fifo.xpw_master5);
        xp_code_store.bind(arb_code_store.xpwr_master4);
        xp_subtask_reference_fifo.bind( arb_subtask_reference_fifo.xpw_master4 );// write access to subtask_reference_fifo through arbiter


        // -- TRANSMIT_PACKETS --------------
        //transmit_packets.tx_fifo        ( arb_tx_fifo.xpw_master2 );
        //transmit_packets.tx_fifo            .bind( tx_fifo.xpwr_1 ); // to arbiter controlling read access to tx_fifo
        //transmit_packets.transceiver_tx_fifo .bind( transceiver_tx_fifo );    // propagate upwards, port for writing transceiver.tx_fifo

        // -- PARSE SULBTASK---------
        parse_subtask.data_fifo             .bind(arb_data_fifo.xpw_master1);// write access to data_fifo through arbiter
        parse_subtask.request_fifo          .bind(arb_request_fifo.xpw_master1);// write access to request_fifo through arbiter
        parse_subtask.subtask_code_fifo     .bind(arb_subtask_code_fifo.xpw_master1);// write access to subtask_code_fifo through arbiter
        parse_subtask.subtask_reference_fifo.bind(arb_subtask_reference_fifo.xpw_master1);// write access to subtask_reference_fifo through arbiter
        parse_subtask.data_address_stack    .bind(arb_data_address_stack.xpw_master1);// connect to data_address_stack via arbiter
        parse_subtask.tx_fifo       .bind(arb_tx_fifo.xpw_master3 );    // to arbiter controlling transceiver_tx_fifo
        parse_subtask.subtask_fifo  .bind(subtask_fifo.xpwr_1); // read access (direct) to subtask_fifo
        parse_subtask.register_set  .bind(arb_register_set.xpwr_master1); // to arbiter controlling register_set
        parse_subtask.code_store  .bind(arb_code_store.xpwr_master1 );  // to arbiter controlling code_store
        parse_subtask.data_store  .bind(arb_data_store.xpwr_master1 ) ; // connect port for access to data_memory, to the arbiter's export
        parse_subtask.subtask_list  .bind(arb_subtask_list.xpwr_master1); // connect to arbiter for access to subtask_list
        parse_subtask.symbol_table  .bind(arb_symbol_table.xpwr_master1);// access to symbol_table through arbiter
        parse_subtask.service       .bind(service.xpwr);        // access to the service register (reading only so no arbitration)

        parse_subtask.pending_subtasks_fifo .bind(arb_pending_subtasks_fifo.xpw_master2);// write access to pending_subtasks_fifo through arbiter

        // -- CORE_CONTROL ------------------
        core_control.tx_fifo                .bind( arb_tx_fifo.xpw_master1 );    // write access to transceiver_tx_fifo through arbiter
        core_control.data_fifo              .bind(arb_data_fifo.xpw_master3);   // write access to data_fifo through arbiter
        core_control.request_fifo           .bind(arb_request_fifo.xpw_master2);    // write access to request_fifo through arbiter
        core_control.subtask_code_fifo      .bind(arb_subtask_code_fifo.xpw_master3);   // write access to subtask_code_fifo through arbiter
        core_control.subtask_reference_fifo .bind(arb_subtask_reference_fifo.xpw_master2);    // write access to subtask_reference_fifo through arbiter
        core_control.subtask_fifo           .bind(arb_subtask_fifo.xpw_master3);  // write access to subtask_fifo through arbiter
        core_control.pending_subtasks_fifo  .bind(arb_pending_subtasks_fifo.xpw_master1);// write access to pending_subtasks_fifo through arbiter
        core_control.register_set           .bind(arb_register_set.xpwr_master2); // to arbiter controlling register_set
        core_control.subtask_list           .bind(arb_subtask_list.xpwr_master2); // connect to arbiter for access to subtask_list
        core_control.data_store           .bind(arb_data_store.xpwr_master2 ) ; // connect port for access to data_memory, to the arbiter's export
        core_control.core_status            .bind(arb_core_status.xpwr_master1);// to arbiter controlling core_status
        core_control.ack_ok                 .bind(arb_ack_ok.xpwr_master1);    // to arbiter for access to ack_ok register
        core_control.core_return_type       .bind(arb_core_return_type.xpwr_master1);// to arboter for access to core_return_type
        core_control.current_subtask        .bind(arb_current_subtask.xpwr_master1);  // to arbiter for access to current_subtask
        core_control.opcode                 .bind(arb_opcode.xpwr_master1);  // to arbiter for access to opcode
        core_control.n_args                 .bind(arb_n_args.xpwr_master1);  // to arbiter for access to n_args
        core_control.request_table          .bind(request_table_1);   // access to request_table through arbiter
        core_control.results_store          .bind(results_store.xpwr_1);        // access to the results_store table (Fifo access)
        core_control.service                .bind(service.xpwr);        // access to the service register (reading only so no arbitration)

        // following connections are needed for the clean_up() operation only?
        core_control.symbol_table       .bind(arb_symbol_table.xpwr_master3);   // access to symbol_table through arbiter
        core_control.data_address_stack .bind(arb_data_address_stack.xpw_master2);
        core_control.arg_addresses      .bind(arb_arg_addresses.xpw_master2);        // connect to arg_addresses

        // -- STORE DATA ------------
        store_data.data_store   .bind( arb_data_store.xpwr_master3) ;  // connect port for access to data_memory, to the arbiter's export
        store_data.register_set .bind( arb_register_set.xpwr_master3); // to arbiter controlling register_set
        store_data.subtask_list .bind( arb_subtask_list.xpwr_master5); // connect to arbiter for access to subtask_list
        store_data.data_fifo    .bind( arb_data_fifo.xpw_master2);  // give it read/write access to data_fifo through arbiter
        store_data.tx_fifo      .bind( arb_tx_fifo.xpw_master2 );    // write access to transceiver_tx_fifo through arbiter
        store_data.symbol_table .bind(arb_symbol_table.xpwr_master2);// access to symbol_table through arbiter
//        store_data.request_table.bind(arb_request_table.xpwr_master2);// access to request_table through arbiter
        store_data.pending_subtasks_fifo.bind(arb_pending_subtasks_fifo.xpw_master4);// write access to pending_subtasks_fifo through arbiter
        store_data.service      .bind(service);

        // -- RECEIVE_PACKETS ---------------
        receive_packets.transceiver_rx_fifo      .bind( transceiver_rx_fifo ); // propagate the port for reading from transceiver's rx_fifo
        receive_packets.data_fifo              .bind( arb_data_fifo             .xpw_master3);  // write access to data_fifo through arbiter
        receive_packets.subtask_code_fifo      .bind( arb_subtask_code_fifo   .xpw_master4 ); // write access to subtask_code_fifo through arbiter
        receive_packets.request_fifo           .bind( arb_request_fifo.xpw_master3); // write access to request_fifo through arbiter
        receive_packets.subtask_reference_fifo .bind( arb_subtask_reference_fifo.xpw_master3 );// write access to subtask_reference_fifo through arbiter
        receive_packets.service                .bind( service);

        // -- STORE_SUBTASK_CODE ------------
        store_subtask_code.subtask_list     .bind(arb_subtask_list.xpwr_master3);     // connect to arbiter for access to subtask_list
        store_subtask_code.subtask_code_fifo.bind( arb_subtask_code_fifo.xpw_master2);      // read/write access to subtask_code_fifo through arbiter
        store_subtask_code.subtask_fifo     .bind( arb_subtask_fifo.xpw_master2);     // write access to subtask_fifo through arbiter
        store_subtask_code.code_status      .bind(arb_code_status.xpwr_master1); // access to code_status (table) through arbiter
        store_subtask_code.code_store     .bind( arb_code_store.xpwr_master3 );     // to arbiter controlling code_store
        store_subtask_code.service          .bind(service);

        // -- ACTIVATE_SUBTASKE_CODE --------
        activate_subtask.subtask_list  .bind( arb_subtask_list.xpwr_master4);    // connect to arbiter for access to subtask_list
        activate_subtask.subtask_fifo  .bind( arb_subtask_fifo.xpw_master1);       // write access to subtask_fifo through arbiter
        activate_subtask.code_status   .bind(arb_code_status.xpwr_master2); // access to code_status (table) through arbiter
        activate_subtask.code_store  .bind( arb_code_store.xpwr_master2 );     // to arbiter controlling code_store
        activate_subtask.subtask_reference_fifo.bind(subtask_reference_fifo.xpwr_1);// read access to subtask_reference_fifo for activate_subtask
        activate_subtask.service       .bind(service);

        // -- PREPARE_SUBTASK ---------------
        //WV I think this is obsolete
//        prepare_subtask.subtask_list            .bind( arb_subtask_list.xpwr_master7);         // connect to arbiter for access to subtask_list
        //prepare_subtask.pending_subtasks_fifo   .bind( pending_subtasks_fifo.xpwr_1);// connect without arbiter, since only writer, to pending_subtasks_fifo
//        prepare_subtask.pending_subtasks_fifo   .bind(arb_pending_subtasks_fifo.xpw_master3);// write access to pending_subtasks_fifo through arbiter

        // -- DISPATCH_DATA_PACKETS ---------
        dispatch_data_packets.data_store  .bind( arb_data_store.xpwr_master4) ;  // connect port for access to data_memory, to the arbiter's export
        dispatch_data_packets.request_fifo  .bind( arb_request_fifo.xpw_master4); // read access to request_fifo for dispatch_data_packets
        dispatch_data_packets.data_fifo.bind(arb_data_fifo.xpw_master4);
        dispatch_data_packets.tx_fifo.bind(arb_tx_fifo.xpw_master4);
        dispatch_data_packets.subtask_fifo.bind(arb_subtask_fifo.xpw_master4);
        dispatch_data_packets.service  .bind(service); // read access to request_fifo for dispatch_data_packets
        dispatch_data_packets.register_set  .bind(arb_register_set.xpwr_master4);
        dispatch_data_packets.symbol_table  .bind(arb_symbol_table.xpwr_master4);
        dispatch_data_packets.request_table          .bind(request_table_2);
        dispatch_data_packets.lookup_table.bind(lookup_table);
        dispatch_data_packets.subtask_list.bind(arb_subtask_list.xpwr_master6);

        // -- ARBITERS ----------------------
        //arb_tx_fifo.pw_slave                .bind( tx_fifo.xpwr_1 );         //connect slave port of tx_fifo arbiter to tx_fifo
        arb_tx_fifo.pw_slave                .bind( transceiver_tx_fifo );     //propagate slave port of tx_fifo arbiter for connection to transceiver_tx_fifo
        arb_data_fifo.pw_slave              .bind( data_fifo.xpwr_1);        //connect slave port of data_fifo arbiter to data_fifo
        arb_request_fifo.pw_slave           .bind( request_fifo.xpwr_1);     //connect slave port of request_fifo arbiter to request_fifo
        arb_subtask_code_fifo.pw_slave      .bind( subtask_code_fifo.xpwr_1);//connect slave port of subtask_code_fifo arbiter to subtask_code_fifo
        arb_subtask_reference_fifo.pw_slave .bind( subtask_reference_fifo.xpwr_1);//connect slave port of subtask_reference_fifo arbiter to subtask_reference_fifo
        arb_subtask_fifo.pw_slave           .bind( subtask_fifo.xpwr_1);     //connect slave port of subtask_fifo arbiter to subtask_fifo
        arb_pending_subtasks_fifo.pw_slave  .bind(pending_subtasks_fifo.xpwr_1); //
        arb_register_set.pwr_slave          .bind( register_set ); // propagate the slave port of arbiter, for register_set access (register_set is outside service_manager
        arb_subtask_list.pwr_slave          .bind( subtask_list); // propagate the slave port of artbiter, for subtask_list access (which is outside tile)
        arb_data_store.pwr_slave          .bind( data_store);// propagate slave port of artbiter, for one port of the data_store
        arb_code_store.pwr_slave_1        .bind( code_store_1);// propagate the first slave port of artbiter, for dual-ported code_store access
        arb_code_store.pwr_slave_2        .bind( code_store_2);// propagate the second slave port of artbiter, for dual-ported code_storge access
        arb_core_status.pwr_slave           .bind( core_status.xpwr);// connect slave port of arbiter for status_register to status_register
        arb_ack_ok.pwr_slave                .bind( ack_ok.xpwr);     // connect slave port of arbiter for ack_ok to ack_ok
        arb_core_return_type.pwr_slave      .bind(core_return_type.xpwr);// connect slave port of arbiter to core_return_type
        arb_current_subtask .pwr_slave      .bind(current_subtask.xpwr); // connect to slave port of arbiter to current_subtask
        arb_opcode.pwr_slave                .bind(opcode         .xpwr); // connect to slave port of arbiter to opcode
        arb_n_args.pwr_slave                .bind(n_args         .xpwr); // connect to slave port of arbiter to n_args
        arb_symbol_table.pwr_slave          .bind(symbol_table.xpwr_1);  //symbol table's export connected to slave port of the corresponding arbiter
        arb_code_status.pwr_slave           .bind(code_status.xpwr_1);   //code_status (table) export connected to slave port of the corresponding arbiter
//        arb_request_table.pwr_slave         .bind(request_table.xpwr_1); //request table's export connected to slave port of the corresponding arbiter
        arb_arg_addresses.pw_slave          .bind(arg_addresses.xpwr_1); // the arbiter for arg_addresses is connected to its export
		arb_data_address_stack.pw_slave.bind(data_address_stack);
        // debug message... #ifdef used in the function; reduced clutter here
        const_debug_msg(name(),kind());
    }//Constructor
private:
};//class: SC_ServiceManager

//==============================================================================
//  DO_PROC()
//==============================================================================

template <typename ADDR_T, typename DATA_T>
void SC_ServiceManager<ADDR_T, DATA_T> :: do_proc()
{
	core_return_type=P_data;
}// funct: SC_ServiceManager :: do_proc()


}//namespace: SC_SBA

#endif /* SC_SERVICEMANAGER_H_ */

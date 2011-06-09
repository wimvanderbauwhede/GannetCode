/*
********************************************************************************
                 |
  File Name      | SC_Gateway.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        |26-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC "Gateway" Class; for use in the GatewayTile
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_GATEWAY_H_
#define SC_GATEWAY_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;


//==============================================================================
//  CLASS: GATEWAY
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC "Gateway" Class; for use in the GatewayTile
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_Gateway : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if     <Packet_t>          transceiver_rx_fifo;//!< read/write port for transceiver rx fifo
    port_SC_Fifo_if     <Packet_t>          transceiver_tx_fifo;//!< read/write port for transceiver tx fifo
    port_SC_Memory_if   <ADDR_T, DATA_T>    result_store;    //!< for accessing result_store in the tile
    port_SC_reg_if		<uint	>       	io_mech;

    //port_SC_Memory_if   <ADDR_T, DATA_T>    result_store_2;    //!< for accessing 2nd result_store in the tile
    //port_SC_Memory_if   <ADDR_T, DATA_T>    data_packet_store;//!< read/write port for accessing data_packet_store in the tile
    //port_SC_reg_if      <bool>              finished;       //!<

    // ---------------------------- EXPORTS ------------------------------------

    // ---------------------------- Sub-Modules --------------------------------
//    SC_receive_packets_gw                        receive_packets;       //!< ...

    // Re-use the transmit_packets module from ServiceManager. If distinct functionality is
    // encountered, then use the SC_transmit_packets_gw (currently redundant) class
    // to customize transmit_packets for the Gateway
//    SC_transmit_packets                         transmit_packets;      //!< ...
    //SC_transmit_packets_gw     <PACKET_T>                  transmit_packets;      //!< ...

    SC_parse_task_description_gw<ADDR_T, DATA_T>  parse_task_description;
    SC_store_result_packets_gw  <ADDR_T, DATA_T>  store_result_packets;

    // ---------------------------- Data Containers , Registers, Arbiters -------

    SC_Register         <SBA::Service >             service;        //!<
    SC_Register         <SBA::ServiceAddress>       address;        //!<
    SBA::TaskDescList                               tasks;

    SC_Stack            <MAX_NTASKS>                tasks_stack;    //!<
    SC_Register         <SBA::Core_Status>          core_status;    //!< SC_Register of enumerated type for storing core_status
    //SC_Register         <SBA::Counter>              task_counter;   //!<
    //SC_Deque            <SBA::StringPair>           tasks;          //!< tasks is of type TaskDescList in the C++ model

    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  rx_fifo;          //!< ...
    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  tx_fifo;            //!< ...

    //!< WN: TODO: Redundant? Probably since only used by receive_packets, when DATA == 1, which we are ignoring for SC model.
    SC_Fifo             <Packet_t, PACKET_FIFO_SZ>  request_fifo;


    SC_Register_Arbiter       <SBA::Core_Status>          arb_core_status;    //!< Arbiter managing contention on core_status
//    SC_Fifo_Arbiter      <Packet_t>                  arb_rx_fifo;      //!< write contention on rx_fifo
    SC_Fifo_Arbiter      <Packet_t>                  arb_request_fifo;   //!< write contention on request_fifo
    SC_Stack_Arbiter                                 arb_tasks_stack;    //!< contention on tasks_stack

    //SC_Register_Arbiter       <SBA::Counter>              arb_task_counter;   //!< Redundant? Check.
    //SC_Fifo_Arbiter      <PACKET_T>                  arb_tx_fifo;        //!< write contention on tx_fifo
    //SC_Register_Arbiter       <bool>                      arb_finished;       //!< contention on port for accessing "finished" boolean in the tile.
    //SC_Memory_Arbiter       <ADDR_T, DATA_T>            arb_result_store; //!< contention on port for accessing "result_store" in the tile
    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Gateway"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Gateway);

    SC_Gateway( sc_module_name      nm_ ,
                SBA::Service        s_  ,
                SBA::ServiceAddress a_  ,
                SBA::TaskDescList&  tds_  ) :
        sc_module               (nm_),
//        receive_packets         ("receive_packets")         ,
//        transmit_packets        ("transmit_packets")        ,
        parse_task_description  ("parse_task_description",tds_ ),
        store_result_packets    ("store_result_packets")    ,
        service                 ("service", s_)             ,
        address                 ("address", a_)             ,
        tasks                   (tds_),
        tasks_stack             ("tasks_stack",0)           ,
        core_status             ("core_status")             ,
        rx_fifo                 ("rx_fifo")                 ,
        tx_fifo                 ("tx_fifo")                 ,
        request_fifo            ("request_fifo")            ,
//        arb_rx_fifo             ("arb_rx_fifo")             ,
        arb_core_status         ("arb_core_status")         ,
        arb_request_fifo        ("arb_request_fifo")        ,
        arb_tasks_stack         ("arb_tasks_stack")
        //arb_task_counter        ("arb_task_counter")        ,
        //arb_tx_fifo             ("arb_tx_fifo")             ,
        //arb_finished            ("arb_finished")
        //arb_result_store      ("arb_result_store")
        //task_counter            ("task_counter")            ,
        //tasks                   ("tasks")                   ,

    {
        SC_THREAD(do_proc);
        //dont_initialize();

        // ----------------------------------
        // ************ Bindings ************
        // ----------------------------------
/*
        // RECEIVE PACKETS
        // ---------------
        receive_packets.transceiver_rx_fifo  .bind(transceiver_rx_fifo);          // propagate up and out for connection to transceiver
        receive_packets.rx_fifo             .bind(arb_rx_fifo.xpw_master1);   // connect to rx_fifo through arbiter

        // TODO: See if arbiter is needed to connect to request_fifo. Apparently only one master
        // but connecting through arbiter just in case. REmove arbiter and connect directly
        // (uncomment the second like and comment the first one)
        receive_packets.request_fifo        .bind(arb_request_fifo.xpw_master1);//
        //receive_packets.request_fifo        .bind(request_fifo);//

        // TRANSMIT PACKETS - FIXME: OBSOLETE  - Direct connection to transceiver now
        // ----------------
        transmit_packets.transceiver_tx_fifo .bind(transceiver_tx_fifo);      // propagate up and out for connection to transceiver

        // TODO: Is an arbiter redundant here? B/c if this only reading, and the one other master
        // parse_task_Descriptiononly writing, then no need of arbiter
        //transmit_packets.tx_fifo            .bind(arb_tx_fifo.xpw_master1); // connect to local tx_fifo through arbiter
        transmit_packets.tx_fifo            .bind(tx_fifo.xpwr_1); // connect to local tx_fifo directly
*/
        // PARSE TASK DESCRIPTION
        // ----------------------
        parse_task_description.result_store   .bind(result_store); //!< propagate up and out to seek connection
        parse_task_description.tasks_stack      .bind(arb_tasks_stack.xpw_master1);//!<
        //parse_task_description.tx_fifo          .bind(tx_fifo.xpwr_2); // connect to local tx_fifo directly
        parse_task_description.tx_fifo          .bind(transceiver_tx_fifo); // connect to local tx_fifo directly
        parse_task_description.core_status      .bind(arb_core_status.xpwr_master1);//!<
        parse_task_description.io_mech          .bind(io_mech);//propagate upwards
        //TODO: See if any other master for task_counter. Apparently not, but still
        // connecting through arbiter just in case. Remove arbiter and connect directly if sure not needed
        // (uncomment the second like and comment the first one)
        //parse_task_description.task_counter     .bind(arb_task_counter.xpwr_master1);    //!<
        //parse_task_description.task_counter     .bind(task_counter.xpwr);    //!<

        // Is arbiter for tx_fifo redundant? See TODO for connection to this arbiter for transmit_packets module
        //parse_task_description.tx_fifo          .bind(arb_tx_fifo.xpw_master2);     //!<

        //parse_task_description.tasks            .bind(tasks.xpwr_1);         //!< connect to local "tasks" TaskDescList directly (no master)
        //parse_task_description.finished         .bind(arb_finished.xpwr_master1);   //!<

        // STORE RESULT PACKETS
        // --------------------
        store_result_packets.tasks_stack      .bind(arb_tasks_stack.xpw_master2);//!<
//        store_result_packets.rx_fifo          .bind(arb_rx_fifo.xpw_master2);     //!<
        store_result_packets.rx_fifo          .bind(transceiver_rx_fifo);     //!<
        store_result_packets.core_status      .bind(arb_core_status.xpwr_master2);//!<
        store_result_packets.io_mech      	  .bind(io_mech);//!< propagate upwards
        store_result_packets.service          .bind(service);
        //store_result_packets.result_store   .bind(result_store_2);  //!< propagate up and out to seek connection
        //store_result_packets.finished         .bind(arb_finished.xpwr_master2);   //!<

        // ARBITERS
        // --------
        arb_core_status     .pwr_slave.bind(core_status.xpwr);
//        arb_rx_fifo         .pw_slave.bind(rx_fifo.xpwr_1);
        arb_request_fifo    .pw_slave.bind(request_fifo.xpwr_1);
        arb_tasks_stack     .pw_slave.bind(tasks_stack.xpwr_1);
        //arb_task_counter    .pwr_slave.bind(task_counter.xpwr);
        //arb_tx_fifo         .pw_slave.bind(tx_fifo.xpwr_1);
        //arb_finished        .pwr_slave.bind(finished); // propagate up and out via port
        //arb_result_store  .pwr_slave.bind(result_store); // propagate up and out via port
    }//constructor

private:

};//class: SC_Gateway

//==============================================================================
//  DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_Gateway<ADDR_T, DATA_T> :: do_proc()
{

}//do_proc()

}//namespace: SC_SBA

#endif /* SC_GATEWAY_H_ */

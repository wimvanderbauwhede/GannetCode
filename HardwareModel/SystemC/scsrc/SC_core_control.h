/*
********************************************************************************
                 |
  File Name      | SC_core_control.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Service Core class (MULTITHREAD==0)
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081029: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_CORE_CONTROL_H_
#define SC_CORE_CONTROL_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: SERVICE MANAGER
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This is core control for single-threaded tile's manager
/*!
   Detailed description here...
*/
template <typename ADDR_T, typename DATA_T>
class SC_core_control :
            public sc_module {
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;   //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T, RegisterEntry>register_set;   //!< write/read port for data memory access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
    port_SC_Fifo_if     <Packet_t>          tx_fifo;            //!< write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;          //!< write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          request_fifo;       //!< write port to access request_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< write port to access subtask_code_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_reference_fifo;//!< write port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;       //!< write port to access subtask_fifo
    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< write port for access to pending_subtasks_fifo
    port_SC_Deque_if    <Word>              results_store;      //!< Fifo access to the results store
    port_SC_reg_if      <bool>              ack_ok;             //!< access to ack_ok boolean
    port_SC_reg_if      <Packet_Type>       core_return_type;   //!< ...
    port_SC_reg_if      <Subtask     >      current_subtask;
    port_SC_reg_if      <uint             > n_args;
    port_SC_reg_if      <uint             > opcode;
    port_SC_reg_if      <SBA::Core_Status>  core_status;    //!< write/read port for code_status access
    port_SC_RequestTable_if  request_table;  //!< port for access to request_table
    // following access for clean_up() only
    port_SC_Memory_if   <ADDR_T, SBA::Word> symbol_table;   //!< port for access to symbol_table
    // following access for clean_up() only
    port_SC_Stack_if                        data_address_stack;//!< w/r port for data_address_stack
    port_SC_Deque_if    <SBA::MemAddress>   arg_addresses;  //!< read write port for arg_addresses memory
    port_SC_reg_if      <SBA::Service>      service;        //!< for reading service id

    // ---------------------------- LOCALS -------------------------------------
    uint scid;  // Should this be an SC_Register in the service_manager, accessed through the port?
    const bool debug_all_;
    const int debug_service_;
    // ---------------------------- METHODS ------------------------------------
    void                do_proc(); // the SC_thread


    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const {
        return "SC_core_control";
    }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_core_control);
    SC_core_control(sc_module_name nm) : sc_module(nm), debug_all_(true),debug_service_(0) {
        SC_THREAD(do_proc);
        //dont_initialize();
        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
    //Helper methods used in do_proc() thread
    SBA::MemAddresses   build_value_address_list(); // for initializing arg_addresses
    void                send_ack();
    void                clean_up();
	void demux_packets_by_type(Packet_t&);
}
;//class: SC_core_control

//==============================================================================
// BUILD_VALUE_ADDRESS_LIST()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
SBA::MemAddresses SC_core_control<ADDR_T, DATA_T> :: build_value_address_list() {
#include "../gensrc/ServiceManager/build_value_address_list.cc"
} //build_value_address_list()

//==============================================================================
// SEND_ACK()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T> :: send_ack() {
#include "../gensrc/ServiceManager/send_ack.cc"
}//send_ack()

//==============================================================================
// CLEAN_UP()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T>:: clean_up() {
#include "../gensrc/ServiceManager/clean_up.cc"
}//clean_up()
//==============================================================================
// demux_packets_by_type()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T> :: demux_packets_by_type(Packet_t& packet) {
#include "../gensrc/ServiceManager/demux_packets_by_type.cc"
} // funct: SC_core_control :: demux_packets_by_type()

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T> :: do_proc() {
#include "../gensrc/ServiceManager/core_control.cc"
}//SC_core_control :: do_proc()

} //namespace: SC_SBA
#endif /* SC_CORE_CONTROL_H_ */

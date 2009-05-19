/*
********************************************************************************
                 |
  File Name      | SC_dispatch_data_packets.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        |  04-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'SC_dispatch_data_packets' class
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************
*/
// * ***** BEGIN LICENSE BLOCK *****
// * Version: AFL 2.1
// *
// * The contents of this file are subject to the Academic Free License Version
// * 2.1 (the "License") you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://opensource.org/licenses/afl-2.1.php
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// *  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
// *
// * ***** END LICENSE BLOCK ***** */


#ifndef SC_DISPATCH_DATA_PACKETS_H_
#define SC_DISPATCH_DATA_PACKETS_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//  CLASS: DISPATCH DATA PACKETS
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the SC_dispatch_data_packets module
    /*!
       Dispatch data packets in answer to a request. Essential for STREAM and CACHE/ACC
    */
template <typename ADDR_T, typename DATA_T>
class SC_dispatch_data_packets :
    public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------

    port_SC_Fifo_if     <Packet_t>          tx_fifo;        //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;      //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          request_fifo;   //!< write port to access request_fifo
//    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< write port to access subtask_code_fifo
//    port_SC_Fifo_if     <Packet_t>          subtask_reference_fifo;//!< write port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;   //!< read port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;   //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T,RegisterEntry> register_set;   //!< write/read port for register-set access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
//    port_SC_Stack_if                        data_address_stack;//!< w/r port for data_address_stack
//    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store;   //!< spec. w/r port for access to code_store (read-only access needed actually)
    port_SC_Memory_if   <ADDR_T, Word>      symbol_table;   //!< port for access to symbol_table
    port_SC_reg_if      <Service>           service;        //!< for reading service id
//    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< write port for access to pending_subtasks_fifo

	port_SC_LookupTable_if		lookup_table;// WV: not implemented at all!
    port_SC_RequestTable_if		request_table;
    // ---------------------------- LOCALS -------------------------------------

    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    void restart_subtask(MemAddress regaddr);
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_dispatch_data_packets"; }
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_dispatch_data_packets);
    SC_dispatch_data_packets(sc_module_name nm) : sc_module(nm)
    {
        SC_THREAD(do_proc);
        //dont_initialize();
        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
};//class: SC_dispatch_data_packets

//==============================================================================
//  DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_dispatch_data_packets <ADDR_T, DATA_T>:: do_proc()
{
#include "../gensrc/ServiceManager/dispatch_data_packets.cc"
}// funct: SC_dispatch_data_packets :: do_proc()



template <typename ADDR_T, typename DATA_T> void SC_dispatch_data_packets <ADDR_T, DATA_T>:: restart_subtask(MemAddress regaddr) {
#include "../gensrc/ServiceManager/restart_subtask.cc"
} // funct: SC_dispatch_data_packets :: restart_subtask



} //namespace: SC_SBA


#endif /* SC_DISPATCH_DATA_PACKETS_H_ */

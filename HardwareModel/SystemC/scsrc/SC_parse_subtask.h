/*
********************************************************************************
                 |
  File Name      | SC_parse_subtask.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'parse_subtask_packets' class
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
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


#ifndef SC_PARSE_SUBTASK_H_
#define SC_PARSE_SUBTASK_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
// DEFS
//------------------------------------------------------------------------------



//==============================================================================
//	CLASS: PARSE SUBTASK
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This is the "SC_parse_subtask" module
/*!
   Detailed description here...
*/
template <typename ADDR_T, typename DATA_T>
class SC_parse_subtask : public sc_module {
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if     <Packet_t>          tx_fifo;        //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;      //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          request_fifo;   //!< write port to access request_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< write port to access subtask_code_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_reference_fifo;//!< write port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;   //!< read port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;   //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T,RegisterEntry> register_set;   //!< write/read port for register-set access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
    port_SC_Stack_if                        data_address_stack;//!< w/r port for data_address_stack
    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store;   //!< spec. w/r port for access to code_store (read-only access needed actually)
    port_SC_Memory_if   <ADDR_T, Word>      symbol_table;   //!< port for access to symbol_table
    port_SC_reg_if      <Service>           service;        //!< for reading service id
    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< write port for access to pending_subtasks_fifo


    // ---------------------------- METHODS ------------------------------------
    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const {
        return "SC_parse_subtask_packets";
    }

    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_parse_subtask);

    SC_parse_subtask(sc_module_name nm) : sc_module(nm) {
        SC_THREAD(do_proc);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
	void demux_packets_by_type(Packet_t&);
}
;//class: SC_parse_subtask

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_parse_subtask<ADDR_T, DATA_T> :: do_proc() {
#include "../gensrc/ServiceManager/parse_subtask.cc"
}// funct: SC_parse_subtask :: do_proc()
//==============================================================================
// demux_packets_by_type()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_parse_subtask<ADDR_T, DATA_T> :: demux_packets_by_type(Packet_t& packet) {
#include "../gensrc/ServiceManager/demux_packets_by_type.cc"
} // funct: SC_parse_subtask :: demux_packets_by_type()

} //namespace: SC_SBA


#endif /* SC_PARSE_SUBTASK_H_ */




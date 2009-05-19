/*
********************************************************************************
                 |
  File Name      | SC_parse_task_description_gw.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 26-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'parse_task_description' class for GatewayTile
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

#ifndef SC_PARSE_TASK_DESCRIPTION_GW_H_
#define SC_PARSE_TASK_DESCRIPTION_GW_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;


//==============================================================================
//  CLASS: PARSE TASK DESCRIPTION (GATEWAY)
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC 'parse_task_description' class for Gateway
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_parse_task_description_gw : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Memory_if   <ADDR_T, DATA_T>    result_store;//!< read/write port for accessing result_store in the tile
    port_SC_Stack_if                        tasks_stack;    //!<
    port_SC_Fifo_if     <Packet_t>          tx_fifo;        //!<
    port_SC_reg_if      <SBA::Core_Status>  core_status;    //!<
    port_SC_reg_if      <uint	>           io_mech;

    //port_SC_reg_if      <bool>              finished;       //!<
    //port_SC_Deque_if    <SBA::StringPair>   tasks;          //!<
    //port_SC_reg_if      <SBA::Counter>      task_counter;   //!<
	//Interface vmif

    // ---------------------------- SUB-MODULES --------------------------------
	SC_Interface			                vmif; // why make it an SC type at all?

    // ---------------------------- METHODS ------------------------------------
    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_parse_task_description_gw"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_parse_task_description_gw);

    SC_parse_task_description_gw(   sc_module_name nm_  ,
                                    TaskDescList& tds_  ) :
        sc_module(nm_)   ,
        vmif	("vmif", tds_)
    {
        // Bindings
        vmif.io_mech    .bind(io_mech); // propagate upwards


        SC_THREAD(do_proc);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
};//class: SC_parse_task_description_gw

//==============================================================================
//  DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_parse_task_description_gw<ADDR_T, DATA_T>:: do_proc() {
#include "../gensrc/Gateway/parse_task_description.cc"
/*

	uint task_id=0;
    uint ntdcs=vmif.receive(core_status);
    if (ntdcs!=0 and tasks_stack.size()>0 ){
        core_status = CS_busy;
        uint task_id=tasks_stack.pop();
        vmif.iodescs[task_id]=ntdcs;
        Bytecode tdc=vmif.tdcs.shift();

        TaskDescription task_description(tdc,task_id);
        Packet_List                 task_description_packet_list=task_description.Packets;
        Word_List nullwl;
        nullwl.push_back((Word)0);
        result_store.mput(task_id,nullwl);

        for(Packet_List::reverse_iterator iter_=task_description_packet_list.rbegin();iter_!=task_description_packet_list.rend();iter_++) {
        	Packet_t task_description_packet=*iter_;
#ifdef VERBOSE
        	cout << "GATEWAY SENDS PACKET:\n";
        	cout << ppPacket(task_description_packet) << endl;
//        	cout << "SC_parse_task_description_gw: pushing packet on tx_fifo\n";
#endif // VERBOSE
            tx_fifo.push(task_description_packet);
        }
    }
*/
}// funct: SC_parse_task_description_gw :: do_proc()

} //namespace: SC_SBA


#endif /* SC_PARSE_TASK_DESCRIPTION_GW_H_ */

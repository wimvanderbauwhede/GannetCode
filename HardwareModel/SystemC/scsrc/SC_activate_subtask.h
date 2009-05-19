/*
********************************************************************************
                 |
  File Name      | SC_activate_subtask.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'activate_subtask_code' class
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


#ifndef SC_ACTIVATE_SUBTASK_H_
#define SC_ACTIVATE_SUBTASK_H_



//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: ACTIVATE SUBTASK
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "SC_activate_subtask" module
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_activate_subtask : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    port_SC_SubtaskList_if              subtask_list;   //!< w/r port for subtask_list access
    port_SC_Fifo_if     <Packet_t>      subtask_reference_fifo;  //!< read port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>          subtask_fifo; //!< write port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, uint>  code_status;    //!< port for access to code_status (table)
    port_SC_Memory_if   <ADDR_T, DATA_T>code_store;   //!< write/read port for code_store
    port_SC_reg_if      <SBA::Service >     service;

    //sc_port<SC_fifo_delayed_get_if<PACKET_T> >  pr_ref_fifo_d;  //!< read port to access subtask_reference_fifo
    //sc_port<SC_fifo_delayed_put_if<PACKET_T> >  pw_staskfifo_d; //!< write port to access subtask_fifo

	// ---------------------------- METHODS ------------------------------------
    void do_proc(); // the thread
    void activate_subtask_helper( CodeAddress task_address, Name_t tservice_id, Word_List& packet);

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_activate_subtask_code"; }
	// ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_activate_subtask);
	SC_activate_subtask(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
	}
private:
    void activate_subtask_helper(CodeAddress task_address,Name_t service_id,Word_List& packet,bool is_subtask_packet);

};//class: SC_activate_subtask






//==============================================================================
//  ACTIVATE_SUBTASK_HELPER()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_activate_subtask <ADDR_T, DATA_T>::activate_subtask_helper( CodeAddress task_address,
                                                                    Name_t      tservice_id,
                                                                    Word_List& packet, bool is_subtask_packet)
{
#include "../gensrc/ServiceManager/activate_subtask_helper.cc"
}//activate_subtask()

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_activate_subtask <ADDR_T, DATA_T> :: do_proc()
{
#include "../gensrc/ServiceManager/activate_subtask.cc"
}// funct: SC_activate_subtask :: do_proc()

} //namespace: SC_SBA

#endif /* SC_ACTIVATE_SUBTASK_H_ */





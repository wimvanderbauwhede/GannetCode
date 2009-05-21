/*
********************************************************************************
                 |
  File Name      | SC_prepare_subtask.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'prepare_subtask' class
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */


#ifndef SC_PREPARE_SUBTASK_H_
#define SC_PREPARE_SUBTASK_H_


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
//	CLASS: PREPARE SUBTASK
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "prepare subtask" module
    /*!
       Detailed description here...
    */
class SC_prepare_subtask : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if<Subtask>    pending_subtasks_fifo;   //!< write port for access to pending_subtasks_fifo
    port_SC_SubtaskList_if      subtask_list;       //!< w/r port for subtask_list access

	// ---------------------------- METHODS ------------------------------------
	void do_proc();

	// overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_prepare_subtask"; }

	// ---------------------------- CONSTRUCTOR --------------------------------

	SC_HAS_PROCESS(SC_prepare_subtask);

	SC_prepare_subtask(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);
        // creation debug message..
        const_debug_msg(name(),kind());
	}
private:
};//class: SC_prepare_subtask

//==============================================================================
//	DO_PROC()
//==============================================================================
void SC_prepare_subtask:: do_proc()
{
}// funct: SC_prepare_subtask :: do_proc()


} //namespace: SC_SBA


#endif /* SC_PREPARE_SUBTASK_H_ */

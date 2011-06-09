/*
********************************************************************************
                 |
  File Name      | SC_Subtask_List.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 02-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC class for Subtask_List
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/

#ifndef SC_SUBTASK_LIST_H_
#define SC_SUBTASK_LIST_H_


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
// the waiting time for accessing a SC_SubtaskList_if methods, a single value
// for all methods for now, just to check arbitration.
//#define _STL_WAIT_ wait(10, SC_NS)


//==============================================================================
//  CLASS: SC Subtask List
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This the SC model of Subtask_List
    /*!
       Currently simply a wrapper around an SBA::Subtask_List object
    */
class SC_Subtask_List :
    public sc_module    ,
    public SC_SubtaskList_if
{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_SubtaskList_if > xpwr_1;
    sc_export<SC_SubtaskList_if > xpwr_2;

    // ---------------------------- Sub-Modules --------------------------------
    SBA::Subtask_List my_stlist;//!< The local SBA::Subtask_List object that contains the memory element
                                //!< and also implements all the access methods as defined in SC_SubtaskList_if

    // ---------------------------- METHODS ------------------------------------
    // The following SC_SubtaskList_if interface methods are implemented in this module and exported
    // They correspond to the access methods of the SBA::Subtask_List class.
    void lock () {} // WV: should these be here?
    void unlock () {}
    void add (const Subtask subtask) {
		_STLIST_WRITE_DLY; // must at least reset status
		my_stlist.add              (subtask);
    }
    void remove(const Subtask subtask) {
    	//_STLIST_WRITE_DLY;  // is a no-op in reality
    	my_stlist.remove(subtask);
    }
    Subtask_Argument_List&  arguments           (const Subtask subtask)                             {
    //_STLIST_ARG_DLY;
    wait((my_stlist.arguments (subtask).size()>>1) * _CLK_P, _CLK_U); // >>1 because 2 args per Word
    return(my_stlist.arguments (subtask) );
    }
    void add_arg (const Subtask subtask,MemAddress address) {
        // pushing an arg means read + write because we need to read the location to push to
        wait(2 * _CLK_P, _CLK_U) ;
        my_stlist.arguments(subtask).push_back(address);
    }

    Subtask_Status          status              (const Subtask subtask)                             {
#ifdef VERBOSE
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
                << "Trying to get status for " << subtask << endl;
#endif
    	_STLIST_READ_DLY; return(my_stlist.status    (subtask) );
    	}
    void                    status              (const Subtask subtask,Subtask_Status status_)      {_STLIST_READ_DLY; my_stlist.status           (subtask, status_); }
    Word                    return_as           (const Subtask subtask)                             {_STLIST_READ_DLY; return(my_stlist.return_as (subtask) ); }
    void                    return_as           (const Subtask subtask,Word return_as_)             {_STLIST_READ_DLY; my_stlist.return_as        (subtask, return_as_); }
    Symbol_t                called_as           (const Subtask subtask)                             { return(my_stlist.called_as (subtask) ); }
    void                    called_as           (const Subtask subtask,Symbol_t called_as)          { my_stlist.called_as        (subtask, called_as); }
    Service                 to                  (const Subtask subtask)                             {_STLIST_READ_DLY; return(my_stlist.to        (subtask) ); }
    void                    to                  (const Subtask subtask, Service to)                 {_STLIST_READ_DLY; my_stlist.to               (subtask, to); }
    Service                 return_to           (const Subtask subtask)                             {; return(my_stlist.return_to (subtask) ); }
    void                    return_to           (const Subtask subtask, Service return_to)          {; my_stlist.return_to        (subtask, return_to); }
    Word                    ack_to              (const Subtask subtask)                             {_STLIST_READ_DLY; return(my_stlist.ack_to    (subtask) ); }
    void                    ack_to              (const Subtask subtask, Word ack_to)                {_STLIST_READ_DLY; my_stlist.ack_to           (subtask, ack_to); }
    uint                    redir               (const Subtask subtask)                             {; return(my_stlist.redir     (subtask) ); }
    void                    redir               (const Subtask subtask, uint redir)                 {; my_stlist.redir            (subtask, redir); }
    uint                    waiting_for_ack     (const Subtask subtask)                             {_STLIST_READ_DLY; return(my_stlist.waiting_for_ack(subtask)); }
    void                    waiting_for_ack     (const Subtask subtask, uint waiting_for_ack)       {_STLIST_READ_DLY; my_stlist.waiting_for_ack  (subtask, waiting_for_ack); }
    CodeAddress             code_address        (const Subtask subtask)                             {; return(my_stlist.code_address(subtask)); }
    void                    code_address        (const Subtask subtask, CodeAddress code_address)   {; my_stlist.code_address     (subtask, code_address); }
    uint                    service_id          (const Subtask subtask)                             {; return(my_stlist.service_id(subtask) ); }
    void                    service_id          (const Subtask subtask, uint service_id)            {; my_stlist.service_id       (subtask, service_id); }
    uint                    nargs               (const Subtask subtask)                             {_STLIST_READ_DLY; return(my_stlist.nargs     (subtask) ); }
    void                    nargs               (const Subtask subtask, uint val)                   {_STLIST_READ_DLY; my_stlist.nargs            (subtask, val); }
    uint                    nargs_absent        (const Subtask subtask)                             {; return(my_stlist.nargs_absent(subtask) ); }
    void                    nargs_absent        (const Subtask subtask, uint val)                   {; my_stlist.nargs_absent     (subtask, val); }
    void                    decr_nargs_absent   (const Subtask subtask)                             {_STLIST_READ_DLY; my_stlist.decr_nargs_absent(subtask); }
    void                    incr_nargs_absent   (const Subtask subtask)                             {_STLIST_READ_DLY; my_stlist.incr_nargs_absent(subtask); }
    uint                    mode                (const Subtask subtask)                             {; return(my_stlist.mode      (subtask) ); }
    void                    mode                (const Subtask subtask, uint val)                   {; my_stlist.mode             (subtask, val); }
    uint                    reg                 (const Subtask subtask)                             {; return(my_stlist.reg       (subtask) ); }
    void                    reg                 (const Subtask subtask, uint val)                   {; my_stlist.reg              (subtask, val); }

    uint                    offset                 (const Subtask subtask)                             {; return(my_stlist.offset       (subtask) ); }
	void                    offset                 (const Subtask subtask, uint val)                   {; my_stlist.offset              (subtask, val); }
	uint                    fsize                 (const Subtask subtask)                             {; return(my_stlist.fsize       (subtask) ); }
	void                    fsize                 (const Subtask subtask, uint val)                   {; my_stlist.fsize              (subtask, val); }

    Subtasks                subtasks            ()                                                  {; return(my_stlist.subtasks  () ); }
    Subtask_List_Item       entry               (const Subtask subtask)                             {; return(my_stlist.entry     (subtask) ); }

    virtual const char* kind() const
        { return "SC_Subtask_List"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Subtask_List);
    SC_Subtask_List(sc_module_name nm) :
        sc_module(nm)
    {
        // ************ Instantiaing ************

        // ************ Bindings ************
        // Bind exports to *this, which is a hierarchichal channel that implements the SC_SubtaskList_if interface
        xpwr_1.bind(*this);
        xpwr_2.bind(*this);

        // creation debug message..
        const_debug_msg(name(),kind());
    }
};/* class: SC_Subtask_List */

} /* namespace: SC_SBA */

#endif /* SC_SUBTASK_LIST_H_ */

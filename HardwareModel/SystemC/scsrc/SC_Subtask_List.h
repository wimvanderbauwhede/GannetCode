/*
********************************************************************************
                 |
  File Name      | SC_Subtask_List.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 02-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC class for Subtask_List
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

#ifndef SC_SUBTASK_LIST_H_
#define SC_SUBTASK_LIST_H_


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
    void lock () {}
    void unlock () {}
    void                    add                 (const Subtask subtask)                             {
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

    Subtask_Status          status              (const Subtask subtask)                             {_STLIST_READ_DLY; return(my_stlist.status    (subtask) ); }
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

/*
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: add (const Subtask subtask) {my_stlist.add(subtask)}

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: remove (const Subtask subtask) {
#ifndef STATIC_ALLOC
        subtasks_list.erase(subtask);
#else
        occ[subtask]=false;
#endif
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

    Subtask_Argument_List& SC_Subtask_List :: arguments(const Subtask subtask) { // but not const
        return subtasks_list[subtask].arguments;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    Subtask_Status SC_Subtask_List :: status (const Subtask subtask) {
        return subtasks_list[subtask].status;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

    void SC_Subtask_List :: status (const Subtask subtask,Subtask_Status status_) {
        subtasks_list[subtask].status=status_;
#ifdef VERBOSE
        //cout << "Subtask_List: Set status for "<<subtask<<" to "<<status_<<":"<<subtasks_list[subtask].status<<"\n";
#endif // VERBOSE
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    Word SC_Subtask_List :: return_as (const Subtask subtask) {
        return subtasks_list[subtask].return_as;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: return_as (const Subtask subtask,Word return_as_) {
        subtasks_list[subtask].return_as=return_as_;
#ifdef VERBOSE
//      cout << "Subtask_List: Set return_as for "<<subtask<<" to "<<return_as_<<":"<<subtasks_list[subtask].return_as<<"\n";
#endif // VERBOSE
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
Symbol_t SC_Subtask_List :: called_as(const Subtask subtask)
{
    return subtasks_list[subtask].called_as;
}
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: called_as(const Subtask subtask,Symbol_t called_as) {
        subtasks_list[subtask].called_as=called_as;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    Service SC_Subtask_List :: to(const Subtask subtask) {
        return subtasks_list[subtask].to;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: to(const Subtask subtask, Service to) {
        subtasks_list[subtask].to=to;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    Service SC_Subtask_List :: return_to(const Subtask subtask) {
        return subtasks_list[subtask].return_to;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: return_to(const Subtask subtask, Service return_to) {
        subtasks_list[subtask].return_to=return_to;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    Word SC_Subtask_List :: ack_to(const Subtask subtask) {
        return subtasks_list[subtask].ack_to;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: ack_to(const Subtask subtask, Word ack_to) {
        subtasks_list[subtask].ack_to=ack_to;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    uint SC_Subtask_List :: redir(const Subtask subtask) {
        return subtasks_list[subtask].redir;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: redir(const Subtask subtask, uint redir) {
        subtasks_list[subtask].redir=redir;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    uint SC_Subtask_List :: waiting_for_ack(const Subtask subtask) {
        return subtasks_list[subtask].waiting_for_ack;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: waiting_for_ack(const Subtask subtask, uint waiting_for_ack) {
        subtasks_list[subtask].waiting_for_ack=waiting_for_ack;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    CodeAddress SC_Subtask_List :: code_address(const Subtask subtask) {
        return subtasks_list[subtask].code_address;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: code_address(const Subtask subtask, CodeAddress code_address) {
        subtasks_list[subtask].code_address=code_address;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    uint SC_Subtask_List :: service_id(const Subtask subtask) {
        return subtasks_list[subtask].service_id;
    }
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    void SC_Subtask_List :: service_id(const Subtask subtask, uint service_id) {
        subtasks_list[subtask].service_id=service_id;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
     uint SC_Subtask_List :: nargs(const Subtask subtask) {
            return subtasks_list[subtask].nargs;
     }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
     void SC_Subtask_List :: nargs(const Subtask subtask, uint val) {
         subtasks_list[subtask].nargs=val;
#ifdef STATIC_ALLOC
         subtasks_list[subtask].arguments.size(val);
#endif // STATIC_ALLOC
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
     uint SC_Subtask_List :: nargs_absent(const Subtask subtask) {
            return subtasks_list[subtask].nargs_absent;
     }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
     void SC_Subtask_List :: nargs_absent(const Subtask subtask, uint val) {
         subtasks_list[subtask].nargs_absent=val;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
     void SC_Subtask_List :: decr_nargs_absent(const Subtask subtask) {
         subtasks_list[subtask].nargs_absent--;
//         return subtasks_list[subtask].nargs_absent;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
     void SC_Subtask_List :: incr_nargs_absent(const Subtask subtask) {
         subtasks_list[subtask].nargs_absent++;
//         return subtasks_list[subtask].nargs_absent;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    uint SC_Subtask_List :: mode(const Subtask subtask) {
            return subtasks_list[subtask].mode;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

        void SC_Subtask_List :: mode(const Subtask subtask, uint val) {
            subtasks_list[subtask].mode=val;
        }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
    uint SC_Subtask_List :: reg(const Subtask subtask) {
            return subtasks_list[subtask].reg;
    }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
        void SC_Subtask_List :: reg(const Subtask subtask, uint val) {
            subtasks_list[subtask].reg=val;
        }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

   Subtasks SC_Subtask_List :: subtasks() {
       Subtasks subtasks;
#ifndef STATIC_ALLOC
    for(SubtaskMap::iterator iter=subtasks_list.begin();iter!=subtasks_list.end();iter++) {
           subtasks.push_back(iter->first);
       }
#else
    for (uint i=0;i<CODE_SZ;i++) {
        if (occ[i]) {
            subtasks.push_back(i);
        }
    }
#endif
       return subtasks;
   }

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
   Subtask_List_Item SC_Subtask_List :: entry(const Subtask subtask) {
       return subtasks_list[subtask];
   }
*/

} /* namespace: SC_SBA */

#endif /* SC_SUBTASK_LIST_H_ */

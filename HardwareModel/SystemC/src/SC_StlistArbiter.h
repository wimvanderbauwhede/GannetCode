/*
********************************************************************************
                 |
  File Name      | SC_StlistArbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 02-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters of the Subtask_List
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

#ifndef SC_STLISTARBITER_H_
#define SC_STLISTARBITER_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA
{

//------------------------------------------------------------------------------
// DEFS
//------------------------------------------------------------------------------

//==============================================================================
//  CLASS: SC STLIST ARBITER INTernal CHANNEL
//==============================================================================
//! This is the SC_StlistArbiter_intchannel module, one for each master trying to access Subtask_List
    /*!
       One instance each of this module for each master connected to subtask_list arbiter
       It is a hierarchical channel that implements the SC_SubtaskList_if interface (all its methods)
    */

class SC_StlistArbiter_intchannel :
    public sc_module  ,
    public SC_SubtaskList_if
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>        p_mutex;    //!< port for shared mutex channel
    sc_port<SC_SubtaskList_if > pwr_slave;  //!< for accessing subtask_list; propogated to the slave_port of parent Arbiter

    // --------------------------- Interface Methods Implemented (Mutexed) ------------
    void                    add                 (const Subtask subtask)
    {
        p_mutex->lock();
        pwr_slave->add              (subtask);
        p_mutex->unlock();
    }

    void                    remove              (const Subtask subtask)
    {
        p_mutex->lock();
        pwr_slave->remove           (subtask);
        p_mutex->unlock();
    }

    Subtask_Argument_List&  arguments           (const Subtask subtask)
    {
        p_mutex->lock();
        Subtask_Argument_List& local = pwr_slave->arguments (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    add_arg           (const Subtask subtask,MemAddress address)
    {
        p_mutex->lock();
        pwr_slave->add_arg        (subtask, address);
        p_mutex->unlock();
    }

    Subtask_Status          status              (const Subtask subtask)
    {
        p_mutex->lock();
        Subtask_Status local = pwr_slave->status           (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    status              (const Subtask subtask, Subtask_Status status_)
    {
        p_mutex->lock();
        pwr_slave->status           (subtask, status_);
        p_mutex->unlock();
    }

    Word                    return_as           (const Subtask subtask)
    {
        p_mutex->lock();
        Word local = pwr_slave->return_as (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    return_as           (const Subtask subtask,Word return_as_)
    {
        p_mutex->lock();
        pwr_slave->return_as        (subtask, return_as_);
        p_mutex->unlock();
    }

    Symbol_t                called_as           (const Subtask subtask)
    {
        p_mutex->lock();
        Symbol_t local = pwr_slave->called_as (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    called_as           (const Subtask subtask,Symbol_t called_as)
    {
        p_mutex->lock();
        pwr_slave->called_as        (subtask, called_as);
        p_mutex->unlock();
    }

    Service                 to                  (const Subtask subtask)
    {
        p_mutex->lock();
        Service local = pwr_slave->to        (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    to                  (const Subtask subtask, Service to)
    {
        p_mutex->lock();
        pwr_slave->to               (subtask, to);
        p_mutex->unlock();
    }

    Service                 return_to           (const Subtask subtask)
    {
        p_mutex->lock();
        Service local = pwr_slave->return_to (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    return_to           (const Subtask subtask, Service return_to)
    {
        p_mutex->lock();
        pwr_slave->return_to        (subtask, return_to);
        p_mutex->unlock();
    }

    Word                    ack_to              (const Subtask subtask)
    {
        p_mutex->lock();
        Word local = pwr_slave->ack_to    (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    ack_to              (const Subtask subtask, Word ack_to)
    {
        p_mutex->lock();
        pwr_slave->ack_to           (subtask, ack_to);
        p_mutex->unlock();
    }

    uint                    redir               (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->redir     (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    redir               (const Subtask subtask, uint redir)
    {
        p_mutex->lock();
        pwr_slave->redir            (subtask, redir);
        p_mutex->unlock();
    }

    uint                    waiting_for_ack     (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->waiting_for_ack(subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    waiting_for_ack     (const Subtask subtask, uint waiting_for_ack)
    {
        p_mutex->lock();
        pwr_slave->waiting_for_ack  (subtask, waiting_for_ack);
        p_mutex->unlock();
    }

    CodeAddress             code_address        (const Subtask subtask)
    {
        p_mutex->lock();
        CodeAddress local = pwr_slave->code_address(subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    code_address        (const Subtask subtask, CodeAddress code_address)
    {
        p_mutex->lock();
        pwr_slave->code_address     (subtask, code_address);
        p_mutex->unlock();
    }

    uint                    service_id          (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->service_id(subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    service_id          (const Subtask subtask, uint service_id)
    {
        p_mutex->lock();
        pwr_slave->service_id       (subtask, service_id);
        p_mutex->unlock();
    }

    uint                    nargs               (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->nargs     (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    nargs               (const Subtask subtask, uint val)
    {
        p_mutex->lock();
        pwr_slave->nargs            (subtask, val);
        p_mutex->unlock();
    }

    uint                    nargs_absent        (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->nargs_absent(subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    nargs_absent        (const Subtask subtask, uint val)
    {
        p_mutex->lock();
        pwr_slave->nargs_absent     (subtask, val);
        p_mutex->unlock();
    }

    void                    decr_nargs_absent   (const Subtask subtask)
    {
        p_mutex->lock();
        pwr_slave->decr_nargs_absent(subtask);
        p_mutex->unlock();
    }

    void                    incr_nargs_absent   (const Subtask subtask)
    {
        p_mutex->lock();
        pwr_slave->incr_nargs_absent(subtask);
        p_mutex->unlock();
    }

    uint                    mode                (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->mode      (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    mode                (const Subtask subtask, uint val)
    {
        p_mutex->lock();
        pwr_slave->mode             (subtask, val);
        p_mutex->unlock();
    }

    uint                    reg                 (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->reg       (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    reg                 (const Subtask subtask, uint val)
    {
        p_mutex->lock();
        pwr_slave->reg              (subtask, val);
        p_mutex->unlock();
    }

    uint                    offset                 (const Subtask subtask)
    {
        p_mutex->lock();
        uint local = pwr_slave->offset       (subtask);
        p_mutex->unlock();
        return(local);
    }

    void                    offset                 (const Subtask subtask, uint val)
    {
        p_mutex->lock();
        pwr_slave->offset              (subtask, val);
        p_mutex->unlock();
    }

    uint                    fsize                 (const Subtask subtask)
     {
         p_mutex->lock();
         uint local = pwr_slave->fsize       (subtask);
         p_mutex->unlock();
         return(local);
     }

     void                    fsize                 (const Subtask subtask, uint val)
     {
         p_mutex->lock();
         pwr_slave->fsize              (subtask, val);
         p_mutex->unlock();
     }

    Subtasks                subtasks            ()
    {
        p_mutex->lock();
        Subtasks local = pwr_slave->subtasks  ();
        p_mutex->unlock();
        return(local);
    }

    Subtask_List_Item       entry               (const Subtask subtask)
    {
        p_mutex->lock();
        Subtask_List_Item local = pwr_slave->entry     (subtask);
        p_mutex->unlock();
        return(local);
    }

    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_HAS_PROCESS( SC_StlistArbiter_intchannel );
    SC_StlistArbiter_intchannel (sc_module_name nm):
        sc_module(nm)
    {
        // debug message...
        //const_debug_msg(name(),kind());
    }
};//class: SC_StlistArbiter_intchannel


//==============================================================================
//  CLASS: SC STLIST ARBITER
//==============================================================================
//! This is the SC_StlistArbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_SubtaskList_if for upto 8 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
class SC_StlistArbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------

    sc_export< SC_SubtaskList_if >  xpwr_master1;  //!< exports for masters of subtsk_list
    sc_export< SC_SubtaskList_if >  xpwr_master2;  //!<
    sc_export< SC_SubtaskList_if >  xpwr_master3;  //!<
    sc_export< SC_SubtaskList_if >  xpwr_master4;  //!<
    sc_export< SC_SubtaskList_if >  xpwr_master5;  //!<
    sc_export< SC_SubtaskList_if >  xpwr_master6;  //!<
    sc_export< SC_SubtaskList_if >  xpwr_master7;  //!<
    sc_export< SC_SubtaskList_if >  xpwr_master8;  //!<

    sc_port< SC_SubtaskList_if  >   pwr_slave;     //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_StlistArbiter_intchannel" for each master connected to arbiter
    SC_StlistArbiter_intchannel master2arb1;
    SC_StlistArbiter_intchannel master2arb2;
    SC_StlistArbiter_intchannel master2arb3;
    SC_StlistArbiter_intchannel master2arb4;
    SC_StlistArbiter_intchannel master2arb5;
    SC_StlistArbiter_intchannel master2arb6;
    SC_StlistArbiter_intchannel master2arb7;
    SC_StlistArbiter_intchannel master2arb8;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_StlistArbiter"; }

public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_StlistArbiter );
    SC_StlistArbiter( sc_module_name nm ) :
        sc_module( nm ) ,
        master2arb1("master2arb1"),
        master2arb2("master2arb2"),
        master2arb3("master2arb3"),
        master2arb4("master2arb4"),
        master2arb5("master2arb5"),
        master2arb6("master2arb6"),
        master2arb7("master2arb7"),
        master2arb8("master2arb8")
    {
        // Bindings
        // ========

        // Bind master exports directly to local hierarchical channels
        // which implement interface methods
        xpwr_master1.bind(master2arb1);
        xpwr_master2.bind(master2arb2);
        xpwr_master3.bind(master2arb3);
        xpwr_master4.bind(master2arb4);
        xpwr_master5.bind(master2arb5);
        xpwr_master6.bind(master2arb6);
        xpwr_master7.bind(master2arb7);
        xpwr_master8.bind(master2arb8);


        // Bind the slave ports of local channels to the slave port of
        // this module, to enable propagation of interface methods
        master2arb1.pwr_slave(pwr_slave);
        master2arb2.pwr_slave(pwr_slave);
        master2arb3.pwr_slave(pwr_slave);
        master2arb4.pwr_slave(pwr_slave);
        master2arb5.pwr_slave(pwr_slave);
        master2arb6.pwr_slave(pwr_slave);
        master2arb7.pwr_slave(pwr_slave);
        master2arb8.pwr_slave(pwr_slave);

        // Bind mutex ports of master2arb channels to the local mutex channel
        master2arb1.p_mutex(slave_mutex);
        master2arb2.p_mutex(slave_mutex);
        master2arb3.p_mutex(slave_mutex);
        master2arb4.p_mutex(slave_mutex);
        master2arb5.p_mutex(slave_mutex);
        master2arb6.p_mutex(slave_mutex);
        master2arb7.p_mutex(slave_mutex);
        master2arb8.p_mutex(slave_mutex);

        // debug message...
        const_debug_msg(name(),kind());
    }

};//class: SC_StlistArbiter

}//namespace: SC_SBA

#endif /* SC_STLISTARBITER_H_ */

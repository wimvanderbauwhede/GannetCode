/*
********************************************************************************
                 |
  File Name      | SC_FifoArbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 13-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters of a FIFO
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

#ifndef SC_ARBITER_H_
#define SC_ARBITER_H_

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
//  CLASS: SC FIFO Arbiter Internal Channel
//==============================================================================
//! This is the SC_FifoArbiter_int_channel module, one for each master
    /*!
       One instance each of this module for each master connected to arbiter
       It is a hierarchical channel that implements the SC_fifo_if interface
    */

template<typename DATA_T>
class SC_FifoArbiter_int_channel :
    public sc_module  ,
    public SC_Fifo_if<DATA_T>
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>            p_mutex;        //!< port for shared mutex channel
    sc_port<SC_Fifo_if<DATA_T> >    pw_slave;       //!< for accessing slave; propogated to the slave_port of parent Arbiter

    // --------------------------- Functions Declarations -----------------------------
    // They correspond to the access methods of the SBA::Fifo class.
    DATA_T          shift()             ;
    //void            unshift(DATA_T&)    ;
    void            push(DATA_T&)       ;
    //DATA_T          pop()               ;
    unsigned int    length()            {return pw_slave->length();}    // untimed so access through mutex not needed
    unsigned int    size()              {return pw_slave->size();}      // untimed so access through mutex not needed
    void            clear()             {pw_slave->clear();}            // - " -


    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_FifoArbiter_int_channel<DATA_T> (sc_module_name nm):
        sc_module(nm) {}
};//SC_FifoArbiter_int_channel


//------------------------------------------------------------------------------
//  SC_FifoArbiter_int_channel::shift()
//------------------------------------------------------------------------------
template <typename DATA_T>
DATA_T SC_FifoArbiter_int_channel<DATA_T> :: shift ()
{
//   OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//        << "Fifo arbiter is trying to shift data " << name() << endl;

   DATA_T val_ = pw_slave-> shift();

//    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//    << "Fifo arbiter has shifted data " << name() << endl;

    return(val_);
}//shift()

/*
//------------------------------------------------------------------------------
//  SC_FifoArbiter_int_channel::unshift()
//------------------------------------------------------------------------------
template <typename DATA_T>
void SC_FifoArbiter_int_channel<DATA_T> :: unshift (DATA_T& val_)
{
    p_mutex->lock();
    pw_slave-> unshift(val_);
    p_mutex->unlock();
}//unshift()
*/

//------------------------------------------------------------------------------
//  SC_FifoArbiter_int_channel::push()
//------------------------------------------------------------------------------
template <typename DATA_T>
void SC_FifoArbiter_int_channel<DATA_T> :: push (DATA_T& val_)
{
//    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//            << "Fifo arbiter is trying  to push data " << name() << endl;
//
    p_mutex->lock();

//   OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//    << "Fifo arbiter has locked mutex " << name() << endl;

    pw_slave-> push(val_);

//   OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//        << "Fifo arbiter is going to unlock mutex " << name() << endl;

    p_mutex->unlock();

//    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//            << "Fifo arbiter has pushed data " << name() << endl;

}//unshift()

/*
//------------------------------------------------------------------------------
//  SC_FifoArbiter_int_channel::pop()
//------------------------------------------------------------------------------
template <typename DATA_T>
DATA_T SC_FifoArbiter_int_channel<DATA_T> :: pop ()
{
    p_mutex->lock();
    DATA_T val_ = pw_slave-> pop();
    p_mutex->unlock();
    return(val_);
}//shift()
*/


//==============================================================================
//  CLASS: SC FIFO ARBITER
//==============================================================================
//! This is the SC_FifoArbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_Fifo_if for upto 4 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
template < typename DATA_T>
class SC_FifoArbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    typedef sc_export< SC_Fifo_if<DATA_T> > xport_type; //!<

    xport_type                                      xpw_master1;  //!< testing with two masters for now
    xport_type                                      xpw_master2;  //!< testing with two masters for now
    xport_type                                      xpw_master3;  //!< testing with two masters for now
    xport_type                                      xpw_master4;  //!< testing with two masters for now
    xport_type                                      xpw_master5;  //!< testing with two masters for now
    sc_port< SC_Fifo_if <DATA_T> >                  pw_slave;     //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_Arbiter_int_delayed_put_channel" for each master connected to arbiter
    SC_FifoArbiter_int_channel<DATA_T>  master2arb1;
    SC_FifoArbiter_int_channel<DATA_T>  master2arb2;
    SC_FifoArbiter_int_channel<DATA_T>  master2arb3;
    SC_FifoArbiter_int_channel<DATA_T>  master2arb4;
    SC_FifoArbiter_int_channel<DATA_T>  master2arb5;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_FifoArbiter"; }


public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_FifoArbiter );
    SC_FifoArbiter( sc_module_name nm ) :
        sc_module( nm ) ,
        master2arb1("master2arb1"),
        master2arb2("master2arb2"),
        master2arb3("master2arb3"),
        master2arb4("master2arb4"),
        master2arb5("master2arb5")
    {
        // Bindings
        // ========

        // Bind master exports directly to local hierarchical channels
        // which implement delayed_put()
        xpw_master1.bind(master2arb1);
        xpw_master2.bind(master2arb2);
        xpw_master3.bind(master2arb3);
        xpw_master4.bind(master2arb4);
        xpw_master5.bind(master2arb5);

        // Bind the slave ports of local channels to the slave port of
        // this module, to enable propagation of delayed_put()
        // is it possible to bind multiple ports to a single port?
        master2arb1.pw_slave(pw_slave);
        master2arb2.pw_slave(pw_slave);
        master2arb3.pw_slave(pw_slave);
        master2arb4.pw_slave(pw_slave);
        master2arb5.pw_slave(pw_slave);

        // Bind mutex ports of master2arb channels to the local mutex channel
        master2arb1.p_mutex(slave_mutex);
        master2arb2.p_mutex(slave_mutex);
        master2arb3.p_mutex(slave_mutex);
        master2arb4.p_mutex(slave_mutex);
        master2arb5.p_mutex(slave_mutex);

        // debug message...
        const_debug_msg(name(),kind());
    }

};//class:SC_Arbiter

} //namespace: SC_SBA

#endif /* SC_ARBITER_H_ */



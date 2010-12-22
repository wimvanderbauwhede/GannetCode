/*
********************************************************************************
                 |
  File Name      | SC_Stack_Arbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        |  26-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters
                 | of an SC_Stack object
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_STACKARBITER_H_
#define SC_STACKARBITER_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

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
//  CLASS: SC Stack Arbiter Internal Channel
//==============================================================================
//! This is the SC_Stack_Arbiter_int_channel module, one for each master
    /*!
       One instance each of this module for each master connected to arbiter
       It is a hierarchical channel that implements the SC_Stack_if interface
    */
class SC_Stack_Arbiter_int_channel :
    public sc_module  ,
    public SC_Stack_if
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>    p_mutex;        //!< port for shared mutex channel
    sc_port<SC_Stack_if>    pw_slave;       //!< for accessing slave; propogated to the slave_port of parent Arbiter

    // --------------------------- Functions Declarations -----------------------------
    unsigned int pop()
    {
        p_mutex->lock();
        unsigned int item = pw_slave -> pop();
        p_mutex->unlock();
        return item;
    }

    void push(unsigned int item)
    {
        p_mutex->lock();
        pw_slave -> push(item);
        p_mutex->unlock();
    }

    void push_back(unsigned int item)
    {
        p_mutex->lock();
        pw_slave -> push(item);
        p_mutex->unlock();
    }

    bool            empty() { return pw_slave -> empty();}
    unsigned int    size()  { return pw_slave -> size(); }

    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_Stack_Arbiter_int_channel (sc_module_name nm):
        sc_module(nm) {}
};//SC_Stack_Arbiter_int_channel




//==============================================================================
//  CLASS: SC STACK ARBITER
//==============================================================================
//! This is the SC_Stack_Arbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_Stack_if for upto 4 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
class SC_Stack_Arbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    typedef sc_export< SC_Stack_if > xport_type; //!<

    xport_type              xpw_master1;  //!< testing with two masters for now
    xport_type              xpw_master2;  //!< testing with two masters for now
    xport_type              xpw_master3;  //!< testing with two masters for now
    xport_type              xpw_master4;  //!< testing with two masters for now
    sc_port< SC_Stack_if >  pw_slave;     //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_Stack_Arbiter_int_delayed_put_channel" for each master connected to arbiter
    SC_Stack_Arbiter_int_channel master2arb1;
    SC_Stack_Arbiter_int_channel master2arb2;
    SC_Stack_Arbiter_int_channel master2arb3;
    SC_Stack_Arbiter_int_channel master2arb4;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Stack_Arbiter"; }


public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_Stack_Arbiter );
    SC_Stack_Arbiter( sc_module_name nm ) :
        sc_module( nm ) ,
        master2arb1("master2arb1"),
        master2arb2("master2arb2"),
        master2arb3("master2arb3"),
        master2arb4("master2arb4")
        //,
    {
        // Bindings
        // ========

        // Bind master exports directly to local hierarchical channels
        // which implement delayed_put()
        xpw_master1.bind(master2arb1);
        xpw_master2.bind(master2arb2);
        xpw_master3.bind(master2arb3);
        xpw_master4.bind(master2arb4);

        // Bind the slave ports of local channels to the slave port of
        // this module, to enable propagation of delayed_put()
        // is it possible to bind multiple ports to a single port?
        master2arb1.pw_slave(pw_slave);
        master2arb2.pw_slave(pw_slave);
        master2arb3.pw_slave(pw_slave);
        master2arb4.pw_slave(pw_slave);

        // Bind mutex ports of master2arb channels to the local mutex channel
        master2arb1.p_mutex(slave_mutex);
        master2arb2.p_mutex(slave_mutex);
        master2arb3.p_mutex(slave_mutex);
        master2arb4.p_mutex(slave_mutex);

        // debug message...
        const_debug_msg(name(),kind());
    }//constructor

};//class: SC_Stack_Arbiter

} //namespace: SC_SBA

#endif /* SC_STACKARBITER_H_ */

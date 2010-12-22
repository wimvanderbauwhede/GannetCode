/*
********************************************************************************
                 |
  File Name      | SC_Deque_Arbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 09-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters
                 | of an SC_Deque object
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_DEQUEARBITER_H_
#define SC_DEQUEARBITER_H_

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
//  CLASS: SC Deque Arbiter Internal Channel
//==============================================================================
//! This is the SC_Deque_Arbiter_int_channel module, one for each master
    /*!
       One instance each of this module for each master connected to arbiter
       It is a hierarchical channel that implements the SC_Deque_if interface
    */

template<typename DATA_T>
class SC_Deque_Arbiter_int_channel :
    public sc_module  ,
    public SC_Deque_if<DATA_T>
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>            p_mutex;        //!< port for shared mutex channel
    sc_port<SC_Deque_if<DATA_T> >    pw_slave;       //!< for accessing slave; propogated to the slave_port of parent Arbiter

    // --------------------------- Functions Declarations -----------------------------
    DATA_T shift()
    {
        p_mutex->lock();
        DATA_T item = pw_slave -> shift();
        p_mutex->unlock();
        return item;
    }

    void unshift(DATA_T& item)
    {
        p_mutex->lock();
        pw_slave -> unshift(item);
        p_mutex->unlock();
    }

    void push(DATA_T& item)
    {
        p_mutex->lock();
        pw_slave -> push(item);
        p_mutex->unlock();
    }

    DATA_T pop()
    {
        p_mutex->lock();
        DATA_T item = pw_slave -> pop();
        p_mutex->unlock();
        return item;
    }

    deque<DATA_T>& read_all () // return by reference, so that deque values can be changed
    {
        p_mutex->lock();
        deque<DATA_T>& item = pw_slave->read_all();
        p_mutex->unlock();
        return item;
    }

    // overloading the subscript '[]' operator since deque is accessed this way too...
    DATA_T& operator [] (unsigned int index)
    {
        p_mutex->lock();
        DATA_T& item = pw_slave -> at(index);
        p_mutex->unlock();
        return item;
    }

    // "at" does same functionality as [] (apparentlt additionall it throws exception for invalid values)
    // but we simply use the [] opreator of deque, make things simple and uniform
    DATA_T& at (unsigned int index)
    {
        p_mutex->lock();
        DATA_T& item = pw_slave -> at(index);
        p_mutex->unlock();
        return item;
    }

    void            operator = (const deque<DATA_T>& rhs_)
    {
        p_mutex->lock();
        pw_slave->read_all() = rhs_;
        p_mutex->unlock();
    }

    unsigned int length() { return pw_slave -> length(); }
    unsigned int size()   { return pw_slave -> size(); }

    // if decide to consume time for clear(), then use locks here too
    void clear(){ pw_slave -> clear(); }

    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_Deque_Arbiter_int_channel<DATA_T> (sc_module_name nm):
        sc_module(nm) {}
};//SC_Deque_Arbiter_int_channel




//==============================================================================
//  CLASS: SC DEQUE ARBITER
//==============================================================================
//! This is the SC_Deque_Arbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_Deque_if for upto 4 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
template < typename DATA_T>
class SC_Deque_Arbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    typedef sc_export< SC_Deque_if<DATA_T> > xport_type; //!<

    xport_type                                      xpw_master1;  //!< testing with two masters for now
    xport_type                                      xpw_master2;  //!< testing with two masters for now
    xport_type                                      xpw_master3;  //!< testing with two masters for now
    xport_type                                      xpw_master4;  //!< testing with two masters for now
    sc_port< SC_Deque_if <DATA_T> >                 pw_slave;     //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_Arbiter_int_delayed_put_channel" for each master connected to arbiter
    SC_Deque_Arbiter_int_channel<DATA_T>  master2arb1;
    SC_Deque_Arbiter_int_channel<DATA_T>  master2arb2;
    SC_Deque_Arbiter_int_channel<DATA_T>  master2arb3;
    SC_Deque_Arbiter_int_channel<DATA_T>  master2arb4;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Deque_Arbiter"; }


public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_Deque_Arbiter );
    SC_Deque_Arbiter( sc_module_name nm ) :
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
    }
};//class:SC_Arbiter

} //namespace: SC_SBA

#endif /* SC_DEQUEARBITER_H_ */

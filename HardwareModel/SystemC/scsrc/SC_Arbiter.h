/*
********************************************************************************
                 |
  File Name      | SC_Arbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 13-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_ARBITER_H_
#define SC_ARBITER_H_

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
//  CLASS: SC ARBITER INTernal DELAYED PUT CHANNEL
//==============================================================================
//! This is the SC_Arbiter_int_delayed_put_channel module, one for each master
    /*!
       One instance each of this module for each master connected to arbiter
       It is a hierarchical channel that implements the SC_fifo_delayed_put_if interface
    */

template<typename DATA_T>
class SC_Arbiter_int_delayed_put_channel :  public sc_module  ,
                                            public SC_fifo_delayed_put_if<DATA_T>
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>                        p_mutex;        //!< port for shared mutex channel
    sc_port<SC_fifo_delayed_put_if<DATA_T> >    pw_slave;        //!< for accessing slave; propogated to the slave_port of parent Arbiter

    // --------------------------- Functions Declarations -----------------------------
    // for delayed_put_if implementation (blocking put with processing delay)
    void delayed_put( const DATA_T& );

    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_Arbiter_int_delayed_put_channel<DATA_T> (sc_module_name nm)://, SC_Arbiter* parent_) :
        sc_module(nm)
    {
        // debug message...
        const_debug_msg(name(),kind());
    }
};//class:SC_Arbiter_int_delayed_put_channel


//==============================================================================
//  CLASS: SC ARBITER
//==============================================================================
//! This is the SC_Arbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_fifo_delayed_put_if for upto 3 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
template < typename DATA_T , int N = 2>
class SC_Arbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    typedef sc_export< SC_fifo_delayed_put_if<DATA_T> > xport_type; //!<
    //port_type master_port[N]; //
    //xport_type                                      xpw_master[2];  //!< testing with two masters for now
    xport_type                                      xpw_master1;  //!< testing with two masters for now
    xport_type                                      xpw_master2;  //!< testing with two masters for now
    xport_type                                      xpw_master3;  //!< testing with two masters for now
    sc_port< SC_fifo_delayed_put_if <DATA_T> , 1 >  pwr_slave;       //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_Arbiter_int_delayed_put_channel" for each master connected to arbiter
    SC_Arbiter_int_delayed_put_channel<DATA_T>  *master2arb1;
    SC_Arbiter_int_delayed_put_channel<DATA_T>  *master2arb2;
    SC_Arbiter_int_delayed_put_channel<DATA_T>  *master2arb3;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_Arbiter );
    SC_Arbiter( sc_module_name nm ) :
    sc_module( nm ) //,
    {
        // instantiating child modules
        master2arb1 = new SC_Arbiter_int_delayed_put_channel<DATA_T>("master2arb1");
        master2arb2 = new SC_Arbiter_int_delayed_put_channel<DATA_T>("master2arb2");
        master2arb3 = new SC_Arbiter_int_delayed_put_channel<DATA_T>("master2arb3");
        // Bindings
        // ========

        // Bind master exports directly to local hierarchical channels
        // which implement delayed_put()
        //xpw_master[1].bind(*master2arb1);
        //xpw_master[2].bind(*master2arb2);
        xpw_master1.bind(*master2arb1);
        xpw_master2.bind(*master2arb2);
        xpw_master3.bind(*master2arb3);

        // Bind the slave ports of local channels to the slave port of
        // this module, to enable propagation of delayed_put()
        // is it possible to bind multiple ports to a single port?
        master2arb1->pw_slave(pw_slave);
        master2arb2->pw_slave(pw_slave);
        master2arb3->pw_slave(pw_slave);

        // Bind mutex ports of master2arb channels to the local mutex channel
        master2arb1->p_mutex(slave_mutex);
        master2arb2->p_mutex(slave_mutex);
        master2arb3->p_mutex(slave_mutex);

        // debug message...
        const_debug_msg(name(),kind());
    }

};//class:SC_Arbiter



//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_Arbiter_int_delayed_put_channel::delayed_put
//* Object              : accepts put request, then using mutex, accesses the slave when it is free
//* Input Parameters    : value to be put in fifo
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename DATA_T>
void SC_Arbiter_int_delayed_put_channel<DATA_T> :: delayed_put (const DATA_T& val_ )
{
    // if MUTEX guarding slave is available, take it
    // have to access it through the port since is outside the module
    p_mutex->lock();

    // put request to slave here
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the delayed_put operation in the slave is complete
    pw_slave->delayed_put( val_ );

    // release lock
    p_mutex->unlock();
}//delayed_put()



} //namespace: SC_SBA

#endif /* SC_ARBITER_H_ */



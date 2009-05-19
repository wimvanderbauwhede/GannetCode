/*
********************************************************************************
                 |
  File Name      | SC_RegArbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 20-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters of a FIFO
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





#ifndef SC_REGARBITER_H_
#define SC_REGARBITER_H_

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
//  CLASS: SC ARBITER INTernal DELAYED PUT CHANNEL
//==============================================================================
//! This is the SC_RegArbiter_intchannel module, one for each master
    /*!
       One instance each of this module for each master connected to register arbiter
       It is a hierarchical channel that implements the SC_reg_out_if interface (write and read)
    */

template<typename DATA_T>
class SC_RegArbiter_intchannel :
    public sc_module  ,
    public SC_reg_out_if_d<DATA_T>
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>                p_mutex;        //!< port for shared mutex channel
    sc_port<SC_reg_out_if_d<DATA_T> >   pwr_slave;      //!< for accessing slave register; propogated to the slave_port of parent Arbiter

    // --------------------------- Functions Declarations -----------------------------
    DATA_T&   read                ();
    void            write               ( const DATA_T& );
    const sc_event& value_changed_event () const {
//    	OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//        << "SC_RegArbiter_intchannel "<<name()<<" value_changed_event() called\n";
    	return pwr_slave->value_changed_event();
    } ;

    void            operator =          (const DATA_T& rhs_);
    bool            operator ==         (const DATA_T& rhs_);
                    operator DATA_T&    ();

    // ---------------------------- CONSTRUCTOR  ---------------------------------------
    SC_HAS_PROCESS( SC_RegArbiter_intchannel );
    SC_RegArbiter_intchannel<DATA_T> (sc_module_name nm):
        sc_module(nm)
    {
        // debug message...
        //const_debug_msg(name(),kind());
    }
};//SC_RegArbiter_intchannel

//*---------------------------------------------------------------------------------------------------------
//* write   (const DATA_T& val_ )
//*---------------------------------------------------------------------------------------------------------
template <typename DATA_T>
void SC_RegArbiter_intchannel<DATA_T> :: write   (const DATA_T& val_ )
{
    // if MUTEX guarding slave is available, take it
    // have to access it through the port since is outside the module
    p_mutex->lock();

    // put request to slave here
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the write operation in the slave is complete
    pwr_slave->write  ( val_ );

    // release lock
    p_mutex->unlock();
}//write()

//*---------------------------------------------------------------------------------------------------------
//* read  ()
//*---------------------------------------------------------------------------------------------------------
template <typename DATA_T>
DATA_T& SC_RegArbiter_intchannel<DATA_T> :: read  ()
{
    // No need of mutex since read can be shared (unlike shift from FIFO)
    // put request to slave here,
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the read operation in the slave is complete


    return(pwr_slave->read  ());
}//read()

//*---------------------------------------------------------------------------------------------------------
//* operator =
//*---------------------------------------------------------------------------------------------------------

template <typename DATA_T>
void SC_RegArbiter_intchannel<DATA_T> :: operator =  (const DATA_T& rhs_)
{
    p_mutex->lock();
    pwr_slave->write(rhs_);
    p_mutex->unlock();
}//'=' overload

//*---------------------------------------------------------------------------------------------------------
//* operator  ==
//*---------------------------------------------------------------------------------------------------------

template <typename DATA_T>
bool SC_RegArbiter_intchannel<DATA_T> :: operator  == (const DATA_T& rhs_)
{
    // no arbitration needed
    return ( pwr_slave->read() == rhs_ );
}

//*---------------------------------------------------------------------------------------------------------
//* operator DATA_T& ()
//*---------------------------------------------------------------------------------------------------------
template <typename DATA_T>
SC_RegArbiter_intchannel<DATA_T> :: operator DATA_T& ()
{
    return ( pwr_slave->read());
}


//==============================================================================
//  CLASS: SC REG ARBITER
//==============================================================================
//! This is the SC_RegArbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_fifo_delayed_put_if for upto 3 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
template < typename DATA_T >
class SC_RegArbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    typedef sc_export< SC_reg_out_if_d<DATA_T> >    xport_type; //!<
    xport_type                                      xpwr_master1;  //!< testing with three masters for now
    xport_type                                      xpwr_master2;  //!<
    xport_type                                      xpwr_master3;  //!<
    xport_type                                      xpwr_master4;  //!< WV: need 4th master for dispatch_data_packet
    sc_port< SC_reg_out_if_d <DATA_T> , 1 >         pwr_slave;     //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_RegArbiter_intchannel" for each master connected to arbiter
    SC_RegArbiter_intchannel<DATA_T>  master2arb1;
    SC_RegArbiter_intchannel<DATA_T>  master2arb2;
    SC_RegArbiter_intchannel<DATA_T>  master2arb3;
    SC_RegArbiter_intchannel<DATA_T>  master2arb4;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_RegArbiter"; }

public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_RegArbiter );
    SC_RegArbiter( sc_module_name nm ) :
        sc_module( nm ) ,
        master2arb1("master2arb1"),
        master2arb2("master2arb2"),
        master2arb3("master2arb3"),
        master2arb4("master2arb4")
    {
        // instantiating child modules
        //master2arb1 = new SC_RegArbiter_intchannel<DATA_T>("master2arb1");
        //master2arb2 = new SC_RegArbiter_intchannel<DATA_T>("master2arb2");
        //master2arb3 = new SC_RegArbiter_intchannel<DATA_T>("master2arb3");
    	//master2arb4 = new SC_RegArbiter_intchannel<DATA_T>("master2arb4");
    	// Bindings
        // ========

        // Bind master exports directly to local hierarchical channels
        // which implement write() and read()
        xpwr_master1.bind(master2arb1);
        xpwr_master2.bind(master2arb2);
        xpwr_master3.bind(master2arb3);
        xpwr_master4.bind(master2arb4);

        // Bind the slave ports of local channels to the slave port of
        // this module, to enable propagation of write/read()
        master2arb1.pwr_slave(pwr_slave);
        master2arb2.pwr_slave(pwr_slave);
        master2arb3.pwr_slave(pwr_slave);
        master2arb4.pwr_slave(pwr_slave);

        // Bind mutex ports of master2arb channels to the local mutex channel
        master2arb1.p_mutex(slave_mutex);
        master2arb2.p_mutex(slave_mutex);
        master2arb3.p_mutex(slave_mutex);
        master2arb4.p_mutex(slave_mutex);

        // debug message...
        const_debug_msg(name(),kind());
    }

    const sc_event& value_changed_event () const {
    	OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
        << "SC_RegArbiter "<<name()<<" value_changed_event() called, value is now "<< master2arb1->read()<< "\n";
    	return master2arb1->value_changed_event(); // but could be any of the three!
    } ;

};//class: SC_RegArbiter

}//namespace: SC_SBA
#endif /* SC_REGARBITER_H_ */

/*
********************************************************************************
                 |
  File Name      | SC_Memory_Arbiter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 01-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters of a SC_Memory(_List) object
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_MEMARBITER_H_
#define SC_MEMARBITER_H_


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
//  CLASS: SC MEM ARBITER INTernal CHANNEL
//==============================================================================
//! This is the SC_Memory_Arbiter_intchannel module, one for each master
    /*!
       One instance each of this module for each master connected to memory arbiter
       It is a hierarchical channel that implements the SC_Memory_if interface (write and read)
    */

template<typename ADDR_T, typename DATA_T>
class SC_Memory_Arbiter_intchannel :
    public sc_module  ,
    public SC_Memory_if<ADDR_T, DATA_T>
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>                    p_mutex;    //!< port for shared mutex channel
    sc_port<SC_Memory_if<ADDR_T,DATA_T> >   pwr_slave;  //!< for accessing slave memory; propogated to the slave_port of parent Arbiter

    // --------------------------- Functions Declarations -----------------------------
    virtual void write(ADDR_T ,DATA_T);
    virtual void append(ADDR_T ,DATA_T);
    virtual DATA_T& read(ADDR_T);
    virtual bool is_free(ADDR_T);
    virtual unsigned int size(ADDR_T);
    // overloading the subscript '[]' operator
    DATA_T&         operator [] (ADDR_T index)
    {
        p_mutex->lock();
        // storing local data by reference so that the original data is returned
        // ([] overloaded) will be used for write as well
        DATA_T& data = pwr_slave->read(index);
        p_mutex->unlock();
        return(data);
    } //non-const
/*
     const DATA_T&   operator [] (ADDR_T index) const
    {
        p_mutex->lock();
        DATA_T data = pwr_slave->read(index);
        p_mutex->unlock();
        return(data);
    } //const
*/
    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_HAS_PROCESS( SC_Memory_Arbiter_intchannel );
    SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T> (sc_module_name nm):
        sc_module(nm)
    {
        // debug message...
        //const_debug_msg(name(),kind());
    }
};//class: SC_Memory_Arbiter_intchannel

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_Memory_Arbiter_intchannel :: write
//* Object              : accepts write request, then using mutex, accesses the slave when it is free
//* Input Parameters    : address and data
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename ADDR_T, typename DATA_T>
void SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T> :: write (ADDR_T addr, DATA_T data)
{
    // if MUTEX guarding slave is available, take it
    // have to access it through the port since is outside the module
    p_mutex->lock();

    // put request to slave here
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the write operation in the slave is complete
    pwr_slave->write(addr, data);

    // release lock
    p_mutex->unlock();
}//write()

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_Memory_Arbiter_intchannel :: append
//* Object              : accepts append request, then using mutex, accesses the slave when it is free
//* Input Parameters    : address and data
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename ADDR_T, typename DATA_T>
void SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T> :: append (ADDR_T addr, DATA_T data)
{
    // if MUTEX guarding slave is available, take it
    // have to access it through the port since is outside the module
    p_mutex->lock();

    // put request to slave here
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the append operation in the slave is complete
    pwr_slave->append(addr, data);

    // release lock
    p_mutex->unlock();
}//append()

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_Memory_Arbiter_intchannel :: read
//* Object              : accepts read request, then using mutex, accesses the slave when it is free
//* Input Parameters    : read address
//* Output Parameters   : reference to read data
//*---------------------------------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
DATA_T&  SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T> :: read(ADDR_T addr)
{
    // No need of mutex since read can be shared (unlike shift from FIFO)
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the read operation in the slave is complete
    // if MUTEX guarding slave is available, take it
    // have to access it through the port since is outside the module
    p_mutex->lock();

    // put request to slave here
    // it is propogated to the slave port of the parent Arbiter
    // this will block until the write operation in the slave is complete
    // storing local data by reference so that the original data is returned
    // since the read operation (used by [] overloaded) will be used for write as well
    DATA_T& data = pwr_slave->read(addr);

    // release lock
    p_mutex->unlock();

    return(data);
}//read()


//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_Memory_Arbiter_intchannel :: is_free
//* Object              : check if address is free
//* Input Parameters    : address
//* Output Parameters   : bool (free or not)
//*---------------------------------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
bool  SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T> :: is_free(ADDR_T addr)
{
    return(pwr_slave->is_free(addr));
}//is_free()

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_Memory_Arbiter_intchannel :: size
//* Object              : check size of data stored at address for timing purposes
//* Input Parameters    : address
//* Output Parameters   : unsigned int
//*---------------------------------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
unsigned  SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T> :: size(ADDR_T addr)
{
    return(pwr_slave->size(addr));
}//size()


//==============================================================================
//  CLASS: SC MEM ARBITER
//==============================================================================
//! This is the SC_Memory_Arbiter module
    /*
        Using an internal sc_mutex channel, this module arbitrates between multiple masters
        Exports SC_Memory_if for upto 3 masters (can increase as required)
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
template < typename ADDR_T, typename DATA_T  >
class SC_Memory_Arbiter
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------

    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master1;  //!< testing with three masters for now
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master2;  //!<
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master3;  //!<
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master4;  //!<
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master5;  // WV: if only we could generate things like this ...
    sc_port< SC_Memory_if <ADDR_T, DATA_T>  >   pwr_slave;     //!< Port for accessing slave, presumably bound to its export.

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_Memory_Arbiter_intchannel" for each master connected to arbiter
    SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T>  master2arb1;
    SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T>  master2arb2;
    SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T>  master2arb3;
    SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T>  master2arb4;
    SC_Memory_Arbiter_intchannel<ADDR_T, DATA_T>  master2arb5;

    sc_mutex slave_mutex;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Memory_Arbiter"; }

public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_Memory_Arbiter );
    SC_Memory_Arbiter( sc_module_name nm ) :
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
        // which implement write() and read()
        xpwr_master1.bind(master2arb1);
        xpwr_master2.bind(master2arb2);
        xpwr_master3.bind(master2arb3);
        xpwr_master4.bind(master2arb4);
        xpwr_master5.bind(master2arb5);

        // Bind the slave ports of local channels to the slave port of
        // this module, to enable propagation of write/read()
        master2arb1.pwr_slave(pwr_slave);
        master2arb2.pwr_slave(pwr_slave);
        master2arb3.pwr_slave(pwr_slave);
        master2arb4.pwr_slave(pwr_slave);
        master2arb5.pwr_slave(pwr_slave);

        // Bind mutex ports of master2arb channels to the local mutex channel
        master2arb1.p_mutex(slave_mutex);
        master2arb2.p_mutex(slave_mutex);
        master2arb3.p_mutex(slave_mutex);
        master2arb4.p_mutex(slave_mutex);
        master2arb5.p_mutex(slave_mutex);

        // debug message...
        const_debug_msg(name(),kind());
    }

};//class: SC_Memory_Arbiter

}//namespace: SC_SBA

#endif /* SC_MEMARBITER_H_ */





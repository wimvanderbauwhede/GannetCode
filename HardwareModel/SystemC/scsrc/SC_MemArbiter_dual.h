/*
 * SC_MemArbiter_dual.h
 *
 *  Created on: 04-Dec-2008
 *      Author: StudentAdmin
 */


/*
********************************************************************************
                 |
  File Name      | SC_MemArbiter_dual.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 04-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for Arbitration between multiple masters of a SC_Memory(_List) object
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

#ifndef SC_MEMARBITER_DUAL_H_
#define SC_MEMARBITER_DUAL_H_


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
#define _ARB_retryperiod 4  // Retry time period for retry in case both slave ports are busy
                            // Specified Number of clock periods (where "_CLK_P" is period in "_CLK_U" units)
//==============================================================================
//  CLASS: SC MEM ARBITER "DUAL" INTernal CHANNEL
//==============================================================================
//! This is the SC_MemArbiter_dual_intchannel module, one for each master, accessing a dual-ported memory
    /*!
       One instance each of this module for each master connected to memory arbiter
       It is a hierarchical channel that implements the SC_Memory_if interface (write and read)
       and presumes access to a dual-ported slave memory (and hence uses two mutexes).
    */

template<typename ADDR_T, typename DATA_T>
class SC_MemArbiter_dual_intchannel :
    public sc_module  ,
    public SC_Memory_if<ADDR_T, DATA_T>
{
public:
    // --------------------------- Ports ----------------------------------------------
    sc_port<sc_mutex_if>                    p_mutex_1;  //!< port for first shared mutex
    sc_port<sc_mutex_if>                    p_mutex_2;  //!< port for second shared mutex

    //!< for accessing slave dual ported memory through ports 1 and 2; propogated to the slave_ports of parent Arbiter
    sc_port<SC_Memory_if<ADDR_T,DATA_T> >   pwr_slave_1;
    sc_port<SC_Memory_if<ADDR_T,DATA_T> >   pwr_slave_2;

    // --------------------------- Functions Declarations -----------------------------
    virtual void write(ADDR_T ,DATA_T);
    virtual DATA_T& read(ADDR_T); // return by reference so can be used to change value as well
    virtual unsigned int size(ADDR_T);
    virtual bool is_free(ADDR_T);
    DATA_T&         operator [] (ADDR_T index) // overload [] operator to allow read operation
    {
        //DATA_T data;
        if (p_mutex_1->trylock()==0)
        {
            DATA_T& data = pwr_slave_1->read(index);
            p_mutex_1->unlock();
            return(data);
        }
        else if (p_mutex_2->trylock()==0)
        {
            DATA_T& data = pwr_slave_2->read(index);
            p_mutex_2->unlock();
            return(data);
        }
        // both mutexes taken; wait for specific time, and try again (recursive)
        else
        {
#ifdef VERBOSE
        	OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
                << "Memory contention, waiting...\n";
#endif
            wait(_ARB_retryperiod * _CLK_P, _CLK_U);
            DATA_T& data = read(index);
            return(data);
        }
        //return(data);
    } //non-const


    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_HAS_PROCESS( SC_MemArbiter_dual_intchannel );
    SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T> (sc_module_name nm):
        sc_module(nm)
    {
        // debug message...
        //const_debug_msg(name(),kind());x
    }
};//class: SC_MemArbiter_dual_intchannel



//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_MemArbiter_dual_intchannel :: write
//* Object              : accepts write request, then using mutexes, accesses whichever port of slave is free
//* Input Parameters    : address and data
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename ADDR_T, typename DATA_T>
void SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T> :: write (ADDR_T addr, DATA_T data)
{
    // if MUTEX guarding slave port 1 is available, take it, use it  for
    // writing to port 1 of slave, and unlock it. Otherwise try other port
    if (p_mutex_1->trylock()==0)
    {
        pwr_slave_1->write(addr, data);
        p_mutex_1->unlock();
    }
    else if (p_mutex_2->trylock()==0)
    {
        pwr_slave_2->write(addr, data);
        p_mutex_2->unlock();
    }
    // both mutexes taken; wait for specific time, and try again (recursive)
    else
    {
        //OSTREAM << "Retrying Dual-port memory access " << endl;
        wait(_ARB_retryperiod * _CLK_P, _CLK_U);
        write(addr, data);
    }
}//write()

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_MemArbiter_dual_intchannel :: read
//* Object              : accepts read request, then using mutex, accesses the slave when it is free
//* Input Parameters    : read address
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
DATA_T&  SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T> :: read(ADDR_T addr)
{
    // local variable should store a reference to the data, rather than just its value
    //DATA_T data;
    //DATA_T& data = pwr_slave->read(addr);

    // if MUTEX guarding slave port 1 is available, take it, use it  for
    // reading  from port 1 of slave, and unlock it. Otherwise try other port
    if (p_mutex_1->trylock()==0)
    {
        DATA_T& data = pwr_slave_1->read(addr);
        p_mutex_1->unlock();
        return(data);
    }
    else if (p_mutex_2->trylock()==0)
    {
        DATA_T& data = pwr_slave_2->read(addr);
        p_mutex_2->unlock();
        return(data);
    }
    // both mutexes taken; wait for specific time, and try again (recursive)
    else
    {
        wait(_ARB_retryperiod * _CLK_P, _CLK_U);
        DATA_T& data = read(addr);
        return(data);
    }
    //return(data);
}//read()


//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_MemArbiter_dual_intchannel :: is_free
//* Object              : check if address is free
//* Input Parameters    : address
//* Output Parameters   : bool (free or not)
//*---------------------------------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
bool  SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T> :: is_free(ADDR_T addr)
{
    return(pwr_slave_1->is_free(addr));
}//is_free()
//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_MemArbiter_dual_intchannel :: size
//* Object              : check size of data stored at address (for timing purposes)
//* Input Parameters    : address
//* Output Parameters   : unsigned integer
//*---------------------------------------------------------------------------------------------------------

template <typename ADDR_T, typename DATA_T>
unsigned int  SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T> :: size(ADDR_T addr)
{
    return(pwr_slave_1->size(addr));
}//size()


//==============================================================================
//  CLASS: SC MEM ARBITER DUAL
//==============================================================================
//! This is the SC_MemArbiter_dual module
    /*
        Using two internal sc_mutex channels, this module arbitrates between multiple masters
        Exports SC_Memory_if for upto 4 masters (can increase as required).
        Arbitrates access to dual-ported memory.
        When an access request is made by a master, checks both ports and gives access to whichever is free.
        If both ports are busy, then polls until one of them becomes free.
        TODO: Use conditional compilation to declare as many exports as required
        If multiple masters try to access the slave while it is locked, the sequence
        with which the blocked masters will get access once it unlocks is indeterministic
    */
template < typename ADDR_T, typename DATA_T  >
class SC_MemArbiter_dual
    : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------

    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master1;  //!< testing with three masters for now
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master2;  //!<
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master3;  //!<
    sc_export< SC_Memory_if<ADDR_T, DATA_T> >   xpwr_master4;  //!<
    sc_port< SC_Memory_if <ADDR_T, DATA_T>  >   pwr_slave_1;   //!< Port for accessing slave port 1, presumably bound to its export.
    sc_port< SC_Memory_if <ADDR_T, DATA_T>  >   pwr_slave_2;   //!< Port for accessing slave port 2

    // ---------------------------- LOCALS -------------------------------------
    // One instance each of "SC_RegArbiter_intchannel" for each master connected to arbiter
    SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T>  master2arb1;
    SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T>  master2arb2;
    SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T>  master2arb3;
    SC_MemArbiter_dual_intchannel<ADDR_T, DATA_T>  master2arb4;

    sc_mutex slave_mutex_1;   //!< To have MUTEX access to the slave port
    sc_mutex slave_mutex_2;   //!< To have MUTEX access to the slave port

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_MemArbiter_dual"; }

public:
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS( SC_MemArbiter_dual );
    SC_MemArbiter_dual( sc_module_name nm ) :
        sc_module( nm ) ,
        master2arb1("master2arb1"),
        master2arb2("master2arb2"),
        master2arb3("master2arb3"),
        master2arb4("master2arb4")
    {
        // Bindings
        // ========

        // Bind master exports directly to local hierarchical channels
        // which implement write() and read()
        xpwr_master1.bind(master2arb1);
        xpwr_master2.bind(master2arb2);
        xpwr_master3.bind(master2arb3);
        xpwr_master4.bind(master2arb4);

        // Bind the two slave ports of master2arb channels to the two slave ports of
        // this module, to enable propagation of write/read()
        master2arb1.pwr_slave_1(pwr_slave_1);
        master2arb2.pwr_slave_1(pwr_slave_1);
        master2arb3.pwr_slave_1(pwr_slave_1);
        master2arb4.pwr_slave_1(pwr_slave_1);

        master2arb1.pwr_slave_2(pwr_slave_2);
        master2arb2.pwr_slave_2(pwr_slave_2);
        master2arb3.pwr_slave_2(pwr_slave_2);
        master2arb4.pwr_slave_2(pwr_slave_2);

        // Bind 2 mutex ports of master2arb channels to the 2 local slave mutexes (since accessing a dual-port memory)
        master2arb1.p_mutex_1(slave_mutex_1);
        master2arb2.p_mutex_1(slave_mutex_1);
        master2arb3.p_mutex_1(slave_mutex_1);
        master2arb4.p_mutex_1(slave_mutex_1);

        master2arb1.p_mutex_2(slave_mutex_2);
        master2arb2.p_mutex_2(slave_mutex_2);
        master2arb3.p_mutex_2(slave_mutex_2);
        master2arb4.p_mutex_2(slave_mutex_2);

        // debug message...
        const_debug_msg(name(),kind());
    }

};//class: SC_MemArbiter_dual

}//namespace: SC_SBA


#endif /* SC_MEMARBITER_DUAL_H_ */

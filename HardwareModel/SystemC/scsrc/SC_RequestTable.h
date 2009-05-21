/*
********************************************************************************
                 |
  File Name      | SC_RequestTable.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of Gannet Hardware
-----------------|--------------------------------------------------------------
  Created        | 08-April-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC class for implementing Requestss
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/

#ifndef SC_REQUEST_TABLE_H_
#define SC_REQUEST_TABLE_H_


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


//==============================================================================
//  CLASS: SC_RequestTable
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This the SC model of SC_RequestTable
    /*!
       Currently simply a wrapper around an SBA::Requests object
    */
template <unsigned int SIZE>
class SC_RequestTable :
    public sc_module    ,
    public SC_RequestTable_if
{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_RequestTable_if > xpwr_1; // Export for access to Requests
    sc_export<SC_RequestTable_if > xpwr_2; // Export for access to Requests

    // ---------------------------- Sub-Modules --------------------------------
    vector<SBA::Requests> requests_table;//!< The local SBA::Requests object that contains the memory element
                                //!< also implements all the access methods as defined in SC_RequestTable_if

    // ---------------------------- METHODS ------------------------------------
    // The following SC_RequestTable_if interface methods are implemented in this module and exported
    // They correspond to the access methods of the SBA::Requests class.

    void push(const SBA::MemAddress,SBA::Word);
    //* Shift a request off the list at the given address.
    SBA::Word shift(const SBA::MemAddress);
    unsigned int size(const SBA::MemAddress);

    virtual const char* kind() const
        { return "SC_RequestTable"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_RequestTable);
    SC_RequestTable(sc_module_name nm) :
        sc_module(nm)
        ,requests_table (SIZE)
    {
        // ************ Instantiaing ************

        // ************ Bindings ************
        // Bind exports to *this, which is a hierarchichal channel that implements the SC_SubtaskList_if interface
        xpwr_1.bind(*this);
        xpwr_2.bind(*this);

        // creation debug message..
        const_debug_msg(name(),kind());
    }
};/* class: SC_RequestTable */

//------------------------------------------------------------------------------
//  SC_RequestTable::push()
//------------------------------------------------------------------------------

//void SC_RequestTable<SIZE> :: push(SBA::MemAddress address,SBA::Word item)
template <unsigned int SIZE>
void SC_RequestTable <SIZE>:: push(const SBA::MemAddress address,SBA::Word item)
{
    wait( 2*_CLK_P, _CLK_U); // 2 cycles: 1. get adress, 2. write to address, 3. update counter (in parallel, dual port)?
    requests_table.at(address).push(item);
}// funct: SC_RequestTable :: push()


//------------------------------------------------------------------------------
//  SC_RequestTable::shift()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
SBA::Word SC_RequestTable <SIZE>:: shift(const SBA::MemAddress address)
{
    unsigned int item = requests_table.at(address).shift();
    wait( 2*_CLK_P, _CLK_U); // 1. get address 2. read from address
    return (item);
}// funct: SC_RequestTable :: shift()


//------------------------------------------------------------------------------
//  SC_RequestTable::size()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
unsigned int SC_RequestTable <SIZE>:: size(const SBA::MemAddress address)
{
    unsigned int item = requests_table.at(address).size();
    // in actual HW we'll need a counter to keep track of the number of requests at each
    // location. It seems logical to store this in the first address. So we need a read to get this value
    wait( _CLK_P , _CLK_U); // wait for one clock cycle
    return (item);
}// funct: SC_RequestTable :: size()



} /* namespace: SC_SBA */




#endif /* SC_REQUEST_TABLE_H_ */

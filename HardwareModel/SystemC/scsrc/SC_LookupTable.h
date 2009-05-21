/*
********************************************************************************
                 |
  File Name      | SC_LookupTable.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of Gannet Hardware
-----------------|--------------------------------------------------------------
  Created        | 08-April-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC class for implementing Lookupss
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_LOOKUP_TABLE_H_
#define SC_LOOKUP_TABLE_H_


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
//  CLASS: SC_LookupTable
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This the SC model of SC_LookupTable
    /*!
       Currently simply a wrapper around an SBA::Lookups object
    */
    
class SC_LookupTable :
    public sc_module    ,
    public SC_LookupTable_if
{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_LookupTable_if > xpwr_1; // Export for access to Lookups
    sc_export<SC_LookupTable_if > xpwr_2; // Export for access to Lookups

    // ---------------------------- Sub-Modules --------------------------------
    SBA::LookupTable lookup_table;//!< The local SBA::Lookups object that contains the memory element
                                //!< also implements all the access methods as defined in SC_LookupTable_if

    // ---------------------------- METHODS ------------------------------------
    // The following SC_LookupTable_if interface methods are implemented in this module and exported
    // They correspond to the access methods of the SBA::Lookups class.

    void write(const SBA::Uint64,SBA::Sint64);
    //* Shift a Lookup off the list at the given address.
    SBA::Sint64 read(const SBA::Uint64);
    void erase(const SBA::Uint64);
    unsigned int size();
	unsigned int count(const SBA::Uint64);
	
    virtual const char* kind() const
        { return "SC_LookupTable"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_LookupTable);
    SC_LookupTable(sc_module_name nm) :
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
};/* class: SC_LookupTable */

//------------------------------------------------------------------------------
//  SC_LookupTable::write()
//------------------------------------------------------------------------------

void SC_LookupTable :: write(const SBA::Uint64 address,SBA::Sint64 item)
{
    wait( 2*_CLK_P, _CLK_U); // assuming CAM
    lookup_table.write(address,item);
}// funct: SC_LookupTable :: write()


//------------------------------------------------------------------------------
//  SC_LookupTable::read()
//------------------------------------------------------------------------------

SBA::Sint64 SC_LookupTable :: read(const SBA::Uint64 address)
{
    SBA::Sint64 item = lookup_table.read(address);
    wait( 2*_CLK_P, _CLK_U); // assuming CAM
    return (item);
}// funct: SC_LookupTable :: read()


//------------------------------------------------------------------------------
//  SC_LookupTable::erase()
//------------------------------------------------------------------------------

void SC_LookupTable :: erase(const SBA::Uint64 address)
{
    lookup_table.erase(address);
    wait( 2*_CLK_P, _CLK_U); // assuming CAM

}// funct: SC_LookupTable :: erase()

//------------------------------------------------------------------------------
//  SC_LookupTable::size()
//------------------------------------------------------------------------------

unsigned int SC_LookupTable :: size()
{
    unsigned int item = lookup_table.size();
    wait( _CLK_P , _CLK_U); // assuming CAM
    return (item);
}// funct: SC_LookupTable :: size()

//------------------------------------------------------------------------------
//  SC_LookupTable::count()
//------------------------------------------------------------------------------

unsigned int SC_LookupTable :: count(const SBA::Uint64 address)
{
    unsigned int item = lookup_table.count(address);
    wait( _CLK_P , _CLK_U); // wait for one clock cycle
    return (item);
}// funct: SC_LookupTable :: size()

} /* namespace: SC_SBA */




#endif /* SC_LOOKUP_TABLE_H_ */

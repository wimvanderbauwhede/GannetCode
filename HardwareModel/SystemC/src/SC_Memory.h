/*
********************************************************************************
                 |
  File Name      | SC_Memory.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 21-Nov-2008. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC wrapper class for using SBA::Memory
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





#ifndef SC_MEMORY_H_
#define SC_MEMORY_H_

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

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//==============================================================================
//  CLASS: SYSTEMC STORAGE MODULE (Base class for SC_Memory)
//==============================================================================

//! The SystemC Store class on which SC_Memory class and emory class is based
    /*!
       Based on the SBA::Store class,
       but not in any way linked to them, (i.e. no inclusion or linking to C++ classes needed)
       while the DATA_T can be any data type, this class does not contain
       put and get functions that can be used for accessing members INSIDE
       a composite DATA_T (like Word_List).
       So use this class for situations where the stored DATA_T is primitive (uint)
       or the DATA_T is composite there is no need for pushing or getting sub-elements.
    */
template <typename DATA_T >
class SC_Store    //:
    //public sc_module
{
private:

protected:
//public:
    // the data storate map is defined as protected so that it is accessible in the derived SC_Memory classes
    map<unsigned int,DATA_T> store; //
    typedef typename map<unsigned int, DATA_T>::iterator  map_iter;

public:

    /// Write operation (can be a list write)
    void mput(unsigned int address,DATA_T data);
    /// Read operation (can be list read)
    // returning by reference so that [] operator overload can be used to write as well as read.
    DATA_T& mget(unsigned int address);
    unsigned int size(unsigned int address);
/*
#ifdef RAM_FIFO
    // RAM-FIFO Push operation for list of words
    void mpush(unsigned int address,Word_List data);
    // RAM-FIFO Shift operation for list of words
    Word_List mshift(unsigned int address);
        // RAM-FIFO Head operation for list of words
    Word_List mhead(unsigned int address);
#endif // RAM_FIFO
*/
    /// Check if address is in use
    bool has(unsigned int address);

    /// Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
    void remove(unsigned int address);
    /// For monitoring
    unsigned int utilized(void);

}; // end of Store class definition



//------------------------------------------------------------------------------
//  SC_Store::mput()
//------------------------------------------------------------------------------

//* List Write operation
template <typename DATA_T >
void SC_Store<DATA_T>::mput(unsigned int address,DATA_T data)
{
    SC_Store::store[address]=data;
}

//------------------------------------------------------------------------------
//  SC_Store::mget()
//------------------------------------------------------------------------------

//* List Read operation
template <typename DATA_T >
DATA_T& SC_Store<DATA_T>::mget(unsigned int address)
{
    return SC_Store::store[address];
}

//------------------------------------------------------------------------------
//  SC_Store::mget()
//------------------------------------------------------------------------------

//* List Read operation
template <typename DATA_T >
unsigned int SC_Store<DATA_T>::size(unsigned int address)
{
    return SC_Store::store[address].size();
}


//------------------------------------------------------------------------------
//  SC_Store::has()
//------------------------------------------------------------------------------

//* Check if address is in use
template <typename DATA_T >
bool SC_Store<DATA_T>::has(unsigned int address)
{
    return (SC_Store::store.count(address)==1);
}

//------------------------------------------------------------------------------
//  SC_Store::remove()
//------------------------------------------------------------------------------

//* Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
template <typename DATA_T >
void SC_Store<DATA_T>::remove(unsigned int address)
{
    map_iter iter=SC_Store::store.find(address);
    if (iter!=SC_Store::store.end())
    {
        SC_Store::store.erase(iter);
    }
}

//------------------------------------------------------------------------------
//  SC_Store::utilized()
//------------------------------------------------------------------------------

//* For monitoring
template <typename DATA_T >
unsigned int SC_Store<DATA_T>::utilized(void)
{
    return SC_Store::store.size();
}


//==============================================================================
//  CLASS: SYSTEMC STORAGE MODULE FOR LISTS
//==============================================================================

//! The SystemC Store class derived from the base SC_Store class and extended for list objects

    /*!
       Dervied on SC_Store, which is based on the SBA::Store class.
       but not in any way linked to C++ classes, (i.e. no inclusion or linking to C++ classes needed)
       The SC_Store is extended
       to include put and get functions which are relevant only if the unit data type in the store
       is a composite data type (e.g. Word_List as used in SBA_C++ model.
    */
template <typename DATA_T >
class SC_Store_List    :
    public SC_Store<DATA_T>
{
public:
    /// Write operation (pushing data into a list object)
    void put(unsigned int address, DATA_T data);
    /// Read operation(popping data off a list object)
    DATA_T get(unsigned int address);

};//class: SC_Store_List

//------------------------------------------------------------------------------
//  SC_Store_List::put()
//------------------------------------------------------------------------------
//*  Write operation
template <typename DATA_T >
void SC_Store_List<DATA_T>::put(unsigned int address, DATA_T data) {
    SC_Store_List::store[address].pop_back();
    SC_Store_List::store[address].push_back(data);
}

//------------------------------------------------------------------------------
//  SC_Store_List::get()
//------------------------------------------------------------------------------
//*  Read operation
template <typename DATA_T >
DATA_T SC_Store_List<DATA_T>::get(unsigned int address) {
    return SC_Store_List::store[address].at(0);
}


//==============================================================================
//  CLASS: SYSTEMC MEMORY MODULE (For List objects)
//==============================================================================


//! The SystemC class for storing list objects
    /*!
       This is derived from SC_Store_List, and contains definition put/get methods
       which assume that the objects stored in the Memory are of type SBA::Word_List
       (or similar) and require access to submembers of list-objects stored in memory
       not in any way linked to C++ classes, (i.e. no inclusion or linking to C++ classes needed)
    */
template <typename ADDR_T, typename DATA_T >
class SC_Memory_List :
    public sc_module    ,
    public SC_Store_List<DATA_T> ,
    public SC_Memory_if<ADDR_T, DATA_T>

{
public:
    // ---------------------------- PORTS --------------------------------------
    // two exports for dual-ported memory
    sc_export<SC_Memory_if<ADDR_T, DATA_T> > xpwr_1;
    sc_export<SC_Memory_if<ADDR_T, DATA_T> > xpwr_2;

    // ---------------------------- Sub-Modules --------------------------------
    //SBA::Memory my_memory;  //!< Using Memory from C++ Model

    // ---------------------------- METHODS ------------------------------------
    // These SC_Memory_if methods need to be implemented in this hierarchichal channel

    // RAM Write operation
    void write(ADDR_T, DATA_T);

    // RAM Read operation
    // Return by reference, so read operation can be used to write as well
    // (as used in the overloaded [] operator
    DATA_T& read(ADDR_T);
    unsigned int size(ADDR_T);
    // Check if address is free
    bool is_free(ADDR_T);

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Memory_List"; }

    // overloading the subscript '[]' operator
    DATA_T&         operator [] (ADDR_T index)         { return SC_Store<DATA_T>::store[index]; } //non-const
    //const DATA_T&   operator [] (ADDR_T index) const   { return store[index]; } //const



    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Memory_List);
    SC_Memory_List(sc_module_name nm) : sc_module(nm)
    {
        // ************ Instantiaing ************
        //my_memory = new SBA::Memory;

        // ************ Bindings ************

        // since this hierarchichal channel implements the transport interface
        // so we bind the exports to *this

        xpwr_1.bind(*this);
        xpwr_2.bind(*this);

        // creation debug message..
        const_debug_msg(name(),kind());

    }
};/* class: SC_Memory_List */

//------------------------------------------------------------------------------
//  SC_Memory_List :: write()
//------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
void SC_Memory_List<ADDR_T, DATA_T> :: write(ADDR_T addr, DATA_T data)
{
    mput(addr, data);
    //my_memory.write(addr, data);
    	unsigned int nwords =data.size();// sizeof(data)>>2;
    	 wait(nwords*_CLK_P, _CLK_U); // delay here, depending on data size
//    	OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//                << "Data of " << nwords <<" Words written to RAM " << name() << endl;

}// funct: SC_Memory_List :: write()

//------------------------------------------------------------------------------
//  SC_Memory_List :: read()
//------------------------------------------------------------------------------
// Return by reference, so read operation can be used to write as well
// (as used in the overloaded [] operator
template <typename ADDR_T, typename DATA_T>
DATA_T& SC_Memory_List<ADDR_T, DATA_T> :: read(ADDR_T addr)
{

    //DATA_T data = my_memory.read(addr);
    DATA_T data = mget(addr);
	unsigned int nwords =data.size();
	wait(nwords*_CLK_P , _CLK_U); // delay here, depending on data size
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//                << "Data of " << nwords <<" Words read from RAM " << name() << endl;

    //return(data);
    //return(mget(addr)); // since returning by reference so cant return the temporary local variable
                        // hence return the actual data.
    return( SC_Store<DATA_T>::store[addr] ); // since returning by reference so cant return the temporary local variable
                        // hence return the actual data.
}// funct: SC_Memory_List :: read()

//------------------------------------------------------------------------------
//  SC_Memory_List :: size()
//------------------------------------------------------------------------------
// Return by reference, so read operation can be used to write as well
// (as used in the overloaded [] operator
template <typename ADDR_T, typename DATA_T>
unsigned int SC_Memory_List<ADDR_T, DATA_T> :: size(ADDR_T addr)
{

    DATA_T data = mget(addr);
    return( SC_Store<DATA_T>::store[addr].size() ); // since returning by reference so cant return the temporary local variable
                        // hence return the actual data.
}// funct: SC_Memory_List :: size()


//------------------------------------------------------------------------------
//  SC_Memory_List :: is_free()
//------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
bool SC_Memory_List<ADDR_T, DATA_T> :: is_free(ADDR_T addr)
{
    //wait(_CLK_P, _CLK_U); // delay here??
    return (has(addr)?false:true);
    //return(my_memory.is_free(addr) );
}// funct: SC_Memory_List :: is_free()






//==============================================================================
//  CLASS: SYSTEMC MEMORY MODULE (For primitive objects)
//==============================================================================


//! The SystemC memory class for storing primitive objects (i.e. not lists)
    /*!
       Derived from SC_Store and does not contain the methods to access data inside list objects
       (put / get).
       USe this for makign Register_set memory and other similar memroies where either data
       is primitive or if lists, then no need for accessing individual members.
       not in any way linked to C++ classes, (i.e. no inclusion or linking to C++ classes needed)
    */
template <typename ADDR_T, typename DATA_T >
class SC_Memory :
    public sc_module    ,
    public SC_Store<DATA_T> ,
    public SC_Memory_if<ADDR_T, DATA_T>

{
public:
    // ---------------------------- PORTS --------------------------------------
    // two exports for dual-ported memory
    sc_export<SC_Memory_if<ADDR_T, DATA_T> > xpwr_1;
    sc_export<SC_Memory_if<ADDR_T, DATA_T> > xpwr_2;

    // ---------------------------- Sub-Modules --------------------------------
    //SBA::Memory my_memory;  //!< Using Memory from C++ Model

    // ---------------------------- METHODS ------------------------------------
    // These SC_Memory_if methods need to be implemented in this hierarchichal channel

    // RAM Write operation
    void write(ADDR_T, DATA_T);

    // RAM Read operation
    // Return by reference, so read operation can be used to write as well
    // (as used in the overloaded [] operator
    DATA_T& read(ADDR_T);
    unsigned int size(ADDR_T);
    // Check if address is free
    bool is_free(ADDR_T);

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Memory"; }

    // overloading the subscript '[]' operator
    DATA_T&         operator [] (ADDR_T index)         { return SC_Store<DATA_T>::store[index]; } //non-const
    //const DATA_T&   operator [] (ADDR_T index) const   { return store[index]; } //const


    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Memory);
    SC_Memory(sc_module_name nm) : sc_module(nm)
    {
        // ************ Instantiaing ************
        //my_memory = new SBA::Memory;

        // ************ Bindings ************

        // since this hierarchichal channel implements the transport interface
        // so we bind the exports to *this

        xpwr_1.bind(*this);
        xpwr_2.bind(*this);

        // creation debug message..
        const_debug_msg(name(),kind());

    }
};/* class: SC_Memory */

//------------------------------------------------------------------------------
//  SC_Memory::write()
//------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
void SC_Memory<ADDR_T, DATA_T> :: write(ADDR_T addr, DATA_T data)
{
    mput(addr, data);
	unsigned int nwords =sizeof(data)>>2;;
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//                << "Data of " << nwords <<" Words written to single-Word RAM " << name() << endl;
    //wait(nwords*_CLK_P , _CLK_U); // delay here, depending on data size
    //my_memory.write(addr, data);
    //wait((sizeof(data)*_CLK_P)>>2 , _CLK_U); // delay here, depending on data size
}// funct: SC_Memory :: write()

//------------------------------------------------------------------------------
//  SC_Memory::read()
//------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
DATA_T& SC_Memory<ADDR_T, DATA_T> :: read(ADDR_T addr)
{
    //DATA_T data = my_memory.read(addr);
    DATA_T data = mget(addr);
	unsigned int nwords =sizeof(data)>>2;
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//                << "Data of " << nwords <<" Words read from single-Word RAM " << name() << endl;
    //wait(nwords*_CLK_P , _CLK_U); // delay here, depending on data size
    //wait((sizeof(data)*_CLK_P)>>2 , _CLK_U); // delay here, depending on data size
    //return(data);
    //return SC_Store::store[address];
    return( SC_Store<DATA_T>::store[addr] ); // since returning by reference so can't return the temporary local variable
                        // hence return the actual data.
}// funct: SC_Memory :: read()

//------------------------------------------------------------------------------
//  SC_Memory :: size()
//------------------------------------------------------------------------------
// Return by reference, so read operation can be used to write as well
// (as used in the overloaded [] operator
template <typename ADDR_T, typename DATA_T>
unsigned int SC_Memory<ADDR_T, DATA_T> :: size(ADDR_T addr)
{
    return( 1 );
}// funct: SC_Memory :: size()

//------------------------------------------------------------------------------
//  SC_Memory::is_free()
//------------------------------------------------------------------------------
template <typename ADDR_T, typename DATA_T>
bool SC_Memory<ADDR_T, DATA_T> :: is_free(ADDR_T addr)
{
    //wait(_CLK_P, _CLK_U); // delay here??
    return (has(addr)?false:true);
    //return(my_memory.is_free(addr) );
}// funct: SC_Memory :: is_free()

}// namespace: SC_SBA

#endif /* SC_MEMORY_H_ */

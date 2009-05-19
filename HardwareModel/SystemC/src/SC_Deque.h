/*
********************************************************************************
                 |
  File Name      | SC_Deque.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 09-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC class implementing arg_addresses, but may be used for other data containers
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

#ifndef SC_DEQUE_H_
#define SC_DEQUE_H


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
//  CLASS: SC_Deque
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! A SC wrapper around an stl::deque object, as a general purpose container. Created for arg_addresses
    /*!
       It contains all access methods that would be needed by an SC_Stack or SC_Fifo object.
       So it may be used to instantite stacks and Fifos too..
       just that it would be an overkill.
       Similarly, if we do not use "read" or "write" methids, but just [], then
       this class can be used to instantiate memories as well. Again, it would be an
        overkill.
       Note that [] is overloaded and can be used as it can be used for a stl::deque object.
       NOTE: and TODO:
       The pop methods do not check if its empty!
    */

template <typename DATA_T>
class SC_Deque :
    public sc_module    ,
    public SC_Deque_if<DATA_T>
{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_Deque_if<DATA_T> > xpwr_1; // Export for access to deque
    sc_export<SC_Deque_if<DATA_T> > xpwr_2; // Export for access to deque

    // ---------------------------- Sub-Modules --------------------------------
    deque<DATA_T> my_deque;     //!< The local deque object

    // ---------------------------- METHODS ------------------------------------

    DATA_T shift()
    {
        DATA_T item = my_deque.front();
        my_deque.pop_front();
        //wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
    // WV: to avoid double counting, it makes more sense to spend no time on shift
    // to be on the safe side we spend 1 cycle
    wait( _CLK_P , _CLK_U);
        return item;
    }

    void unshift(DATA_T& item)
    {
        wait( _CLK_P , _CLK_U);
        my_deque.push_front(item);
        wait( ( ( sizeof(item)>>2 )-1 )*_CLK_P , _CLK_U);
    }

    void push(DATA_T& item)
    {
        //wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
        wait( _CLK_P , _CLK_U);
        my_deque.push_back(item);
        wait( ( ( sizeof(item)>>2 )-1 )*_CLK_P , _CLK_U);
    }

    DATA_T pop()
    {
        DATA_T item = my_deque.back();
        my_deque.pop_back();
        //wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
        //wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
    // WV: to avoid double counting, it makes more sense to spend no time on pop
    // to be on the safe side we spend 1 cycle
    wait( _CLK_P , _CLK_U);
        return item;
    }

    // read_all function needed to enable an overloaded = operator in the
    // interface and port classes for SC_Deque, such that the complete deque object
    // can be initialized in one go
    deque<DATA_T>& read_all ()
    {
        return my_deque; // return by reference, so that deque values can be changed
    } //non-const

    // implicit cast overloaded to assign to Word_List
    operator Word_List& ()
    {
        return my_deque;
    }

    // overloading the subscript '[]' operator since deque is accessed this way too...
    DATA_T& operator [] (unsigned int index)
    {
        return my_deque[index];
    } //non-const

    // "at" does same functionality as [] (apparentlt additionall it throws exception for invalid values)
    // but we simply use the [] opreator of deque, make things simple and uniform
    DATA_T& at (unsigned int index)
    {
        return my_deque[index];
    } //non-const

    // overloading the assignment operator for SC_Deque, so that it accepts deque<DATA_T> type
    // to enable initialization of local stl::deque object
    void operator = (const deque<DATA_T>& rhs_)
    {
        //wait(_CLK_P, _CLK_U); // TODO: How much wait here?
        my_deque = rhs_;
    }

    unsigned int length() { return my_deque.size(); }
    unsigned int size()   { return my_deque.size(); }
    //consume time for clear()?
    void         clear()  { my_deque.clear(); }


    virtual const char* kind() const
        { return "SC_Deque"; }
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Deque);
    SC_Deque(sc_module_name nm):
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
};/* class: SC_Deque */


} /* namespace: SC_SBA */


#endif /* SC_DEQUE_H_ */

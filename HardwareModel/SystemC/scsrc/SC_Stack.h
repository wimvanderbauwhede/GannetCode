/*
********************************************************************************
                 |
  File Name      | SC_Stack.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 09-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC class for implementing Stacks
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_STACK_H_
#define SC_STACK_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
// DEFS
//------------------------------------------------------------------------------


//==============================================================================
//  CLASS: SC_Stack
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This the SC model of SC_Stack
    /*!
       Currently simply a wrapper around an SBA::Stack object
    */
template <unsigned int SIZE>
class SC_Stack :
    public sc_module    ,
    public SC_Stack_if
{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_Stack_if > xpwr_1; // Export for access to Stack
    sc_export<SC_Stack_if > xpwr_2; // Export for access to Stack

    // ---------------------------- Sub-Modules --------------------------------
    SBA::Stack<SIZE> my_stack;//!< The local SBA::Stack object that contains the memory element
                                //!< also implements all the access methods as defined in SC_Stack_if

    // ---------------------------- METHODS ------------------------------------
    // The following SC_Stack_if interface methods are implemented in this module and exported
    // They correspond to the access methods of the SBA::Stack class.


    void            push        (unsigned int item);
    void            push_back   (unsigned int item);
    unsigned int    pop         ();                     //* Pop an address to use off the stack.
    bool            empty       ();                     //* Check if stack is empty
    unsigned int    size        ();

    virtual const char* kind() const
        { return "SC_Stack"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Stack);
    SC_Stack(sc_module_name nm, unsigned int offset) :
        sc_module(nm)   ,
        my_stack (offset)
    {
        // ************ Instantiaing ************

        // ************ Bindings ************
        // Bind exports to *this, which is a hierarchichal channel that implements the SC_SubtaskList_if interface
        xpwr_1.bind(*this);
        xpwr_2.bind(*this);

        // creation debug message..
        const_debug_msg(name(),kind());
    }
};/* class: SC_Stack */

//------------------------------------------------------------------------------
//  SC_Stack::push()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
void SC_Stack<SIZE> :: push(unsigned int item)
{
    wait( _CLK_P, _CLK_U);
    my_stack.push(item);
}// funct: SC_Stack :: push()

//------------------------------------------------------------------------------
//  SC_Stack::push_back()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
void SC_Stack<SIZE> :: push_back(unsigned int item)
{
    // wait depending on size
    wait( _CLK_P, _CLK_U);
    my_stack.push_back(item);
}// funct: SC_Stack :: push_back()

//------------------------------------------------------------------------------
//  SC_Stack::pop()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
unsigned int SC_Stack<SIZE> :: pop()
{
    unsigned int item = my_stack.pop();
    wait( _CLK_P, _CLK_U);
    return (item);
}// funct: SC_Stack :: pop()

//------------------------------------------------------------------------------
//  SC_Stack::empty()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
bool SC_Stack<SIZE> :: empty()
{
    bool item = my_stack.empty();
    //wait( _CLK_P, _CLK_U); // wait for one clock cycle
    return (item);
}// funct: SC_Stack :: empty()

//------------------------------------------------------------------------------
//  SC_Stack::size()
//------------------------------------------------------------------------------
template <unsigned int SIZE>
unsigned int SC_Stack<SIZE> :: size()
{
    unsigned int item = my_stack.size();
    //wait( _CLK_P , _CLK_U); // wait for one clock cycle
    return (item);
}// funct: SC_Stack :: size()



} /* namespace: SC_SBA */




#endif /* SC_STACK_H_ */

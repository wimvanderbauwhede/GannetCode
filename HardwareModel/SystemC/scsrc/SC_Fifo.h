/*
********************************************************************************
                 |
  File Name      | SC_Fifo.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 21-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for FIFOs
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


#ifndef SC_FIFO_H_
#define SC_FIFO_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//==============================================================================
//  CLASS: SC_Fifo
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the Fifo class used for various FIFOs around the Service Manager
    /*!
       This SC_Fifo uses an internal tlm::fifo. SBA::Fifo could not be used because we need
       blocking read and write (shift, push) methods in our model. This way, we dont have to poll
       the FIFO until its non-empty/non-full. SBA::Fifo did not give us these blocking methods.
       tlm_fifo uses internal events to provide us with blocking reads/writes, that block until a
       fifo is non-empty/full, and hence save us the hassle of manually checking for this.

       sc_fifo could have been used too but it was giving errors when the DATA_T was SBA::Packet_t
       msot likely because it expects os << operator to be defined for the data type, and it wasnt for Packet_t.

       tlm_fifo seems to have done the  job as required.
    */

template <typename DATA_T, SBA::Word depth>
class SC_Fifo :
    public SC_Fifo_if<DATA_T>   ,
    public sc_module
{
public:
    // ------------------------------------- PORTS  ------------------------------------
    sc_export<SC_Fifo_if<DATA_T> > xpwr_1; // exports. Give access to ALL SBA::Fifo methods
    sc_export<SC_Fifo_if<DATA_T> > xpwr_2; //


private:
    // ------------------------------------- Locals ------------------------------------
    //SBA::Fifo<DATA_T, depth> my_fifo;   //!< The internal FIFO to which the exports are bound
    tlm::tlm_fifo <DATA_T>         my_fifo; //!< The internal FIFO to which the exports are bound

public:
    // ---------------------------Exported Functions -----------------------------------
    // The following SC_Fifo_if interface methods are implemented in this module and exported
    // They correspond to the access methods of the SBA::Stack class.

    DATA_T          shift()             ;
    //void            unshift(DATA_T&)    ;
    void            push(DATA_T&)       ;
    //DATA_T          pop()               ;
    unsigned int    length()            {return (unsigned int) my_fifo.used();}
    unsigned int    size()              {return (unsigned int) my_fifo.used();}
    void            clear()             ;

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Fifo"; }
    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_Fifo<DATA_T, depth> (sc_module_name nm) :
        sc_module(nm)   ,
        my_fifo((int) depth) // if using tlm_fifo
        {
            //!< Bind the interface exports to *this
            //!< since these are implemented in *this hierarchichal channel
            xpwr_1.bind(*this);
            xpwr_2.bind(*this);

            // creation debug message..
            const_debug_msg(name(),kind());
        }
};//class: SC_Fifo

//------------------------------------------------------------------------------
//  SC_Fifo::shift()
//------------------------------------------------------------------------------
/* WV what I would like is that shift() spends 1 cycle, then returns the data, then spends
(sizeof(item)*_CLK_P)>>2 -1 cycles before allowing another shift()
But I see no way to achieve that.
*/

template <typename DATA_T, SBA::Word depth>
DATA_T SC_Fifo<DATA_T, depth> :: shift()
{
    DATA_T item = my_fifo.get();
    //wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
    // WV: to avoid double counting, it makes more sense to spend no time on shift
    // to be on the safe side we spend 1 cycle
    wait( _CLK_P , _CLK_U);
    return (item);
}// funct: SC_Fifo :: shift()

/*
//------------------------------------------------------------------------------
//  SC_Fifo::unshift()
//------------------------------------------------------------------------------
template <typename DATA_T, SBA::Word depth>
void SC_Fifo<DATA_T, depth> :: unshift(DATA_T& item)
{
    wait( sizeof(item)*_CLK_P, _CLK_U);
    my_fifo.unshift(item);
}// funct: SC_Fifo :: unshift()
*/

//------------------------------------------------------------------------------
//  SC_Fifo::push()
//------------------------------------------------------------------------------

/* WV to mimic pilelining, push() spends 1 cycle before doing the actual push, then spends
(sizeof(item)*_CLK_P)>>2 -1 cycles before returning (i.e. before allowing another push() )


*/


template <typename DATA_T, SBA::Word depth>
void SC_Fifo<DATA_T, depth> :: push(DATA_T& item)
{
#ifdef VERBOSE
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << "Data item " << " pushed onto Fifo " << name() << endl;
#endif
//    wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
    wait( _CLK_P , _CLK_U);
    my_fifo.put(item);
    wait( ( ( sizeof(item)>>2 )-1 )*_CLK_P , _CLK_U);

}// funct: SC_Fifo :: push()

/*
//------------------------------------------------------------------------------
//  SC_Fifo::pop()
//------------------------------------------------------------------------------
template <typename DATA_T, SBA::Word depth>
DATA_T SC_Fifo<DATA_T, depth> :: pop()
{
    DATA_T item = my_fifo.pop();
    wait( sizeof(item)*_CLK_P, _CLK_U);
    return (item);
}// funct: SC_Fifo :: pop()
*/

//------------------------------------------------------------------------------
//  SC_Fifo::clear()
//------------------------------------------------------------------------------
template <typename DATA_T, SBA::Word depth>
void SC_Fifo<DATA_T, depth> :: clear()
{

    // tlm_fifo does not have a clear function. So read until empty
    while (my_fifo.used() != 0)
    {
        //wait( sizeof(item)*_CLK_P, _CLK_U); // TODO: Waste cycles here?
        my_fifo.get();
    }
}// funct: SC_Fifo :: clear()

// ============================================================================
// Specialised clas template for Packet_t
// ============================================================================

template < SBA::Word depth>
class SC_Fifo<Packet_t, depth> :
    public SC_Fifo_if<Packet_t>   ,
    public sc_module
{
public:
    // ------------------------------------- PORTS  ------------------------------------
    sc_export<SC_Fifo_if<Packet_t> > xpwr_1; // exports. Give access to ALL SBA::Fifo methods
    sc_export<SC_Fifo_if<Packet_t> > xpwr_2; //


private:
    // ------------------------------------- Locals ------------------------------------
    //SBA::Fifo<DATA_T, depth> my_fifo;   //!< The internal FIFO to which the exports are bound
    tlm::tlm_fifo <Packet_t>         my_fifo; //!< The internal FIFO to which the exports are bound

public:
    // ---------------------------Exported Functions -----------------------------------
    // The following SC_Fifo_if interface methods are implemented in this module and exported
    // They correspond to the access methods of the SBA::Stack class.

	Packet_t          shift()             ;
    //void            unshift(Packet_t&)    ;
    void            push(Packet_t&)       ;
    //DATA_T          pop()               ;
    unsigned int    length()            {return (unsigned int) my_fifo.used();}
    unsigned int    size()              {return (unsigned int) my_fifo.used();}
    void            clear()             ;

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Fifo"; }
    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_Fifo<Packet_t, depth> (sc_module_name nm) :
        sc_module(nm)   ,
        my_fifo((int) depth) // if using tlm_fifo
        {
            //!< Bind the interface exports to *this
            //!< since these are implemented in *this hierarchichal channel
            xpwr_1.bind(*this);
            xpwr_2.bind(*this);

            // creation debug message..
            const_debug_msg(name(),kind());
        }
};//class: SC_Fifo

    //------------------------------------------------------------------------------
    //  SC_Fifo::shift()
    //------------------------------------------------------------------------------
    /* WV what I would like is that shift() spends 1 cycle, then returns the data, then spends
    (sizeof(item)*_CLK_P)>>2 -1 cycles before allowing another shift()
    But I see no way to achieve that.
    */

    template <SBA::Word depth>
    Packet_t SC_Fifo<Packet_t, depth> :: shift()
    {
    	Packet_t item = my_fifo.get();
        //wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
        // WV: to avoid double counting, it makes more sense to spend no time on shift
        // to be on the safe side we spend 1 cycle
        wait( _CLK_P , _CLK_U);
        return (item);
    }// funct: SC_Fifo :: shift()

    //------------------------------------------------------------------------------
    //  SC_Fifo::push()
    //------------------------------------------------------------------------------

    /* WV to mimic pilelining, push() spends 1 cycle before doing the actual push, then spends
    (sizeof(item)*_CLK_P)>>2 -1 cycles before returning (i.e. before allowing another push() )
    */

    template <SBA::Word depth>
    void SC_Fifo<Packet_t, depth> :: push(Packet_t& item)
    {
    	unsigned int nwords = item.size(); //( sizeof(item)>>2 );
#ifdef VERBOSE
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
                << "Data item of " << nwords <<" Words pushed onto Fifo " << name() << endl;
#endif
    //    wait( (sizeof(item)*_CLK_P)>>2 , _CLK_U);
        wait( _CLK_P , _CLK_U);
        my_fifo.put(item);
        wait( ( nwords-1 )*_CLK_P , _CLK_U);

    }// funct: SC_Fifo :: push()

    //------------------------------------------------------------------------------
    //  SC_Fifo::clear()
    //------------------------------------------------------------------------------
    template <SBA::Word depth>
    void SC_Fifo<Packet_t, depth> :: clear()
    {

        // tlm_fifo does not have a clear function. So read until empty
        while (my_fifo.used() != 0)
        {
            //wait( sizeof(item)*_CLK_P, _CLK_U); // TODO: Waste cycles here?
            my_fifo.get();
        }
    }// funct: SC_Fifo :: clear()

} //namespace: SC_SBA
#endif /* SC_FIFO_H_ */


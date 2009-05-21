/*
********************************************************************************
                 |
  File Name      | SC_Registers.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 19-Nov-2008. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | SystemC classes for Registers used in the Service manager
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_REGISTERS_H_
#define SC_REGISTERS_H_
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
//  CLASS: SC RUNTIME
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC class for Register
    /*!
       It is a hierarchical channel that uses an internal sc_signal object to
       implement the sc_signal_inout_if
       hierarchical class has been used to allow later inclusion of more functionality
       into the SC_Register class.
       Note that assignment operator '=' has been overloaded
    */

template <typename T>
class SC_Register :
    public sc_module    ,
    public SC_reg_out_if_d<T>
{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_reg_out_if_d<T> >  xpwr;   // inout export for accessing register (timed). Only read/write/value_changed_event exported
    sc_export<sc_signal_out_if<T> > xpwr_ut;// inout export for accessing register (un-timed). Gives access to ALL sc_signal methods
    // ---------------------------Exported Functions -----------------------------------
    // this form part of the SC_reg_out_if_d interface

    void            write               ( const T& );
    T&              read                ();
    const sc_event& value_changed_event () const;

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Register"; }


    // NOTE that assignment operator '=' has been overloaded and should be used.
    // Interface/port/arbiter classes have been overloaded too, use assignment operator
    // through port as well
    void operator = (const T& rhs_)
    {
        wait(_CLK_P, _CLK_U); // wait for one clock period
        my_signal.write(rhs_);
        //return *this;
    }

    // SC_Register has implicit cast implemented so that
    // when it is used in a function argument, it returns its data value
    operator T& ()
    {
        //T local = my_signal.read();
        //return local; // not directly returning from function becase it returns const
        local_non_const = my_signal.read();;
        return local_non_const;
        //return my_signal.read();
    }


    // overload the equality operator as well, when compared with T type
    bool operator == (const T& rhs)
    {
        return ( my_signal.read() == rhs );
    }

    // overload the equality operator, if compared with SC_Register<T> type
    bool operator == (const SC_Register<T>& rhs)
    {
        return ( my_signal.read() == rhs.value() );
    }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Register);
    // no arguments except name
    SC_Register(sc_module_name nm) :
        sc_module(nm)
    {
        xpwr_ut.bind    (my_signal);// bind export to local channel that implements the exported interface
        xpwr.bind       (*this);    // bind this export to THIS hierarchichal channel that implements delayed access to the local sc_signal channel

        // creation debug message..
        const_debug_msg(name(),kind());
    }

    // constructor with initial value passed as argument
    SC_Register(    sc_module_name nm,
                    T& val_) :
        sc_module(nm)
    {
        xpwr_ut.bind    (my_signal);// bind export to local channel that implements the exported interface
        xpwr.bind       (*this);    // bind this export to THIS hierarchichal channel that implements delayed access to the local sc_signal channel

        // write initial value
        my_signal = val_;

        // creation debug message..
        const_debug_msg(name(),kind());
    }


private:
    sc_signal<T> my_signal; // the local channel that implements the exported interface
    T local_non_const;
};/* class: SC_Register */

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : overloading the '=' operator for SC_Register
//* Object              :
//* Input Parameters    :
//* Output Parameters   :
//*---------------------------------------------------------------------------------------------------------
/*
template <typename T>
T& SC_Register<T> :: operator = (const T& rhs)
{
    wait(_CLK_P, _CLK_U); // wait for one clock period
    my_signal.write(rhs);
    return *this;
}// '=' operator overload
*/



//*---------------------------------------------------------------------------------------------------------
//* Function Name       : write_d
//* Object              : Extend the sc_signal.write to include read/write delay
//* Input Parameters    : value to be put in signal
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename T>
void SC_Register<T> :: write  ( const T& val_)
{
    wait(_CLK_P, _CLK_U); // wait for one clock period
    my_signal.write(val_);
	   OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
     << "SC_Register "<<name()<<" write() called, value is now "<< my_signal.read()<< "\n";

}//write()

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : delayed_put
//* Object              : Extend the tlm_fifo.put() to include read/write delay
//* Input Parameters    : value to be put in fifo
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename T>
T& SC_Register<T> :: read  ()
{
    wait(_CLK_P, _CLK_U); // wait for one clock period
    // returning local non_const for compatibility with overloaded T&() implicit cast operator
    // which is implemented in the specialized port.
    local_non_const = my_signal.read();
    return local_non_const;
    //return(my_signal.read());

}//read()

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : value_changed_event
//* Object              : event that triggers when there is a change in value
//* Input Parameters    :
//* Output Parameters   : reference to signal (can be used in an dynamic sensitivity list (wait)
//*---------------------------------------------------------------------------------------------------------

template <typename T>
const sc_event& SC_Register<T> :: value_changed_event() const
{
	   OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
        << "SC_Register "<<name()<<" value_changed_event() called, value is now "<< my_signal.read()<< "\n";
    return(my_signal.value_changed_event());
}//value_changed_event()

}//namespace: SC_SBA

#endif /* SC_REGISTERS_H_ */

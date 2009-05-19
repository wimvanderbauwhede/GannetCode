/* ---- DEPRECATED ---
********************************************************************************
                 |
  File Name      | SC_SBA_Fifo.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC module for FIFOs (based on tlm_fifos). DEPRECATED
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
                 | WN_20081221: DEPRECATED. The new class (SC_Fifo) is based on SBA::Fifo
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


#ifndef SC_SBA_FIFO_H_
#define SC_SBA_FIFO_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//==============================================================================
//  CLASS: SC_SBA_Fifo
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the Fifo class used for various FIFOs around the Service Manager
    /*!
       This SC_SBA_Fifo is uses an internal tlm_fifo to export put and get interfaces
       TODO: Why not use a sc_fifo? Any real benefit of tlm_fifo? Debug interface?
    */
template <typename DATA_T>
class SC_SBA_Fifo : public SC_fifo_delayed_put_if<DATA_T> ,
                    public SC_fifo_delayed_get_if<DATA_T> ,
                    public sc_module
{
public:
    // ------------------------------------- PORTS  ------------------------------------
    sc_export<SC_fifo_put_if<DATA_T> > xpw_fifo; // untimed exports. Give access to ALL tlm_fifo methods as defined by tlm interfaces
    sc_export<SC_fifo_get_if<DATA_T> > xpr_fifo; // "
    sc_export<SC_fifo_delayed_put_if<DATA_T> > xpw_fifo_d; // delayed/timed exports. Only export push/shift  interface
    sc_export<SC_fifo_delayed_get_if<DATA_T> > xpr_fifo_d; // "

    // a debug interface here as well?


private:
    // ------------------------------------- Locals ------------------------------------
//    sc_time unit_delay;           //!< The unit delay per DATA_T element
    tlm::tlm_fifo<DATA_T> my_fifo;  //!< The internal FIFO to which the exports are bound

public:
    // ---------------------------Exported Functions -----------------------------------
    // delayed_put interface (blocking put with processing delay)
    void delayed_put( const DATA_T& );

    // delayed_get interface (blocking get with processing delay)
    DATA_T delayed_get( tlm::tlm_tag<DATA_T> *t = 0 );

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_SBA_Fifo"; }


    // ---------------------------- CONSTRUCTORS ---------------------------------------
    SC_SBA_Fifo<DATA_T> (sc_module_name nm) :
        sc_module(nm)
        {

            //!< Bind put and get exports to tlm_fifo.
            //!< tlm_fifo implements the put and get interfaces
            xpw_fifo.bind(my_fifo);
            xpr_fifo.bind(my_fifo);

            //!< Bind the delayed interface exports to *this
            //!< since these are implemented in *this hierarchichal channel
            xpw_fifo_d.bind(*this);
            xpr_fifo_d.bind(*this);

            // creation debug message..
            const_debug_msg(name(),kind());


        }

      SC_SBA_Fifo<DATA_T> (sc_module_name nm, int size_ = 1) :
        sc_module(nm)   ,
        my_fifo(size_)
        {
            //!< Bind put and get exports directly to tlm_fifo.
            //!< tlm_fifo implements the put and get interfaces
            xpw_fifo.bind(my_fifo);
            xpr_fifo.bind(my_fifo);

            //!< Bind the delayed interface exports to *this
            //!< since these are implemented are *this hierarchichal channel
            //!< the delayed_put/get local functions then access the tlm_fifo
            //!< after introducint the appropriate delay
            xpw_fifo_d.bind(*this);
            xpr_fifo_d.bind(*this);

            // creation debug message..
            const_debug_msg(name(),kind());

        }


};//class: SC_SBA_Fifo

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : delayed_put
//* Object              : Extend the tlm_fifo.put() to include read/write delay
//* Input Parameters    : value to be put in fifo
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

template <typename DATA_T>
void SC_SBA_Fifo<DATA_T> :: delayed_put (const DATA_T& val_ )
{
    wait(sizeof(val_)*_CLK_P, _CLK_U);
    my_fifo.put(val_);  // the reverse of get; first wait, then write, so that
                        // FIFO becomes non-empty AFTER the write delay

}//delayed_put()
//*---------------------------------------------------------------------------------------------------------
//* Function Name       : delayed_get
//* Object              : Extend the tlm_fifo.get() to include read/write delay
//* Input Parameters    : reference of variable into which data should be read
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

// modelled on the T get (...) function of tlm_fifo
// so that that this delayed_get function can be used
// in exactly the same way as the T get(...) function
template <typename DATA_T>
DATA_T SC_SBA_Fifo<DATA_T> :: delayed_get ( tlm::tlm_tag<DATA_T> * )
{
    DATA_T val_;
    val_ = my_fifo.get();   // first read and THEN wait. Otherwise it may be the wait time expires
                            // while the reader is blocked because of the FIFO being empty
                            // in this case, the read will be immediate upon FIFO becoming non-empty
                            // which is not the correct behaviour
    wait(sizeof(val_)*_CLK_P, _CLK_U);
    return(val_);
}//delayed_get()


} //namespace: SC_SBA

#endif /* SC_SBA_FIFO_H_ */


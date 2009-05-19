/*
********************************************************************************
                 |
  File Name      | SC_transmit_packets_gw.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 26-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'transmit_packets' class for the Gateway
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


#ifndef SC_TRANSMIT_PACKETS_GW_H_
#define SC_TRANSMIT_PACKETS_GW_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;


//==============================================================================
//  CLASS: TRANSMIT PACKETS (FOR GATEWAY)
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "transmit packets" module for the gateway. REDUNDANT since SC_transmit_packets for ServiceManager is usable here as well.
    /*!
       Detailed description here...

    */
template<typename PACKET_T>
class SC_transmit_packets_gw : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if <PACKET_T>  tranceiver_tx_fifo; //!<
    port_SC_Fifo_if <PACKET_T>  tx_fifo;          //!<

    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_transmit_packets_gw"; }
    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_transmit_packets_gw);
    SC_transmit_packets_gw(sc_module_name nm) : sc_module(nm)
    {
        SC_THREAD(do_proc);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());

    }//constructor
private:
};//class: SC_transmit_packets_gw

//==============================================================================
//  DO_PROC()
//==============================================================================
template<typename PACKET_T>
void SC_transmit_packets_gw<PACKET_T> :: do_proc()
    {
    }// funct: SC_transmit_packets_gw :: do_proc()

} //namespace: SC_SBA

#endif /* SC_TRANSMIT_PACKETS_GW_H_ */

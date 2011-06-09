/* **************** DEPERACATED ******************
********************************************************************************
                 |
  File Name      | SC_transmit_packets.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'transmit_packets' class
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */


#ifndef SC_TRANSMIT_PACKETS_H_
#define SC_TRANSMIT_PACKETS_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS:TRANSMIT PACKETS
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "transmit packets" module - DEPRECATED
    /*!
       Detailed description here...

    */
class SC_transmit_packets : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    //sc_port<SC_fifo_delayed_put_if<PACKET_T> >  tx_fifo    ; //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if<Packet_t>   tx_fifo;        //!< read port for access to tx_fifo
    port_SC_Fifo_if<Packet_t>   transceiver_tx_fifo; //!< write port for access to traceiver.tx_fifo

    // ---------------------------- Sub-Modules --------------------------------


	// ---------------------------- METHODS ------------------------------------
	void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_transmit_packets"; }

	// ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_transmit_packets);
	SC_transmit_packets(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);
		//dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
	}
private:
};//class: SC_transmit_packets

//==============================================================================
//	DO_PROC()
//==============================================================================
void SC_transmit_packets :: do_proc()
{
}// funct: SC_transmit_packets :: do_proc()



} //namespace: SC_SBA

#endif /* SC_TRANSMIT_PACKETS_H_ */

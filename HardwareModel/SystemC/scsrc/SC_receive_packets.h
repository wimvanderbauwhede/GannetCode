/*
********************************************************************************
                 |
  File Name      | SC_receive_packets.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'receive packets' class
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_RECEIVE_PACKETS_H_
#define SC_RECEIVE_PACKETS_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"
//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: RECEIVE PACKETS
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "receive packets" module
    /*!
       Detailed description here...
    */
class SC_receive_packets : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if<Packet_t>   transceiver_rx_fifo; //!< read port for tranceiver rx fifo

    port_SC_Fifo_if<Packet_t>   data_fifo;                 //!< write port
    port_SC_Fifo_if<Packet_t>   subtask_code_fifo;         //!< write port
    port_SC_Fifo_if<Packet_t>   request_fifo;              //!< write port
    port_SC_Fifo_if<Packet_t>   subtask_reference_fifo;    //!< write port

    port_SC_reg_if<SBA::Service > service;
	// ---------------------------- METHODS ------------------------------------
	void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_receive_packets"; }

	// ---------------------------- CONSTRUCTOR --------------------------------

	SC_HAS_PROCESS(SC_receive_packets);

	SC_receive_packets(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);

		// creation debug message..
        const_debug_msg(name(),kind());
	}
private:
	void demux_packets_by_type(SBA::Packet_t&);
};//class: SC_receive_packets

//==============================================================================
//	DO_PROC()
//==============================================================================
void SC_receive_packets :: do_proc() {
#include "../gensrc/ServiceManager/receive_packets.cc"
}// funct: SC_receive_packets :: do_proc()

void SC_receive_packets :: demux_packets_by_type(Packet_t& packet) {
#include "../gensrc/ServiceManager/demux_packets_by_type.cc"
} // funct: SC_receive_packets :: demux_packets_by_type()

} //namespace: SC_SBA
#endif /* SC_RECEIVE_PACKETS_H_ */

/*
********************************************************************************
                 |
  File Name      | SC_receive_packets_gw.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 26-Jan-2009. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'receive packets' class for GatewayTile
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_RECEIVE_PACKETS_GW_H_
#define SC_RECEIVE_PACKETS_GW_H_


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
//  CLASS: RECEIVE PACKETS GATEWAY
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "receive packets" module for use in the Gateway
    /*!
       Detailed description here...
    */
class SC_receive_packets_gw : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if <Packet_t>  tranceiver_rx_fifo; //!<
    port_SC_Fifo_if <Packet_t>  rx_fifo;          //!<
    port_SC_Fifo_if <Packet_t>  request_fifo;       //!<

    //    port_SC_Fifo_if<PACKET_T>   tranceiver_rx_fifo; //!< read port for tranceiver rx fifo
//    port_SC_Fifo_if<PACKET_T>   data_fifo;                 //!< write port
//    port_SC_Fifo_if<PACKET_T>   request_fifo;              //!< write port

    // ---------------------------- METHODS ------------------------------------
    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_receive_packets_gw"; }

    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_receive_packets_gw);

    SC_receive_packets_gw(sc_module_name nm) : sc_module(nm)
    {
        SC_THREAD(do_proc);
        //SC_THREAD(demux_packets_by_type);

        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
};//class: SC_receive_packets_gw

//==============================================================================
//  DO_PROC()
//==============================================================================
void SC_receive_packets_gw :: do_proc()
    {
    }// funct: SC_receive_packets :: do_proc()
} //namespace: SC_SBA

#endif /* SC_RECEIVE_PACKETS_GW_H_ */

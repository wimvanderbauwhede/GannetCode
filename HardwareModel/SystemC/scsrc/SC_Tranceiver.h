/*
********************************************************************************
                 |
  File Name      | SC_Tranceiver.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 17-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC SC_Tranceiver class
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081117: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/



#ifndef SC_TRANCEIVER_H_
#define SC_TRANCEIVER_H_

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
//  CLASS: SERVICE TILE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This the tile
    /*!
       Has one thread that transmits packets whenever there are packets to transmit.
       It does not have a 'read' thread since the network proactively writes
       directly to the transceiver.
    */
template <typename PACKET_T>
class SC_Tranceiver : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    // for writing to network.rx_fifo
    port_SC_Fifo_if <PACKET_T>   network_rx_fifo;
    // the network writes directly into a Tile (i.e. the transceiver's rx_fifo)
    // through the provided export, so no port for reading from network

    // ---------------------------- EXPORTS ------------------------------------
    // Network uses this export to directly write to the tile's transceiver
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo;    //!< Export for read/write to internal Rx FIFO

    // this export is used by the service_managar/gateway to write to transceiver for transmission
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_txfifo;    //!< Export for read/write to internal Tx FIFO

    // ---------------------------- Sub-Modules / Primitives -------------------
    SC_Fifo<PACKET_T, PACKET_FIFO_SZ>           tx_fifo;
    SC_Fifo<PACKET_T, PACKET_FIFO_SZ>           rx_fifo;

    SBA::Service                service;
    SBA::ServiceAddress         address;

    // ---------------------------- METHODS ------------------------------------
    // receive packets not needed because export provided to network which writes to the rx_fifo
    //void receive_packets();
    // If there are any packets in the TX FIFO, dispatch them
    void transmit_packets();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Tranceiver"; }


    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Tranceiver);
    SC_Tranceiver(sc_module_name nm,SBA::Service        s_  ) :
        sc_module(nm),
        tx_fifo("tx_fifo"),
        rx_fifo("rx_fifo"),
        service(s_)
    {

        // *** Bindings ***

        // Binding Rx_Fifo read and write exports to internal Rx FIFO
        xpwr_rxfifo    (rx_fifo.xpwr_1);
        //xpr_rxfifo_d    (rx_fifo.xpr_fifo_d);

        // Binding Tx_Fifo read and write exports to internal Tx FIFO
        xpwr_txfifo    (tx_fifo.xpwr_1);
        //xpr_txfifo_d    (tx_fifo.xpr_fifo_d);

        // Put two packets inside the local rx_fifo for testing
        // using the non-time-consuming method
        //PACKET_T i;

        SC_THREAD(transmit_packets);
        // creation debug message..
        const_debug_msg(name(),kind());
    }


};/* class: SC_Tranceiver */

//==============================================================================
//  DO_PROC()
//==============================================================================
template <typename PACKET_T>
void SC_Tranceiver <PACKET_T>:: transmit_packets()
    {
        // transmit forever
        while(true)
        {
            // blocking read from local tx_fifo. Non-empty tx_fifo indicates a packet ready
            // to be sent to network
            PACKET_T packet = tx_fifo.shift();
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Tranceiver: transmitting packet to NoC"<< endl;
#endif
            /*
            int to=(packet.front()>>8)&0xFF;
            int return_to=packet.front()&0xFF;
            int ctrl=(packet.front()>>26)&0x7;
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") To:"<<to<<"; Return-to:"<<return_to<<";Ctrl:"<<ctrl<<" Length:"<<packet.size()-3 <<endl;
            */
            //			cout << "SC_Tranceiver: transmitting packet to NoC\n";
            // write this packet read from tx_fifo to network
            // the write is through port
            // the port will be connected to the appropriate export
            // that the network provides for storing this source tile's incomig data
            network_rx_fifo.push(packet);
        }
    }// funct: SC_Tile :: do_proc()

} /* namespace: SC_SBA */




#endif /* SC_TRANCEIVER_H_ */

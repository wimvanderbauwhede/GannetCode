/*
********************************************************************************
                 |
  File Name      | SC_Tranceiver.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 17-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC SC_Tranceiver class
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081117: Created.
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



#ifndef SC_TRANCEIVER_H_
#define SC_TRANCEIVER_H_

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
//  CLASS: SERVICE TILE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This the tile
    /*!
       Has one thread that transmits packets whenever there are packets to transmit.
       It does not have a 'read' thread since the network proactively writes
       directly to the tranceiver.
    */
template <typename PACKET_T>
class SC_Tranceiver : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    // for writing to network.rx_fifo
    port_SC_Fifo_if <PACKET_T>   network_rx_fifo;
    // the network writes directly into a Tile (i.e. the tranceiver's rx_fifo)
    // through the provided export, so no port for reading from network

    // ---------------------------- EXPORTS ------------------------------------
    // Network uses this export to directly write to the tile's tranceiver
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo;    //!< Export for read/write to internal Rx FIFO

    // this export is used by the service_managar/gateway to write to tranceiver for transmission
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
        service(s_),
        tx_fifo("tx_fifo"),
        rx_fifo("rx_fifo")
    {
        // *** Instantiaing ***

        //tx_fifo = new SC_SBA_Fifo<PACKET_T>   ("tx_fifo", 20);
        //rx_fifo = new SC_SBA_Fifo<PACKET_T>   ("rx_fifo", 20);

        // *** Bindings ***

        // Binding Rx_Fifo read and write exports to internal Rx FIFO
        xpwr_rxfifo    (rx_fifo.xpwr_1);
        //xpr_rxfifo_d    (rx_fifo.xpr_fifo_d);

        // Binding Tx_Fifo read and write exports to internal Tx FIFO
        xpwr_txfifo    (tx_fifo.xpwr_1);
        //xpr_txfifo_d    (tx_fifo.xpr_fifo_d);

        // Put two packets inside the local rx_fifo for testing
        // using the non-time-consuming method
        PACKET_T i;

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
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Tranceiver: transmitting packet to NoC"<< endl;
#endif
            //			cout << "SC_Tranceiver: transmitting packet to NoC\n";
            // write this packet read from tx_fifo to network
            // the write is through port
            // the port will be connected to the appropriate export
            // that the network provides for storing this source tile's incomig data
            // TODO: is the delay in first reading and then writing means it is doubled?
            network_rx_fifo.push(packet);
        }
    }// funct: SC_Tile :: do_proc()

} /* namespace: SC_SBA */




#endif /* SC_TRANCEIVER_H_ */

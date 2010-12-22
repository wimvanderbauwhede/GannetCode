/*
********************************************************************************
                 |
  File Name      | SC_QuarcTransceiver.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 17-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC SC_QuarcTranceiver class: TRX for the Quarc NoC
-----------------|--------------------------------------------------------------
  Modifications  | WV20101024: Copy from SC_QuarcTransceiver
                 | 
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/

#ifndef SC_TRANSCEIVER_H_
#define SC_TRANSCEIVER_H_

#include "SC_SBA.h"

#define _QUARC_NEXT 0
#define _QUARC_RCROSS 1
#define _QUARC_LCROSS 2
#define _QUARC_PREV 3

#define _QUARC_ERROR -1

using namespace std;

//==============================================================================
//  CLASS: SERVICE TILE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

/*!
   The TRX has one thread that transmits packets whenever there are packets to transmit.
   It does not have a 'read' thread since the network proactively writes
   directly to the transceiver.
*/
template <typename PACKET_T>
class SC_QuarcTransceiver : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    // for writing to network.rx_fifo. This should be renamed to tx_fifo0 etc
    port_SC_Fifo_if <PACKET_T>   network_rx_fifo0;
    port_SC_Fifo_if <PACKET_T>   network_rx_fifo1;
    port_SC_Fifo_if <PACKET_T>   network_rx_fifo2;
    port_SC_Fifo_if <PACKET_T>   network_rx_fifo3;

    port_SC_Config_if           cfg;
    // the network writes directly into a Tile (i.e. the transceiver's rx_fifo)
    // through the provided export, so no port for reading from network

    // ---------------------------- EXPORTS ------------------------------------
    // Network uses this export to directly write to the tile's transceiver
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo0;    //!< Export for read/write to internal Rx FIFO
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo1;    //!< Export for read/write to internal Rx FIFO
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo2;    //!< Export for read/write to internal Rx FIFO
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo3;    //!< Export for read/write to internal Rx FIFO

    sc_export<SC_Fifo_if<PACKET_T> > xpwr_rxfifo; // for Service Manager

    // this export is used by the service_managar/gateway to write to transceiver for transmission
    sc_export<SC_Fifo_if<PACKET_T> > xpwr_txfifo;    //!< Export for read/write to internal Tx FIFO

    // ---------------------------- Sub-Modules / Primitives -------------------
    SC_Fifo<PACKET_T, 1>           rx_fifo0;
    SC_Fifo<PACKET_T, 1>           rx_fifo1;
    SC_Fifo<PACKET_T, 1>           rx_fifo2;
    SC_Fifo<PACKET_T, 1>           rx_fifo3;

    SC_Fifo<PACKET_T, PACKET_FIFO_SZ>           tx_fifo;
    SC_Fifo<PACKET_T, PACKET_FIFO_SZ>           rx_fifo;

    SBA::Service                service;
    SBA::ServiceAddress         address;

    // ---------------------------- METHODS ------------------------------------
    // receive packets needed for delivery/forwarding
    void receive_packets0();
    void receive_packets1();
    void receive_packets2();
    void receive_packets3();
    // If there are any packets in the TX FIFO, dispatch them
    void transmit_packets();

    int calc_quad(int destAddress, int nodeIndex, int netSize);
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_QuarcTransceiver"; }



    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_QuarcTransceiver);
    SC_QuarcTransceiver(sc_module_name nm,SBA::Service        s_  ) :
        sc_module(nm),
        rx_fifo0("rx_fifo0"),
		rx_fifo1("rx_fifo1"),
		rx_fifo2("rx_fifo2"),
		rx_fifo3("rx_fifo3"),
		tx_fifo("tx_fifo"),
        rx_fifo("rx_fifo"),
        service(s_)
    {

        // *** Bindings ***

        // Binding Rx_Fifo read and write exports to internal Rx FIFO
        xpwr_rxfifo0    (rx_fifo0.xpwr_1);
        xpwr_rxfifo1    (rx_fifo1.xpwr_1);
        xpwr_rxfifo2    (rx_fifo2.xpwr_1);
        xpwr_rxfifo3    (rx_fifo3.xpwr_1);

        xpwr_rxfifo    (rx_fifo.xpwr_1);
        //xpr_rxfifo_d    (rx_fifo.xpr_fifo_d);

        // Binding Tx_Fifo read and write exports to internal Tx FIFO
        xpwr_txfifo    (tx_fifo.xpwr_1);
        //xpr_txfifo_d    (tx_fifo.xpr_fifo_d);

        SC_THREAD(transmit_packets);
        SC_THREAD(receive_packets0);
        SC_THREAD(receive_packets1);
        SC_THREAD(receive_packets2);
        SC_THREAD(receive_packets3);
        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
    SBA::ServiceAddress getDst(Word_List p);
    SBA::ServiceAddress getSrc(Word_List p);

};/* class: SC_QuarcTransceiver */

//==============================================================================
//  Methods
//==============================================================================
template <typename PACKET_T>
SBA::ServiceAddress SC_QuarcTransceiver <PACKET_T>::getDst(Word_List p) {
	Word qph=p[0];
	return qph&0xffff;
}

template <typename PACKET_T>
SBA::ServiceAddress SC_QuarcTransceiver <PACKET_T>::getSrc(Word_List p) {
	Word qph=p[0];
	return (qph>>16) &0xffff;
}

template <typename PACKET_T>
void SC_QuarcTransceiver <PACKET_T>:: transmit_packets()
    {
        // transmit forever
        while(true)
        {
            // blocking read from local tx_fifo. Non-empty tx_fifo indicates a packet ready
            // to be sent to network
            PACKET_T packet = tx_fifo.shift();
            /*
             * Based on the packet src and dst address we decide where to send the packet
             */

//            SBA::Service        dst_service_id  = getTo(getHeader(packet));
//            SBA::ServiceAddress dst_addr        = cfg.services(dst_service_id).address;
            SBA::ServiceAddress node_addr        = cfg.services(service).address;

            SBA::ServiceAddress dst_addr=getDst(packet);
            SBA::ServiceAddress src_addr=getSrc(packet);
            int quadrant=-1;
            if (node_addr==dst_addr) { // sending to oneself, this should not happen
            	rx_fifo.push(packet); //
            } else {
            	// Quadrant calculation
            	quadrant=calc_quad( dst_addr,  src_addr,  NSERVICES + 1);
            }

#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: transmitting packet to "<<dst_addr<<" on port "<< quadrant<< endl;
#endif
            //			cout << "SC_QuarcTransceiver: transmitting packet to NoC\n";
            // write this packet read from tx_fifo to network
            // the write is through port
            // the port will be connected to the appropriate export
            // that the network provides for storing this source tile's incomig data
            switch (quadrant) {
				case _QUARC_NEXT: network_rx_fifo0.push(packet); break;
				case _QUARC_RCROSS: network_rx_fifo1.push(packet); break;
				case _QUARC_LCROSS: network_rx_fifo2.push(packet); break;
				case _QUARC_PREV: network_rx_fifo3.push(packet); break;
				default: cerr << "Quadrant must be [0..3]!\n";
            }
        }
    }// funct: SC_QuarcTransceiver <PACKET_T>:: transmit_packets()

template <typename PACKET_T>
void SC_QuarcTransceiver <PACKET_T>:: receive_packets0()
    {
        // receive forever
        while(true)
        {
            // blocking read on rx_fifo0
            PACKET_T packet = rx_fifo0.shift();
            /*
             * Based on the packet dst address we decide to deliver or forward the packet
             *
             */
//            SBA::Service        dst_service_id  = getTo(getHeader(packet));
//            bool fwd=(dst_service_id!=service);
            SBA::ServiceAddress node_addr        = cfg.services(service).address;
            SBA::ServiceAddress dst_addr=getDst(packet);
            SBA::ServiceAddress src_addr=getSrc(packet);
            bool fwd=(node_addr!=dst_addr);
#ifdef SC_VERBOSE
//            SBA::ServiceAddress dst_addr        = cfg.services(dst_service_id).address;
//            SBA::ServiceAddress node_addr        = cfg.services(service).address;
//            SBA::Service        src_service_id  = getReturn_to(getHeader(packet));
//            SBA::ServiceAddress src_addr        = cfg.services(src_service_id).address;

            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: received packet from "<< src_addr<<" on port 0"<< endl;
#endif
            if (fwd) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: forwarded packet to "<<dst_addr<<" on port 0"<< endl;
#endif

            	network_rx_fifo0.push(packet); // this assumes that there is only one
            } else {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: delivered packet to RX fifo"<< endl;
#endif

				rx_fifo.push(packet);
            }
        }
    }// funct: SC_QuarcTransceiver <PACKET_T>:: receive_packets0()


template <typename PACKET_T>
void SC_QuarcTransceiver <PACKET_T>:: receive_packets1()
    {
        // receive forever
        while(true)
        {
            // blocking read on rx_fifo0
            PACKET_T packet = rx_fifo1.shift();
            /*
             * Based on the packet dst address we decide to deliver or forward the packet
             *
             */
//            SBA::Service        dst_service_id  = getTo(getHeader(packet));
//            bool fwd=(dst_service_id!=service);
            SBA::ServiceAddress node_addr        = cfg.services(service).address;
            SBA::ServiceAddress dst_addr=getDst(packet);
            SBA::ServiceAddress src_addr=getSrc(packet);
            bool fwd=(node_addr!=dst_addr);

#ifdef SC_VERBOSE
//            SBA::ServiceAddress dst_addr        = cfg.services(dst_service_id).address;
//            SBA::ServiceAddress node_addr        = cfg.services(service).address;
//            SBA::Service        src_service_id  = getReturn_to(getHeader(packet));
//            SBA::ServiceAddress src_addr        = cfg.services(src_service_id).address;
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: received packet from "<< src_addr<<" on port 1"<< endl;
#endif
            if (fwd) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: forwarded packet to "<<dst_addr<<" on port 3"<< endl;
#endif

            	network_rx_fifo3.push(packet); // this assumes that there is only one
            } else {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: delivered packet to RX fifo"<< endl;
#endif

				rx_fifo.push(packet);
            }
        }
    }// funct: SC_QuarcTransceiver <PACKET_T>:: receive_packets1()


template <typename PACKET_T>
void SC_QuarcTransceiver <PACKET_T>:: receive_packets2()
    {
        // receive forever
        while(true)
        {
            // blocking read on rx_fifo0
            PACKET_T packet = rx_fifo2.shift();
            /*
             * Based on the packet dst address we decide to deliver or forward the packet
             *
             */
//            SBA::Service        dst_service_id  = getTo(getHeader(packet));
//            bool fwd=(dst_service_id!=service);
            SBA::ServiceAddress node_addr        = cfg.services(service).address;
            SBA::ServiceAddress dst_addr=getDst(packet);
            SBA::ServiceAddress src_addr=getSrc(packet);
            bool fwd=(node_addr!=dst_addr);


#ifdef SC_VERBOSE
//            SBA::ServiceAddress dst_addr        = cfg.services(dst_service_id).address;
//            SBA::ServiceAddress node_addr        = cfg.services(service).address;
//            SBA::Service        src_service_id  = getReturn_to(getHeader(packet));
//            SBA::ServiceAddress src_addr        = cfg.services(src_service_id).address;

            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: received packet from "<< src_addr<<" on port 2"<< endl;
#endif
            if (fwd) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: forwarded packet to "<<dst_addr<<" on port 0"<< endl;
#endif

            	network_rx_fifo0.push(packet); // this assumes that there is only one
            } else {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: delivered packet to RX fifo"<< endl;
#endif

				rx_fifo.push(packet);
            }
        }
    }// funct: SC_QuarcTransceiver <PACKET_T>:: receive_packets2()



template <typename PACKET_T>
void SC_QuarcTransceiver <PACKET_T>:: receive_packets3()
    {
        // receive forever
        while(true)
        {
            // blocking read on rx_fifo0
            PACKET_T packet = rx_fifo3.shift();
            /*
             * Based on the packet dst address we decide to deliver or forward the packet
             *
             */
//            SBA::Service        dst_service_id  = getTo(getHeader(packet));
//            bool fwd=(dst_service_id!=service);
            SBA::ServiceAddress node_addr        = cfg.services(service).address;
            SBA::ServiceAddress dst_addr=getDst(packet);
            SBA::ServiceAddress src_addr=getSrc(packet);
            bool fwd=(node_addr!=dst_addr);


#ifdef SC_VERBOSE
//            SBA::ServiceAddress dst_addr        = cfg.services(dst_service_id).address;
//            SBA::ServiceAddress node_addr        = cfg.services(service).address;
//            SBA::Service        src_service_id  = getReturn_to(getHeader(packet));
//            SBA::ServiceAddress src_addr        = cfg.services(src_service_id).address;

            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: received packet from "<< src_addr<<" on port 3"<< endl;
#endif
            if (fwd) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: forwarded packet to "<<dst_addr<<" on port 3"<< endl;
#endif
            	network_rx_fifo3.push(packet); // this assumes that there is only one
            } else {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<"@"<<node_addr<<" SC_QuarcTransceiver: delivered packet to RX fifo"<< endl;
#endif
				rx_fifo.push(packet);
            }
        }
    }// funct: SC_QuarcTransceiver <PACKET_T>:: receive_packets3()

template <typename PACKET_T>
int SC_QuarcTransceiver <PACKET_T>::calc_quad(int destAddress, int nodeIndex, int netSize) {
    int rdest = (destAddress> nodeIndex)?destAddress- nodeIndex : destAddress - nodeIndex + netSize;
    if (rdest <= netSize/4 ) {
        return _QUARC_NEXT;
	} else if (rdest > netSize/4 && rdest <= netSize/2) {
	return _QUARC_RCROSS;
	} else if (rdest > netSize/2 && rdest < 3*netSize/4) {
	return _QUARC_LCROSS;
	} else if (rdest >= 3*netSize/4 && rdest < netSize) {
	return _QUARC_PREV;
	} else {
	return _QUARC_ERROR;
	}
}


} /* namespace: SC_SBA */




#endif /* SC_TRANSCEIVER_H_ */

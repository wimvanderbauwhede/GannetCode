/*
********************************************************************************
                 |
  File Name      | SC_Network.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 18-Nov-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Module for the Network on Chip in Gannet
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081118: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
*/

#ifndef SC_NETWORK_H_
#define SC_NETWORK_H_

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
//  CLASS: SC_Network: The SystemC Class for the Gannet Platform's NoC
//==============================================================================
/*
 * WV 15/03/2009
 * We assume the NoC is a Quarc with 16 nodes but we only use 12
 * We assume a delay of nhops*(4+nwords/flit) and a latency of (4+nwords/flit)
 * nhops is determined as explained in the Quarc paper
 *
 */

#define QUARC_NCYCLES_LINK 5
#define QUARC_NWORDS_FLIT 1

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC Class for the Gannet Platform's NoC
    /*!
       Detailed description here...
    */
template <typename PACKET_T>
class SC_Network : public sc_module
{
public:
    // TODO: is there a way to declare array of ports and exports?
    // array of ports is possible, but what about exports?
    // if no exports


    // ---------------------------- PORTS --------------------------------------

    port_SC_Config_if           cfg; //!< port for access to SBA::Config object (wrapped inside an sc_module)

    // array of specialized ports for writing to tiles (The rx_fifo of their transceiver)
    // NSERVICES + 1 because one port for gatewaytile (It will always be connected to the last port)
    port_SC_Fifo_if<PACKET_T, NSERVICES+1> pw_tile; //!<

    // ---------------------------- Exorts ------------------------------------
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_0 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_1 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_2 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_3 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_4 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_5 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_6 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_7 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_8 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_9 ;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_10;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_11;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_12;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_13;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_14;   // export provided for writing to rx_fifo
//    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_15;   // export provided for writing to rx_fifo
//    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_16;   // export provided for writing to rx_fifo
    sc_export<SC_Fifo_if<PACKET_T> >    xpwr_rx_fifo_17;   // export provided for writing to rx_fifo (of Gateway)

    // ---------------------------- Sub-Modules --------------------------------
    // The array of SC Fifos has to be declared as a pointer to an array here
    // because each instantiation of the array element in an SC_module array has to be given a unique name
    // That can't be done inside the definition. It's done in the constructor, using NEW.
    // NSERVICES + 1 so that there is a set of FIFOS for GatewayTile as well.
    SC_Fifo <PACKET_T, PACKET_FIFO_SZ> *tx_fifo[NSERVICES + 1];//!< Array of tx_fifos, one for each tile
    SC_Fifo <PACKET_T, PACKET_FIFO_SZ> *rx_fifo[NSERVICES + 1];//!< Array of rx_fifos, one for each tile

    // ---------------------------- METHODS ------------------------------------
    // create one thread for each tile
    void receive(int);
    void transmit(int);
#ifdef QUARC_DELAY
    unsigned int calc_nhops(unsigned int src, unsigned int dest);
#endif //  QUARC_DELAY
    // the SC_thread that spawns instances of the two functions for each tile
    void spawns();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Network"; }


    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Network);
    SC_Network(sc_module_name nm) :
        sc_module(nm) {
        // instantiate array of TX FIFOs, one for each tile
        for (unsigned int i=0; i < NSERVICES + 1; i++)
            tx_fifo[i] = new SC_Fifo <PACKET_T, PACKET_FIFO_SZ> (SC_concatenate("tx_fifo",i));  //!< ..  .
        // instantiate array of RX FIFOs, one for each tile
        for (unsigned int j=0; j < NSERVICES+ 1; j++)
            rx_fifo[j] = new SC_Fifo <PACKET_T, PACKET_FIFO_SZ> (SC_concatenate("rx_fifo",j));  //!< ..  .
        //FIXME: This is hardcoded for a 16-node Quarc.
        // export access to internal rx_fifos
        xpwr_rx_fifo_0 .   bind( (*rx_fifo[0 ]) );
        xpwr_rx_fifo_1 .   bind( (*rx_fifo[1 ]) );
        xpwr_rx_fifo_2 .   bind( (*rx_fifo[2 ]) );
        xpwr_rx_fifo_3 .   bind( (*rx_fifo[3 ]) );
        xpwr_rx_fifo_4 .   bind( (*rx_fifo[4 ]) );
        xpwr_rx_fifo_5 .   bind( (*rx_fifo[5 ]) );
        xpwr_rx_fifo_6 .   bind( (*rx_fifo[6 ]) );
        xpwr_rx_fifo_7 .   bind( (*rx_fifo[7 ]) );
        xpwr_rx_fifo_8 .   bind( (*rx_fifo[8 ]) );
        xpwr_rx_fifo_9 .   bind( (*rx_fifo[9 ]) );
        xpwr_rx_fifo_10.   bind( (*rx_fifo[10]) );
        xpwr_rx_fifo_11.   bind( (*rx_fifo[11]) );
        xpwr_rx_fifo_12.   bind( (*rx_fifo[12]) );
        xpwr_rx_fifo_13.   bind( (*rx_fifo[13]) );
        xpwr_rx_fifo_14.   bind( (*rx_fifo[14]) );
//        xpwr_rx_fifo_15.   bind( (*rx_fifo[15]) ); // 16-node Quarc
//        xpwr_rx_fifo_16.   bind( (*rx_fifo[16]) );
        xpwr_rx_fifo_17.   bind( (*rx_fifo[15]) ); // For gateway

        SC_THREAD(spawns);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
    }
    // ---------------------------- Primitive Members --------------------------

};/* class: SC_Network */

//==============================================================================
//  spawns()
//==============================================================================

// spawn the receive() and transmit() threads here for each tile
// Spawn NSERVICES + 1; a pair of thread for each tile instance, and the gatewaytile.
template <typename PACKET_T>
void SC_Network<PACKET_T> :: spawns()
{
    // sc_spawn arguments:
        // arg1 = pointer to function (passes to sc_bind)
        // arg2 = this; indicates function is not global but a member function (passed to sc_bind)
        // arg3 = tile_id argument to functions. Passed to sc_bind
        // arg4 = name of function (using SC_concatenate to generate unique name for each thread
    for (unsigned int i=0; i < NSERVICES+1 ; i++) {
        sc_spawn (  sc_bind (   &SC_Network<PACKET_T>::receive  ,
                                this,
                                i),
                    SC_concatenate("receive",i)
                  );
        sc_spawn (  sc_bind (   &SC_Network<PACKET_T>::transmit  ,
                                this,
                                i),
                    SC_concatenate("transmit",i)
                  );
    }//for
}



//==============================================================================
//  receive()
//==============================================================================
template <typename PACKET_T>
void SC_Network<PACKET_T> :: receive(int tile_id)
{
    // do a blocking read of local rx_fifo reserved for tile instance_0
    // will block here until fifo is not-empty
    // NOTE: we are dereferencing the pointer here, and using dot. Could use -> too directly.
	while(true) {
    PACKET_T packet = (*rx_fifo[tile_id]).shift();
#ifdef SC_DEBUG
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << name() << " NoC receive() " << tile_id << " has read its rx_fifo" << endl;
#endif //SC_DEBUG

    // read destination from the packet
    // required read of the "cfg" object, which is done through the port
    // (Note that [] operator is overloaded for the port used to access configure object
    // no time consumed here... explicitly model through wait() ??
    SBA::Service        service_id  = getTo(getHeader(packet));
    SBA::ServiceAddress dest        = cfg.services(service_id).address;

#ifdef QUARC_DELAY
    unsigned int nhops = calc_nhops(tile_id,dest);
    // FIXME! delay requires a proper "delay line"

    send_to_network() {
    while (true) {
    // block on rx_fifo;
    // get packet;
    // post in network_delay_fifo
	sc_event packet_received; // should probably be class global!
	sc_time t_delay(delay*_CLK_P , _CLK_U);// should probably be class global!
	notify(t_delay,packet_received);
	}
	}
	receive_from_network() {
   while (true) {
    // block on packet_received event;
    wait(packet_received);
    // get packet from network_delay_fifo
    // post in tx_fifo
	}
	}

    unsigned int delay = nhops*(QUARC_NCYCLES_LINK-1+QUARC_NWORDS_FLIT);
#endif // QUARC_DELAY

    unsigned int latency = (QUARC_NCYCLES_LINK-1+QUARC_NWORDS_FLIT)*(getLength(getHeader(packet))+3)/QUARC_NWORDS_FLIT; // FIXME: correct for time taken to push packet onto fifo
    wait(latency*_CLK_P , _CLK_U);
    // now transmit to the destination... store in the local tx_fifo for that destination
    // another thread will read this fifo and store packets into the destination tile
    // we are 'pushing' packets into the tiles (and the tiles are pushing packets into the network)
    // so the network object needs to store the packets inside the tile.

    (*tx_fifo[dest]).push(packet);

#ifdef SC_DEBUG
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << name() << " NoC receive() " << tile_id
            << " has pushed packet to the local tx_fifo for dest: " << dest <<  endl;
#endif //SC_DEBUG
	}
}

//==============================================================================
//  transmit()
//==============================================================================
template <typename PACKET_T>
void SC_Network<PACKET_T> :: transmit(int tile_id)
{
    PACKET_T packet;
    // transmit forever
    while(true)
    {
        // do a blocking read of tx_fifo reserved for tile instance_0
        // will block here until fifo is no-empty
        // when a receive_and_transmit() thread has stored a packet in the tx_fifo for this thread
        // then it will transmit
        packet = (*tx_fifo[tile_id]).shift();
#ifdef SC_DEBUG
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
                << name() << " NoC transmit() has found a packet for instance_" << tile_id << endl;
#endif //SC_DEBUG

        // now transmit to the destination tile (which will go into that tile's
        // transceiver's rx_fifo....
        // we are 'pushing' packets into the tiles (and the tiles are pushing packets into the network)
        // so the network object needs to store the packets inside the tile.
        // This write will be through to port, which would be connected to the export of the approprate tile
        // TODO: create an array of ports so that can simply address the right tile though  tile_id
        //pw_tile_0.push(packet);
        // dereference or -> needed because array of ports
        (*(pw_tile[tile_id])).push(packet);
#ifdef SC_DEBUG
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
                << name() << " NoC transmit() has pushed packet onto instance_" << tile_id << endl;
#endif //SC_DEBUG
    }

} // transmit()

#ifdef QUARC_DELAY
template <typename PACKET_T>
unsigned int SC_Network<PACKET_T> :: calc_nhops(unsigned int src, unsigned int dest) {
	unsigned int nnodes=NSERVICES+1; // must be multiple of 4
	// 1. Normalise
	// 1.1
	unsigned int ndest = dest-src;
	if (ndest<0) {
		ndest=nnodes+ndest;
	}
	// now we have: 1,2,3,4;5,6,7,8;8,9,10,11;12,13,14,15
	unsigned int nhops=0;
	if (ndest<=nnodes/4) {
		nhops=ndest;
	} else if (ndest<=nnodes/2) {
		nhops=1+nnodes/2-ndest;
	} else if (ndest<=3*nnodes/4) {
		nhops=1+ndest-nnodes/2;
	} else {
		nhops= nnodes-ndest;
	}
	return nhops;
} // calc_nhops
#endif //QUARC_DELAY

} /* namespace: SC_SBA */



#endif /* SC_NETWORK_H_ */

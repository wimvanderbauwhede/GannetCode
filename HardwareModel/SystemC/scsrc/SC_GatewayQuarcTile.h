/*
********************************************************************************
                 |
  File Name      | SC_GatewayQuarcTile.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 26-Jan-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Gateway Tile Class
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_GATEWAYTILE_H_
#define SC_GATEWAYTILE_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: SERVICE TILE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC Gateway Tile Class
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_GatewayQuarcTile : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    // port for writing to rx_fifo in the network that corresponds to this tile
    port_SC_Fifo_if <Packet_t>	network_rx_fifo0;
    port_SC_Fifo_if <Packet_t>	network_rx_fifo1;
    port_SC_Fifo_if <Packet_t>	network_rx_fifo2;
    port_SC_Fifo_if <Packet_t>	network_rx_fifo3;
    port_SC_Config_if		     cfg;
    port_SC_reg_if<uint	>       io_mech;

    // ---------------------------- EXPORTS ------------------------------------
    // Export provided (to network) for writing to Rx FIFO of this tile's transceiver
    sc_export<SC_Fifo_if<Packet_t> > xpwr_rxfifo0;    //!<
    sc_export<SC_Fifo_if<Packet_t> > xpwr_rxfifo1;
    sc_export<SC_Fifo_if<Packet_t> > xpwr_rxfifo2;
    sc_export<SC_Fifo_if<Packet_t> > xpwr_rxfifo3;
	// ---------------------------- Sub-Modules --------------------------------
    SC_Register     <SBA::Service>              service;
    SC_Register     <SBA::ServiceAddress>       address;

    SC_Gateway      <ADDR_T, DATA_T>            gateway;
    SC_Fragmenter									fragmenter;
    SC_Defragmenter									defragmenter;

    SC_QuarcTransceiver   <Packet_t>            transceiver;
    SC_Memory_List  <ADDR_T, DATA_T>            result_store;    //!< Lists (SBA::Word_List) stored, so using the appropriate List memory class
    SBA::TaskDescList                           task_descriptions; //!< SystemC type? (Does it correspond to a hardware data store element?)


    //SC_Memory_List  <ADDR_T, DATA_T>            data_packet_store;    //!< Lists (SBA::Word_List) stored, so using the appropriate List memory class
    //SC_Register     <bool>                      finished; //!< SystemC type? (SC_Register?)

	// ---------------------------- METHODS ------------------------------------
	//void do_proc();
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_GatewayQuarcTile"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_GatewayQuarcTile);
    SC_GatewayQuarcTile( sc_module_name      nm_ ,
                    SBA::Service        s_  ,
                    SBA::ServiceAddress a_  ,
                    SBA::TaskDescList& tds_  ) :
//    SC_GatewayQuarcTile( sc_module_name nm ):
	   sc_module           (nm_)   ,
	   service             ("service", s_) ,
	   address             ("address", a_) ,
	   gateway             (   "gateway"   ,
	                           s_          ,
	                           a_          ,
	                           tds_        ),
	   transceiver          ("transceiver",s_),
       fragmenter   ("fragmenter",s_),
       defragmenter   ("defragmenter",s_),
	   result_store      ("result_store"),
       task_descriptions   (tds_)
	   //finished            ("finished")
       //data_packet_store ("data_packet_store"),
	{
        //SC_THREAD(do_proc);
        // creation debug message..
        const_debug_msg(name(),kind());

        // ----------------------------------
        // ************ Bindings ************
        // ----------------------------------

        // propagate upwards the write export to transceiver.rx_fifo (for access by network)
        // i.e. bind the Tile's export to the export of Tranceiver
        xpwr_rxfifo0.bind  (transceiver.xpwr_rxfifo0);
        xpwr_rxfifo1.bind  (transceiver.xpwr_rxfifo1);
        xpwr_rxfifo2.bind  (transceiver.xpwr_rxfifo2);
        xpwr_rxfifo3.bind  (transceiver.xpwr_rxfifo3);

        // GATEWAY
        // -------
//        gateway.transceiver_rx_fifo  .bind(transceiver.xpwr_rxfifo);
//        gateway.transceiver_tx_fifo  .bind(transceiver.xpwr_txfifo);

		// bind gateway port to rx_fifo read export of transceiver
        gateway.transceiver_rx_fifo.bind   (defragmenter.xpwr_rxfifo);
		defragmenter.transceiver_rx_fifo.bind   (transceiver.xpwr_rxfifo);
        // bind gateway port to tx_fifo write export of transceiver
		gateway.transceiver_tx_fifo.bind   (fragmenter.xpwr_txfifo);
        fragmenter.transceiver_tx_fifo.bind   (transceiver.xpwr_txfifo);

        gateway.result_store      .bind(result_store.xpwr_1);
        gateway.io_mech				.bind(io_mech);//propagate upwards the port access to io_mech
        //gateway.result_store_2    .bind(result_store.xpwr_2);
        //gateway.data_packet_store .bind(data_packet_store.xpwr_1);
        //gateway.finished            .bind(finished.xpwr);

        // TRANCEIVER
        // ----------
        transceiver.cfg	.bind(cfg);
        fragmenter.cfg	.bind(cfg);
        defragmenter.cfg	.bind(cfg);

        transceiver  .network_rx_fifo0.bind(network_rx_fifo0);
        transceiver  .network_rx_fifo1.bind(network_rx_fifo1);
        transceiver  .network_rx_fifo2.bind(network_rx_fifo2);
        transceiver  .network_rx_fifo3.bind(network_rx_fifo3);
	}
	// ---------------------------- Primitive Members --------------------------
};/* class: SC_GatewayQuarcTile */

} /* namespace: SC_SBA */

#endif /* SC_GATEWAYTILE_H_ */

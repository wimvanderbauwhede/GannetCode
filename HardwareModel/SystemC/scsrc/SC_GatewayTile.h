/*
********************************************************************************
                 |
  File Name      | SC_GatewayTile.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 26-Jan-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Gateway Tile Class
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

#ifndef SC_GATEWAYTILE_H_
#define SC_GATEWAYTILE_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

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
class SC_GatewayTile : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    // port for writing to rx_fifo in the network that corresponds to this tile
    port_SC_Fifo_if <Packet_t>	network_rx_fifo;
    port_SC_reg_if<uint	>       io_mech;

    // ---------------------------- EXPORTS ------------------------------------
    // Export provided (to network) for writing to Rx FIFO of this tile's tranceiver
    sc_export<SC_Fifo_if<Packet_t> > xpwr_rxfifo;    //!<

	// ---------------------------- Sub-Modules --------------------------------
    SC_Register     <SBA::Service>              service;
    SC_Register     <SBA::ServiceAddress>       address;

    SC_Gateway      <ADDR_T, DATA_T>            gateway;
    SC_Tranceiver   <Packet_t>                  tranceiver;
    SC_Memory_List  <ADDR_T, DATA_T>            result_store;    //!< Lists (SBA::Word_List) stored, so using the appropriate List memory class
    SBA::TaskDescList                           task_descriptions; //!< SystemC type? (Does it correspond to a hardware data store element?)


    //SC_Memory_List  <ADDR_T, DATA_T>            data_packet_store;    //!< Lists (SBA::Word_List) stored, so using the appropriate List memory class
    //SC_Register     <bool>                      finished; //!< SystemC type? (SC_Register?)

	// ---------------------------- METHODS ------------------------------------
	//void do_proc();
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_GatewayTile"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_GatewayTile);
    SC_GatewayTile( sc_module_name      nm_ ,
                    SBA::Service        s_  ,
                    SBA::ServiceAddress a_  ,
                    SBA::TaskDescList& tds_  ) :
//    SC_GatewayTile( sc_module_name nm ):
	   sc_module           (nm_)   ,
	   service             ("service", s_) ,
	   address             ("address", a_) ,
	   gateway             (   "gateway"   ,
	                           s_          ,
	                           a_          ,
	                           tds_        ),
	   tranceiver          ("tranceiver",s_),
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

        // propagate upwards the write export to tranceiver.rx_fifo (for access by network)
        // i.e. bind the Tile's export to the export of Tranceiver
        xpwr_rxfifo.bind  (tranceiver.xpwr_rxfifo);

        // GATEWAY
        // -------
        gateway.tranceiver_rx_fifo  .bind(tranceiver.xpwr_rxfifo);
        gateway.tranceiver_tx_fifo  .bind(tranceiver.xpwr_txfifo);
        gateway.result_store      .bind(result_store.xpwr_1);
        gateway.io_mech				.bind(io_mech);//propagate upwards the port access to io_mech
        //gateway.result_store_2    .bind(result_store.xpwr_2);
        //gateway.data_packet_store .bind(data_packet_store.xpwr_1);
        //gateway.finished            .bind(finished.xpwr);

        // TRANCEIVER
        // ----------
        tranceiver  .network_rx_fifo.bind(network_rx_fifo);
	}
	// ---------------------------- Primitive Members --------------------------
};/* class: SC_GatewayTile */

} /* namespace: SC_SBA */

#endif /* SC_GATEWAYTILE_H_ */

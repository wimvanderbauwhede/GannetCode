/*
********************************************************************************
                 |
  File Name      | SC_Tile.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 29-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Service Tile Class
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

#ifndef SC_TILE_H_
#define SC_TILE_H_

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
//	CLASS: SERVICE TILE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This the tile
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_Tile : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if <Packet_t>   network_rx_fifo;   //!< for writing to to network.rx_fifo
    port_SC_Config_if		     cfg_services; 		//!< port for access to SBA::Config object (wrapped inside an sc_module)
													//!< required by service_core

    // ---------------------------- EXPORTS ------------------------------------
    sc_export<SC_Fifo_if<Packet_t> > xpwr_rxfifo;    //!< Export provided (to network) for writing to Rx FIFO of this tile's tranceiver

    // ---------------------------- Sub-Modules --------------------------------
    SC_Register         <SBA::Service >             service;                //!<
    SC_Register         <SBA::ServiceAddress>       address;                //!<

    SC_ServiceManager   <ADDR_T, DATA_T>            service_manager;//!< ...
    SC_ServiceCore     	                            service_core;   //!< Interface to the core

    SC_Memory_List      <ADDR_T, DATA_T>            data_store;   //!< Since Lists stored, so using the appropriate List memory class
    SC_Stack            <DATA_SZ>                   data_address_stack;
    SC_Memory_List      <ADDR_T, DATA_T>            code_store;   //!< Since Lists stored, so using the appropriate List memory class
    SC_Memory           <ADDR_T, RegisterEntry>     register_set;   //!< registers[NREG] modelled as a memory
    SC_RequestTable <NREGS>							request_table;
    SC_LookupTable 									lookup_table;
    SC_Tranceiver       <Packet_t>                  tranceiver;     //!< ...
    SC_Subtask_List                                 subtask_list;   //!< The subtask list (should be  inside service manager?)


    // look up table? - SystemC type?
//     SBA::LookupTable                                lookup_table;

	// ---------------------------- METHODS ------------------------------------
	void do_proc();
	 // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Tile"; }

	// ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_Tile);
    SC_Tile(    sc_module_name      nm_ ,
                SBA::Service        s_  ,
                SBA::ServiceAddress a_  ):
        sc_module(nm_) ,
        service ("service", s_),
        address ("address", a_),
        service_manager (   "service_manager",
                            s_,
                            a_) ,
        service_core ("service_core",s_),
        data_store ("data_store"),
        data_address_stack ("data_address_stack", DATA_OF),
        code_store ("code_store"),
        tranceiver   ("tranceiver",s_),
        register_set ("register_set"),
        request_table("request_table"),
        lookup_table("lookup_table"),
        subtask_list ("subtask_list")
	{
        // ************ Instantiaing ************


	    // ----------------------------------
		// ************ Bindings ************
        // ----------------------------------

        // ----------- SERVICE CORE ---------

        service_core.subtask_list   .bind (subtask_list.xpwr_2);                // one port of subtask_list for core
        service_core.data_store   .bind ( data_store.xpwr_2 );              // one port of data_store for core
        service_core.core_status    .bind (service_manager.xpwr_corestatus );   // to arbiter inside the service_manager controlling core_status inside the manager
        service_core.results_store  .bind (service_manager.xp_results_store);   // give service_core access to the results_store Fifo inside the manager
        service_core.ack_ok         .bind (service_manager.xp_ack_ok);          // give service_core access to the ack_ok boolean inside the manager
        service_core.core_return_type.bind(service_manager.xp_core_return_type);// give service_core access to core_return_type signal inside the manager
        service_core.current_subtask.bind(service_manager.xp_current_subtask);  // for access to current_subtask inside the manager
        service_core.opcode         .bind(service_manager.xp_opcode         );  // for access to opcode inside the manager
        service_core.n_args         .bind(service_manager.xp_n_args         );  // for access to n_args inside the manager
        service_core.arg_addresses  .bind(service_manager.xp_arg_addresses);    // for access to arg_addresses inside the manager
        service_core.service_id 	.bind(service_manager.xp_service_id);    	// for access to service_id inside the manager
        service_core.cfg_services	.bind(cfg_services);	//propagate up the port for accessing config object
        service_core.symbol_table   .bind(service_manager.xp_symbol_table);
        service_core.subtask_reference_fifo   .bind(service_manager.xp_subtask_reference_fifo);
//        service_core.transceiver_tx_fifo.bind   (tranceiver.xpwr_txfifo); // FIXME: NEED ARBITER!
        service_core.transceiver_tx_fifo.bind(service_manager.xp_transceiver_tx_fifo);
        service_core.code_store.bind(service_manager.xp_code_store);
		service_core.lookup_table.bind(lookup_table.xpwr_2);
		service_core.data_address_stack.bind(data_address_stack.xpwr_2);
        // ----------- SERVICE MANAGER ------

		// bind service_mngr port to rx_fifo read export of tranceiver
		service_manager.tranceiver_rx_fifo.bind   (tranceiver.xpwr_rxfifo);

        // bind service_mngr port to tx_fifo write export of tranceiver
        service_manager.tranceiver_tx_fifo.bind   (tranceiver.xpwr_txfifo);

		// Bind servicemanager's port for access to data memory, to the one of the exports provided by dual-ported data memory
        service_manager.data_store.bind  ( data_store.xpwr_1 );
        service_manager.data_address_stack.bind(data_address_stack.xpwr_1);
        //service_manager.data_store_1.bind  ( data_store.xpwr_1 );
        //service_manager.data_store_2.bind  ( data_store.xpwr_2 );

        // Bind servicemanager's two ports for access to code memory, to the two export provided by dual-ported code memory
        service_manager.code_store_1.bind  ( code_store.xpwr_1 );
        service_manager.code_store_2.bind  ( code_store.xpwr_2 );

        // Bind servicemanager's port for access to register memory, to the export provided by register_memory
        service_manager.register_set.bind  ( register_set.xpwr_1 );

        // Bind manager's port for access to subtask_list, to the export provided by subtask_list
        service_manager.subtask_list.bind  (subtask_list.xpwr_1);

        service_manager.request_table_1.bind (request_table.xpwr_1);
        service_manager.request_table_2.bind (request_table.xpwr_2);
        service_manager.lookup_table.bind(lookup_table.xpwr_1);

        // ----------- TRANCEIVER -----------

        tranceiver.network_rx_fifo  .bind(network_rx_fifo); // tranceiver's port for writing to network is propagated upwards

		// propagate upwards the write export to rx_fifo (for access by network)
		// i.e. bind the Tile's export to the export of Tranceiver
		xpwr_rxfifo.bind  (tranceiver.xpwr_rxfifo);

		SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());

	}
	// ---------------------------- Primitive Members --------------------------



};/* class: SC_Tile */

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_Tile<ADDR_T, DATA_T> :: do_proc()
    {
    // **** 4TEST *****
    SBA::Data d;
    Word w = 10;
    d.push(w);
    SBA::MemAddress a = 1;
    code_store.write(a, d);
    //code_store1.write(a,d);
    //cout << sc_time_stamp() << " Tile has read " << d.shift() << " from code store address " << a << endl;
    // *** 4TEST END ****
    }// funct: SC_Tile :: do_proc()

} /* namespace: SC_SBA */

#endif /* SC_TILE_H_ */

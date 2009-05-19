/*
********************************************************************************
                 |
  File Name      | SC_System.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 29-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Module for the complete Gannet System
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081029: Created.
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

#ifndef SC_SYSTEM_H_
#define SC_SYSTEM_H_


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
//  CLASS: THE SBA GANNET SYSTEM
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC Module for the complete Gannet System
    /*!
       Detailed description here...
    */
class SC_System : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------


    // ---------------------------- Non-SC types ------------------------------
    SBA::ServiceAddress gw_address;
    SBA::Services       services;
    uint                nservices;
    SBA::TaskDescList   task_descriptions; // This should be SC_type too? SC_Deque?
    SBA::Word_List      results;


    // ---------------------------- SC-Sub-Modules -----------------------------
    SC_Network <Packet_t>   network;//!< The NoC (Use Mahmoud's?)
    SC_Config               scfg;    //!< The SystemC wrapper class containing the SBA::Config object

    //!< Array of tiles
    SC_Tile         <MemAddress, Data> *instances[NSERVICES];
    //!< One instance of the GatewayTile for the system
    SC_GatewayTile  <MemAddress, Data> gwinstance;

	SC_Register <uint>	io_mech; // this needs to be SC type for access by Tiles
	//SC_Register <bool>	interface_obj; // will this be an SC_module with ports?


	unsigned int one;
    //SBA::Bridge       *bridge;        //!< The bridge from C++ model for now
    // ---------------------------- METHODS ------------------------------------
    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_System"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_System);
    SC_System(  sc_module_name nm   ,
                TaskDescList& tds_  ) :
        sc_module(nm)  ,
        network     ("network"),
        scfg         ("cfg")     ,
        task_descriptions (tds_),
        gwinstance  (   "gwinstance"    ,
                        0               , // service
                        NSERVICES       , // address (last address for Gateway)
                        tds_            ),
                        one(1),
        io_mech		("io_mech",one)

    {
        // ************ Instantiaing ************

        // Initialize the Services object by reading the Config object
        // two 'dots' required because the SC_Config 'cfg' object is a wrapper
        // around an internal SBA::Config 'cfg' object.
        services = scfg.cfg.services;

        // Array of sc_modules have to be dynamically instantiated
        foreach(Services, services)
        {
            Service     service_id  = iter->first;
            ServicePair servicep    = iter->second;
            ServiceAddress service_address = servicep.address;

            //cout << "ServiceAddress is " << service_address << endl;
            if  (service_address != NSERVICES)
            {
              instances[service_address] = new SC_Tile <SBA::MemAddress, SBA::Data>
                                                (   SC_concatenate("instance",service_address)    ,
                                                    service_id  ,
                                                    service_address);  //!< ..  .
            }//if
        }//foreach

        // Deprecated instantiation loop. Now using foreach to iterate through the Services map.
        //for (int i=0; i< (int)NSERVICES; i++)
        //    instances[i] = new SC_Tile <SBA::Packet_t, SBA::MemAddress, SBA::Data>  (SC_concatenate("instance",i));  //!< ..  .


        // give interface object access to io_mech
        // interface.io_mech	.bind(io_mech);

        // give the network access to the SC_Config object
        network.cfg_services .bind (scfg.xp_1);

        // Network's "pw_tile" is a port array. So have to make connection for each tile
        // Each connection is assigned a position in the array on
        // a first-connected first-position basis
        // TODO: can make this a loop. Do it (once sc_export issue is  resolved?)

        network.pw_tile .bind ( (*instances[0 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[1 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[2 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[3 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[4 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[5 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[6 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[7 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[8 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[9 ]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[10]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[11]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[12]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[13]) .xpwr_rxfifo );
        network.pw_tile .bind ( (*instances[14]) .xpwr_rxfifo );
//        network.pw_tile .bind ( (*instances[15]) .xpwr_rxfifo );
//        network.pw_tile .bind ( (*instances[16]) .xpwr_rxfifo );
        network.pw_tile .bind ( gwinstance       .xpwr_rxfifo ); // network's last port connection to gateway's export

        // connect tile's port for writing to networks rx_fifo, to the export provided by network
        // TODO: convert this to loop, but how? not possible to have an array of exports
        (*instances[0 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_0  );
        (*instances[1 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_1  );
        (*instances[2 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_2  );
        (*instances[3 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_3  );
        (*instances[4 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_4  );
        (*instances[5 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_5  );
        (*instances[6 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_6  );
        (*instances[7 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_7  );
        (*instances[8 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_8  );
        (*instances[9 ]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_9  );
        (*instances[10]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_10 );
        (*instances[11]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_11 );
        (*instances[12]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_12 );
        (*instances[13]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_13 );
        (*instances[14]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_14 );
//        (*instances[15]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_15 );
//        (*instances[16]) .network_rx_fifo .bind ( network.xpwr_rx_fifo_16 );

        gwinstance       .network_rx_fifo .bind ( network.xpwr_rx_fifo_17 ); // network's last export given to gateway
        gwinstance       .io_mech	.bind		(io_mech); // gateway tile has access to io_mech

        // connect tile's port for accessing config object, to the export provided by SC_Confif
        // TODO: convert this to loop, but how? not possible to have an array of exports
        (*instances[0 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[1 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[2 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[3 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[4 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[5 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[6 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[7 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[8 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[9 ]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[10]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[11]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[12]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[13]) .cfg_services .bind ( scfg.xp_2 );
        (*instances[14]) .cfg_services .bind ( scfg.xp_2 );
//        (*instances[15]) .cfg_services .bind ( scfg.xp_2 );
//        (*instances[16]) .cfg_services .bind ( scfg.xp_2 );


        SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
    }//Constructor
};/* class: SC_System */

//==============================================================================
//  DO_PROC()
//==============================================================================
void SC_System :: do_proc()
{
    //debug message
    //run_debug_msg(name());

}// funct: SC_System :: do_proc()

} /* namespace: SC_SBA */

#endif /* SC_SYSTEM_H_ */

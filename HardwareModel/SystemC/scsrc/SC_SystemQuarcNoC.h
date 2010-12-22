/*
********************************************************************************
                 |
  File Name      | SC_SystemQuarcNoC.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
  	  	  	  	 | with each tile integrating a Quarc NoC TRX
-----------------|--------------------------------------------------------------
  Created        | 29-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Module for the complete Gannet System
-----------------|--------------------------------------------------------------
  Modifications  | WV_20101024: Copy from SC_System.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_SYSTEM_H_
#define SC_SYSTEM_H_

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
//  CLASS: The Gannet SoC Platform  SBA System
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC Module for the complete Gannet System
    /*!
       Detailed description here...
    */
class SC_SystemQuarcNoC : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------


    // ---------------------------- Non-SC types ------------------------------
    SBA::ServiceAddress gw_address;
    SBA::Services       services;
    uint                nservices;
    string 				task_data;
    SBA::TaskDescList   task_descriptions; // This should be SC_type too? SC_Deque?
    SBA::Word_List      results;


    // ---------------------------- SC-Sub-Modules -----------------------------
//    SC_Network <Packet_t>   network;//!< The NoC (Use Mahmoud's?)
    SC_Config               scfg;    //!< The SystemC wrapper class containing the SBA::Config object

    //!< Array of tiles
    SC_QuarcTile         <MemAddress, Data> *instances[NSERVICES];
    //!< One instance of the GatewayTile for the system
    SC_GatewayQuarcTile  <MemAddress, Data> gwinstance;
    unsigned int one;
	SC_Register <uint>	io_mech; // this needs to be SC type for access by Tiles
	//SC_Register <bool>	interface_obj; // will this be an SC_module with ports?
    //SBA::Bridge       *bridge;        //!< The bridge from C++ model for now
    // ---------------------------- METHODS ------------------------------------
    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_SystemQuarcNoC"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_SystemQuarcNoC);
    SC_SystemQuarcNoC(  sc_module_name nm   ,
                TaskDescList& tds_  ) :
        sc_module(nm)  ,
        task_descriptions (tds_),
//        network     ("network"),
        scfg         ("cfg")     ,
        gwinstance  (   "gwinstance"    ,
                        0               , // service
                        NSERVICES       , // address (last address for Gateway)
                        tds_            ),
                        one(1),
        io_mech		("io_mech",one)

    {
    	// WV02122009: bit of a hack to get the data file
    	StringPair tdc_file=task_descriptions.front();
    	task_data=tdc_file.datafile;
    	// ************ Instantiaing ************

        // Initialize the Services object by reading the Config object
        // two 'dots' required because the SC_Config 'cfg' object is a wrapper
        // around an internal SBA::Config 'cfg' object.
        services = scfg.cfg.services;

        // Array of sc_modules have to be dynamically instantiated
        for(Services::iterator iter=services.begin();iter!=services.end();iter++) {
            Service     service_id  = iter->first;
            ServicePair servicep    = iter->second;
            ServiceAddress service_address = servicep.address;

            //cout << "ServiceAddress is " << service_address << endl;
            if  (service_address != NSERVICES)
            {
              instances[service_address] = new SC_QuarcTile <SBA::MemAddress, SBA::Data>
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
/*
        // give the network access to the SC_Config object
//        network.cfg.bind (scfg.xp_1);

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
*/
        gwinstance       .io_mech	.bind		(io_mech); // gateway tile has access to io_mech
        gwinstance.cfg.bind(scfg.xp_2);
        // connect tile's port for accessing config object, to the export provided by SC_Config
        // TODO: convert this to loop, but how? not possible to have an array of exports
        (*instances[0 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[1 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[2 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[3 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[4 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[5 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[6 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[7 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[8 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[9 ]) .cfg .bind ( scfg.xp_2 );
        (*instances[10]) .cfg .bind ( scfg.xp_2 );
        (*instances[11]) .cfg .bind ( scfg.xp_2 );
        (*instances[12]) .cfg .bind ( scfg.xp_2 );
        (*instances[13]) .cfg .bind ( scfg.xp_2 );
        (*instances[14]) .cfg .bind ( scfg.xp_2 );
//        (*instances[15]) .cfg .bind ( scfg.xp_2 );
//        (*instances[16]) .cfg .bind ( scfg.xp_2 );


		#include "../gensrc/QuarcNoC.cc"

        SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
    }//Constructor
};/* class: SC_System */

//==============================================================================
//  DO_PROC()
//==============================================================================
void SC_SystemQuarcNoC :: do_proc()
{
    //debug message
    //run_debug_msg(name());

}// funct: SC_System :: do_proc()

} /* namespace: SC_SBA */

#endif /* SC_SYSTEM_H_ */

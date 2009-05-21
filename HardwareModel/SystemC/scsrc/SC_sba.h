/*
***********************************************************************************************************
                 |
  File Name      | SC_sba.h
-----------------|-----------------------------------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|-----------------------------------------------------------------------------------------
  Created        | October 2008. Department of Computing Science, University of Glasgow
-----------------|-----------------------------------------------------------------------------------------
  Description    | The root header file for the SC_SBA Library
-----------------|-----------------------------------------------------------------------------------------
  Modifications  |
                 |
***********************************************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

//==============================================================================
// CONVENTIONS
//==============================================================================
/*
Prefixes:
=========

    p_      = port
    xp_     = export
    pw_     = port for writing
    pr_     = port for reading
    pwr_    = write/read port
    xpw_    = export to be used for writing
    xpr_    = export to be used for reading
    xpwr_   = write/read export
    SC_     = SystemC model relevant

    port_<interface_name> = specialized port for accessing methods of <interface_name>. Can access methods using dot(.) operator with spec. ports.




Suffixes:
=========

    _T      = Data type
    _if     = interface class
    _d      = delayed version of (redundant since by default everything is timed. _ut is more relevant)
    _ut     = un-time version of ...
    _gw     = A GatewayTile/Gateway specific sub_module (to differentiate from ServiveTile/ServiceManager specific logic)

*/

//==============================================================================
// Service-based SoC project: Root header file
//==============================================================================

#ifndef SC_SBA_H_
#define SC_SBA_H_

/* TIMING */
#define SC_TIMED    // define for timed model (WN_20081105: Not being used)


/* C++ Libraries abd headers*/
#include <iostream>
#include <iomanip>
#include <typeinfo>
//#include st


/*	SystemC header and TLM header file
	The SyctemC and TLM folder should be in PATH  */
#include "systemc.h"
#include "tlm.h"

//==============================================================================
/*	Direct Inlcudes from C++ Model of Garnet-HW
i.e. the /Garnet-HW/C++ folder
Relative path so change if directory structure violated	*/
//==============================================================================

/* The C++ ServiceCore Library*/
//#include "SBA/ServiceCoreLibrary.h"


/* Configuration file */
#include "SBA/ServiceConfiguration.h"

/* Core typedefs */
#include "SBA/Base/Types.h"

/* For Storage Class; Also contains FIFOS, Lists etc; but should use SC types */
#include "SBA/Types.h"

namespace SC_SBA {
const bool debug_all=true;
const SBA::Service debug_service=0;
}


/* Packet Types */
#include "SBA/Packet.h"
//#include "SBA/Packet.cc"

/* For Memory Class */
#include "SBA/Memory.h"

/* For LookupTable */
#include "SBA/LookupTable.h"

/* For C++ model's 'Config' object */
//#include "SBA/SystemConfiguration.h"

/* For C++ model's objects inside the service manager */
#include "SBA/ServiceManagerObjects.h"

/* For C++ model's  TaskDescription class*/
#include "SBA/TaskDescription.h"

/* For the C++ model's Gateway tile */
//#include "SBA/GatewayTile.h"

/* For the C++ model's Network model */
//#include "SBA/Network.h"

/* For the C++ model's Bridge */
//#include "SBA/Bridge.h"


//==============================================================================
/*	For all SC_SBA libraries, the path is relative to this source header file
	so maintain directory structure of the complete Gannet-HW folder */
//==============================================================================

// TODO: Have manually overwritten NSERVICES because currently
// the SystemC model hport_SC_Fifo_ifas to be manually updated for any change in number
// of tiles (because sc_exports cannot be declared as an array)
//#define NSERVICES 12



#include "SC_Debug.h"
#include "SC_Timing.h"


#include "SC_SBA_Interfaces.h"
#include "SC_Spec_Ports.h"

#include "SC_Helpers.h"
//#include "../lib/SC_Helpers.h"

#include "SC_SystemConfigurationConsts.h"
#include "SC_ServiceCoreLibrary.h"
#include "SC_SystemConfiguration.h"

#include "SC_Config.h"
//#include "SC_req_rsp.h"

#include "SC_Registers.h"
//#include "SC_SBA_Fifo.h"
#include "SC_Deque.h"
#include "SC_Fifo.h"
#include "SC_Word_List.h"
#include "SC_Stack.h"
#include "SC_RequestTable.h"
#include "SC_LookupTable.h"
#include "SC_Memory.h"
#include "SC_Subtask_List.h"

#include "SC_RegArbiter.h"
#include "SC_DequeArbiter.h"
#include "SC_StackArbiter.h"
#include "SC_FifoArbiter.h"
#include "SC_MemArbiter.h"
#include "SC_MemArbiter_dual.h"
#include "SC_StlistArbiter.h"


#include "SC_ServiceCore.h"
#include "SC_core_control.h"
#include "SC_receive_packets.h"
#include "SC_store_subtask_code.h"
#include "SC_activate_subtask.h"
#include "SC_store_data.h"
#include "SC_dispatch_data_packets.h"

#include "SC_parse_subtask.h"
#include "SC_prepare_subtask.h"
#include "SC_transmit_packets.h"




#include "SC_ServiceManager.h"
#include "SC_Tranceiver.h"

#include "SC_Tile.h"

#include "SC_Interface.h"
//#include "SC_receive_packets_gw.h"
//#include "SC_transmit_packets_gw.h"
#include "SC_parse_task_description_gw.h"
#include "SC_store_result_packets_gw.h"

#include "SC_Gateway.h"

#include "SC_GatewayTile.h"

#include "SC_Network.h"



#include "SC_System.h"

#include "SC_Runtime.h"

#endif /* SC_SBA_H_ */

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
//#include "../test/ServiceCoreLibrary.h"


/* Configuration file */
#include "../test/ServiceConfiguration.h"

/* Core typedefs */
#include "../test/Base/Types.h"

/* For Storage Class; Also contains FIFOS, Lists etc; but should use SC types */
#include "../test/Types.h"

/* Packet Types */
#include "../test/Packet.h"
//#include "../test/Packet.cc"

/* For Memory Class */
#include "../test/Memory.h"

/* For LookupTable */
#include "../test/LookupTable.h"

/* For C++ model's 'Config' object */
//#include "../test/SystemConfiguration.h"

/* For C++ model's objects inside the service manager */
#include "../test/ServiceManagerObjects.h"

/* For C++ model's  TaskDescription class*/
#include "../test/TaskDescription.h"

/* For the C++ model's Gateway tile */
//#include "../test/GatewayTile.h"

/* For the C++ model's Network model */
//#include "../test/Network.h"

/* For the C++ model's Bridge */
//#include "../test/Bridge.h"


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
#include "SC_receive_packets_gw.h"
#include "SC_transmit_packets_gw.h"
#include "SC_parse_task_description_gw.h"
#include "SC_store_result_packets_gw.h"

#include "SC_Gateway.h"

#include "SC_GatewayTile.h"

#include "SC_Network.h"



#include "SC_System.h"

#include "SC_Runtime.h"



#endif /* SC_SBA_H_ */

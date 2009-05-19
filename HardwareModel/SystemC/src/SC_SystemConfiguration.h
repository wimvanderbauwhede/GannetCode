
/** \file SystemConfiguration.h
   
 \brief Gannet Service-based SoC project - C++/SystemC System Configuration
        
        Generated from SBA.yml with create_Cxx_SystemConfiguration.rb
*/

/* ***** BEGIN LICENSE BLOCK *****
 * Version: AFL 2.1
 *
 * The contents of this file are subject to the Academic Free License Version
 * 2.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://opensource.org/licenses/afl-2.1.php
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 *  
 *
 * ***** END LICENSE BLOCK ***** */

//==============================================================================
//
// System Configuration
//
// GENERATED from YAML configuration using create_Cxx_SystemConfiguration.rb
//
//==============================================================================

// 

#ifndef _SC_SBA_SYSTEM_CONFIGURATION_H_
#define _SC_SBA_SYSTEM_CONFIGURATION_H_

#include <map>
#include "SC_sba.h"

using namespace std;

typedef unsigned int UINT;
// WV: FIXME: this should go elsewhere!
#define FP 0 // No floating-point support in ALU

namespace SC_SBA {


//Not elegant, but static arrays are a lot faster than linked lists!
const UINT NSERVICES = 15;

class Config {
	public:	
	Services services;

	Config()
	{
	unsigned int gw_address=NSERVICES; // must be the LAST address

	services[0]= ServicePair(gw_address,&SC_SBA::SCLib::sba_GATEWAY);

/*
 * It is crucial that the addresses (first elt of ServicePair) are contiguous
 * The service ids (indices of the services array) do not need to be (services is a map)
 * 
 * Currently, any id > 32 will be sent to the bridge
 * 
 */	

// services[service_id]=ServicePair(service_address,&SBA::SCLib::ls_LET);
#ifndef NO_SERVICES
	services[5]=ServicePair( 5,&SC_SBA::SCLib::ls_RAND,0,0 );
	services[11]=ServicePair( 11,&SC_SBA::SCLib::none,0,0 );
	services[6]=ServicePair( 6,&SC_SBA::SCLib::none,0,0 );
	services[12]=ServicePair( 12,&SC_SBA::SCLib::none,0,0 );
	services[1]=ServicePair( 3,&SC_SBA::SCLib::ls_IO,1,1 );
	services[7]=ServicePair( 7,&SC_SBA::SCLib::ls_ALU,0,1 );
	services[13]=ServicePair( 13,&SC_SBA::SCLib::none,0,0 );
	services[2]=ServicePair( 2,&SC_SBA::SCLib::ls_BEGIN,1,1 );
	services[8]=ServicePair( 8,&SC_SBA::SCLib::none,0,0 );
	services[14]=ServicePair( 14,&SC_SBA::SCLib::none,0,0 );
	services[3]=ServicePair( 1,&SC_SBA::SCLib::ls_LET,1,1 );
	services[9]=ServicePair( 9,&SC_SBA::SCLib::none,0,0 );
	services[15]=ServicePair( 0,&SC_SBA::SCLib::none,0,0 );
	services[4]=ServicePair( 4,&SC_SBA::SCLib::ls_IF,0,0 );
	services[10]=ServicePair( 10,&SC_SBA::SCLib::none,0,0 );
#endif // NO_SERVICES
    };    
	
};
		
} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_H_*/


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

#ifndef _SBA_SYSTEM_CONFIGURATION_H_
#define _SBA_SYSTEM_CONFIGURATION_H_

#ifndef STATIC_ALLOC
#include <map>
#endif
#include "ServiceCoreLibrary.h"

using namespace std;

typedef unsigned int UINT;
// WV: FIXME: this should go elsewhere!
#define FP 0 // No floating-point support in ALU

namespace SBA {

const UINT SC_LAMBDA = 0;
const UINT SC_DET1 = 0;
const UINT SC_INV1 = 0;
const UINT SC_GATEWAY = 0;
const UINT SC_DET2 = 0;
const UINT SC_INV2 = 0;
const UINT SC_A = 0;
const UINT SC_ALU = 0;
const UINT SC_SCALE = 0;
const UINT SC_B = 0;
const UINT SC_TRAN = 0;
const UINT SC_CROSS = 0;
const UINT SC_LET = 0;
const UINT SC_MMULT = 0;
const UINT SC_NONE15 = 0;
const UINT SC_IF = 0;
const UINT SC_MADD = 0;

const UINT S_LAMBDA = 16;
const UINT S_DET1 = 5;
const UINT S_INV1 = 11;
const UINT S_GATEWAY = 0;
const UINT S_DET2 = 6;
const UINT S_INV2 = 12;
const UINT S_A = 1;
const UINT S_ALU = 7;
const UINT S_SCALE = 13;
const UINT S_B = 2;
const UINT S_TRAN = 8;
const UINT S_CROSS = 14;
const UINT S_LET = 3;
const UINT S_MMULT = 9;
const UINT S_NONE15 = 15;
const UINT S_IF = 4;
const UINT S_MADD = 10;

const UINT A_READ = 2;
const UINT A_HEAD = 5;
const UINT A_eq = 15;
const UINT A_IFTC = 3;
const UINT A_LIST = 4;
const UINT A_gt = 14;
const UINT A_RETURNTC = 2;
const UINT A_LETTC = 10;
const UINT A_minus = 10;
const UINT A_TAIL = 6;
const UINT A_times = 11;
const UINT A_LENGTH = 7;
const UINT A_CONS = 8;
const UINT A_plus = 9;
const UINT A_lt = 13;
const UINT A_not = 21;
const UINT A_ASSIGN = 1;
const UINT A_APPEND = 9;
const UINT A_over = 12;
const UINT A_RETURN = 1;
const UINT A_UPDATE = 3;

//Not elegant, but static arrays are a lot faster than linked lists!
const UINT NSERVICES = 15;

class Config {
	public:	
	Services services;

	Config()
	{
	unsigned int gw_address=NSERVICES; // must be the LAST address

    // for static allocation. By checking service_address we know if the slot is empty or not
#ifdef STATIC_ALLOC
    for (uint i=0;i<MAX_NSERVICES;i++) {
            services[i]=ServicePair(MAX_NSERVICES,&SBA::SCLib::none);
    }
#endif

	services[0]= ServicePair(gw_address,&SBA::SCLib::sba_GATEWAY);

/*
 * It is crucial that the addresses (first elt of ServicePair) are contiguous
 * The service ids (indices of the services array) do not need to be (services is a map)
 * 
 * Currently, any id > 32 will be sent to the bridge
 * 
 */	

// services[service_id]=ServicePair(service_address,&SBA::SCLib::ls_LET);
#ifndef NO_SERVICES
	services[5]=ServicePair( 5,&SBA::SCLib::ls_PROC_MATRIX );
	services[11]=ServicePair( 11,&SBA::SCLib::ls_PROC_MATRIX );
	services[6]=ServicePair( 6,&SBA::SCLib::ls_PROC_MATRIX );
	services[12]=ServicePair( 12,&SBA::SCLib::ls_PROC_MATRIX );
	services[1]=ServicePair( 1,&SBA::SCLib::ls_RND_MATRIX );
	services[7]=ServicePair( 7,&SBA::SCLib::ls_ALU );
	services[13]=ServicePair( 13,&SBA::SCLib::ls_PROC_MATRIX );
	services[2]=ServicePair( 2,&SBA::SCLib::ls_RND_MATRIX );
	services[8]=ServicePair( 8,&SBA::SCLib::ls_PROC_MATRIX );
	services[14]=ServicePair( 14,&SBA::SCLib::ls_PROC_MATRIX );
	services[3]=ServicePair( 3,&SBA::SCLib::ls_LET );
	services[9]=ServicePair( 9,&SBA::SCLib::ls_PROC_MATRIX );
	services[15]=ServicePair( 0,&SBA::SCLib::none );
	services[4]=ServicePair( 4,&SBA::SCLib::ls_IF );
	services[10]=ServicePair( 10,&SBA::SCLib::ls_PROC_MATRIX );
#endif // NO_SERVICES
    };    
	
};
		
} // SBA
#endif /*_SBA_SYSTEM_CONFIGURATION_H_*/

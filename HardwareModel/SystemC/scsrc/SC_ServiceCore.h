/*
********************************************************************************
                 |
  File Name      | SC_ServiceCore.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'service_core' class
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

  */


#ifndef SC_SERVICE_CORE_H_
#define SC_SERVICE_CORE_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: SERVICE CORE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{



//! Service Core module, a wrapper around the actual IP core functionality
    /*!
       The Service Core module assumes that transactions with the IP core can be modelled as function calls.
       The Service Core provides access to the data store and lookup table on the Tile as well a
       a local state register. In this way it is possible to model stateful IP cores.
       The IP core can override the default exit status (CS_done) to indicate that it is stateful and not finished.
    */

// SC_ServiceCore_signature contains all the ports etc
#include "SC_ServiceCore_signature.h"


//==============================================================================
//	DO_PROC()
//==============================================================================
void SC_ServiceCore :: do_proc()
{
	while (true) {
		// wait until there is a change in the core_status
		// and then proceed only if status is CS_busy, indicating core should
		// now perform the task
//		cout << "SC_ServiceCore: waiting for event\n";
		wait(core_status.value_changed_event());
		/*
		#if MULTI_THREADED_CORE==1
		        if (core_status[tid]==CS_busy ){

		#ifdef VERBOSE
		            cout << "" <<service<< " ServiceCore " <<service<< " (tid:" <<tid<< "): " <<arg_addresses[tid].size()<< " addresses"<<endl;
		#endif // VERBOSE
		            core_status[tid]=CS_done;

		    		FuncPointer fp=sba_system.cfg.services[service_id[tid]].core;
		            results_store[tid]=(*fp)((Base::ServiceCore*)this,arg_addresses[tid]);

		        }
		#else // MULTI_THREADED_CORE==0
		*/
        if (core_status==CS_busy ){

#ifdef VERBOSE
            //cout << "" <<service<< " ServiceCore: " <<arg_addresses.size()<< " addresses"<<endl;
            cout << service <<" ServiceCore: " <<service_id <<":"<<arg_addresses.size()<< " addresses"<<endl;
#endif // VERBOSE

            local_core_status=CS_done;

            // read values by address
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_ServiceCore: core starts"<< endl;
#endif
    		arg_addresses_local = arg_addresses.read_all();



			//Uint opcode_val=opcode.read();
    		// wait for as long as core takes
            // t_proc_value is a bit difficult: is it the time per in put argument Word? Or per address?
            // e.g. if it's matrix inversion then obv. it should be proportional to the size of the input matrix
            // but what if there are several input arguments?
            // the current solution, is to take the sum of sizes of all args
    		uint argsz=0;
            for(MemAddresses::iterator iter_=arg_addresses_local.begin();iter_!=arg_addresses_local.end();iter_++) {
                MemAddress address=*iter_;
				argsz+=data_store.size(address);
			}
            // The mechanism below does not work for service cores with aliases!
            // we must add the aliases as a separate table and look up by opcode
            uint t_core = cfg.services(service).t_setup + cfg.services(service).t_proc_value*argsz;
#ifdef SC_VERBOSE
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << service << " SC_ServiceCore: waiting "<<t_core<<" ns\n";
#endif // SC_VERBOSE
            wait(t_core*_CLK_P, _CLK_U);
            if (opcode==A_S_RETURN or opcode==A_S_IF) {
            	results_store=SCLib::ls_S_IF((void*)this,arg_addresses_local);
            } else {
            SC_FuncPointer fp=cfg.services(service).core;
    		results_store=(*fp)((void*)this,arg_addresses_local);
            }
    		OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<name()<<" ("<<service<<")" <<" SC_ServiceCore: core has finished with status "<<core_status<< endl;
#ifdef VERBOSE
    		for (unsigned int i=0;i<results_store.size();i++) {
			cout << "SC_ServiceCore: RESULT["<< i <<"]: "<< results_store[i] << "\n";
    		}
#endif
        	if (core_status==CS_busy) {
        		core_status=CS_done;
        		}
    		//core_status=local_core_status;
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            <<name()<<" SC_ServiceCore:: set core_status to "<<core_status<<"\n";
#endif
        } else {
#ifdef SC_VERBOSE
//			cout << "SC_ServiceCore: core_status changed to "<<core_status<<"\n";
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            <<name()<<" SC_ServiceCore: core_status changed to "<<core_status<<"\n";
#endif
		}//if
	}//while(true)



//#endif //  MULTI_THREADED_CORE

} // funct: SC_ServiceCore :: do_proc()




} //namespace: SC_SBA

#endif /* SC_SERVICE_CORE_H_ */

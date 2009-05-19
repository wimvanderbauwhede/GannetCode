/*
********************************************************************************
                 |
  File Name      | SC_store_data.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'store_data_packets' class
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


#ifndef SC_STORE_DATA_H_
#define SC_STORE_DATA_H_



//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: STORE DATA
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "store data " module
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_store_data : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;     //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T, RegisterEntry>register_set;   //!< write/read port for register_set access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
    port_SC_Fifo_if     <Packet_t>          tx_fifo    ;    //!< write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;      //!< read port to access data_fifo
    port_SC_Memory_if   <ADDR_T, SBA::Word> symbol_table;   //!< port for access to symbol_table
//    port_SC_Memory_if   <ADDR_T, Requests>  request_table;  //!< port for access to request_table
    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< w/r port for access to pending_subtasks_fifo
    port_SC_reg_if      <SBA::Service >     service;

    // ---------------------------- METHODS ------------------------------------
	void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_store_data"; }


	// ---------------------------- CONSTRUCTOR --------------------------------

	SC_HAS_PROCESS(SC_store_data);

	SC_store_data(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
	}
private:
};//class: SC_store_data

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_store_data <ADDR_T, DATA_T> :: do_proc()
{
#include "../gensrc/ServiceManager/store_data.cc"
/*
#ifdef VERBOSE
//if (debug_all or service==debug_service){
    cout << "" <<service<< " store_data()"<<endl;
//}
#endif // VERBOSE
    while(true)
    {
        Packet_t data_packet=data_fifo.shift();
#ifdef VERBOSE
//if (debug_all or service==debug_service){
        cout << ppPacket(data_packet) <<"\n";
//}
#endif // VERBOSE
        Word label= getReturn_as(getHeader(data_packet));
        MemAddress data_address=getSubtask(label) & F_DataAddress;

#ifdef VERBOSE
//if (debug_all or service==debug_service){
        cout << "" <<service<< " store_data() ADDRESS: " <<data_address<< ""<<endl;
//}
#endif // VERBOSE
        if (getStatus(symbol_table[data_address])!=DS_present)
        {
#ifdef VERBOSE
//if (debug_all or service==debug_service){
        cout << "" <<service<< " store_data() STATUS: ";
        cout << (int)getStatus(symbol_table[data_address])<<"\n";
//}
#endif // VERBOSE
            if (getLength_p(data_packet)==0)
            {
                symbol_table[data_address]=setStatus(symbol_table[data_address],DS_present);
            }
            else
            {
                Payload_t data_packet_payload=getPayload(data_packet);
                data_store.mput(data_address,data_packet_payload);
                symbol_table[data_address]=setStatus(symbol_table[data_address],DS_present);
#ifdef VERBOSE
//if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() STATUS: ";
                cout << (int)getStatus(symbol_table[data_address])<<"\n";
//}
#endif // VERBOSE
            }

            Subtask subtask=getSubtask(symbol_table[data_address]);
//            cout << service << " SUBTASK LIST: "<<subtask<< " status: "<<subtask_list.status(subtask) << endl;
#ifdef VERBOSE
//if (debug_all or service==debug_service){
             cout << "" <<service<< " store_data() SUBTASK: " <<subtask<< ""<<endl;
//}
#endif // VERBOSE
            if(subtask_list.status(subtask)!=STS_deleted) {

            subtask_list.decr_nargs_absent(subtask);



            if(subtask_list.status(subtask)!=STS_inactive  )
            {
#ifdef VERBOSE
//if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " in list"<<endl;
//}
#endif // VERBOSE

                if (subtask_list.nargs_absent(subtask)==0)
                {
#ifdef VERBOSE
//if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() push SUBTASK " <<subtask<< " onto pending_subtasks_fifo"<<endl;
//}
#endif // VERBOSE
                    subtask_list.status(subtask,STS_pending);
                    pending_subtasks_fifo.push(subtask);
                } else {
#ifdef VERBOSE
//if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " still missing "<< subtask_list.nargs_absent(subtask) <<" arg(s)"<<endl;
//}
#endif // VERBOSE

                }//if
            }//if(subtask_list.status(subtask)!=STS_inactive )
            }//if(subtask_list.status(subtask)!=STS_deleted)
        }//if(getStatus(symbol_table[data_address])!=DS_present)
    }//while(true)
*/

}// funct: SC_store_data :: do_proc()

} //namespace: SC_SBA

#endif /* SC_STORE_DATA_H_ */

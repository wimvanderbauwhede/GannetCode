/*
********************************************************************************
                 |
  File Name      | SC_dispatch_data_packets.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        |  04-Dec-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'SC_dispatch_data_packets' class
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


#ifndef SC_DISPATCH_DATA_PACKETS_H_
#define SC_DISPATCH_DATA_PACKETS_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//  CLASS: DISPATCH DATA PACKETS
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the SC_dispatch_data_packets module
    /*!
       Dispatch data packets in answer to a request. Essential for STREAM and CACHE/ACC
    */
template <typename ADDR_T, typename DATA_T>
class SC_dispatch_data_packets :
    public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------

    port_SC_Fifo_if     <Packet_t>          tx_fifo;        //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;      //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          request_fifo;   //!< write port to access request_fifo
//    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< write port to access subtask_code_fifo
//    port_SC_Fifo_if     <Packet_t>          subtask_reference_fifo;//!< write port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;   //!< read port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;   //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T,RegisterEntry> register_set;   //!< write/read port for register-set access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
//    port_SC_Stack_if                        data_address_stack;//!< w/r port for data_address_stack
//    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store;   //!< spec. w/r port for access to code_store (read-only access needed actually)
    port_SC_Memory_if   <ADDR_T, Word>      symbol_table;   //!< port for access to symbol_table
    port_SC_reg_if      <Service>           service;        //!< for reading service id
//    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< write port for access to pending_subtasks_fifo

	port_SC_LookupTable_if		lookup_table;// WV: not implemented at all!
    port_SC_RequestTable_if		request_table;
    // ---------------------------- LOCALS -------------------------------------

    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    void restart_subtask(MemAddress regaddr);
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_dispatch_data_packets"; }
    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_dispatch_data_packets);
    SC_dispatch_data_packets(sc_module_name nm) : sc_module(nm)
    {
        SC_THREAD(do_proc);
        //dont_initialize();
        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
};//class: SC_dispatch_data_packets

//==============================================================================
//  DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_dispatch_data_packets <ADDR_T, DATA_T>:: do_proc()
{

#ifdef VERBOSE
//if (debug_all or service==debug_service ){
cout << "" <<service<< " dispatch_data_packets()"<<endl;
//}
#endif // VERBOSE
        Packet_Fifo pending_requests_fifo;
        while (true){
            Packet_t request=request_fifo.shift();
#ifdef VERBOSE
//if (debug_all or service==debug_service ){
cout <<  ppPacket(request)<<endl;
//}
#endif // VERBOSE
            Word packet_label= getReturn_as(getHeader(request));
            Word var_label=getPayload(request)[0];
            MemAddress  data_address=0;

                bool has_label=0;
                DS_t data_status=DS_absent;
                 uint mode;
            if (getKind(var_label)==K_D ) {
                uint reg_address=getReg(var_label);
                mode=getMode(var_label);
                data_address=reg_address;
#ifdef VERBOSE
//                if (debug_all or service==debug_service                ){
                    cout << "dispatch_data_packets(): Mode: CACHE: reg " <<reg_address<< ", mode " <<mode<< ""<<endl;
//                }
#endif // VERBOSE
                if (register_set[reg_address].status==RDS_present ){
                    has_label=1;
                    data_status=getStatus(symbol_table[reg_address]);
#ifdef VERBOSE
//                    if (debug_all or service==debug_service){
                        cout << "dispatch_data_packets(): Mode: CACHE: data address " <<data_address<< ", status " <<data_status<< ""<<endl;
//                    }
#endif // VERBOSE
                } else {
                    data_status=getStatus(symbol_table[reg_address]);
                    request_table.push(reg_address,packet_label);
#ifdef VERBOSE
//                    if (debug_all or service==debug_service){
                        cout << "dispatch_data_packets(): Mode: CACHE: queue request for reg " <<reg_address<< " (" <<register_set[reg_address].data_address<< ")"<<endl;
//                    }
#endif // VERBOSE
                 }
                   if (mode==M_stream ){
                       restart_subtask(reg_address);
                   }

            } else if (getKind(var_label)==K_L) {


                if (lookup_table.count(getName(var_label))==1){
                    has_label=1;
//                    cout << "VAR_LABEL:"<<var_label<<" name:"<<(int)getName(var_label)<<"\n";
                    Word word=lookup_table.read(getName(var_label));
//                    cout <<"WORD: "<<word<<"\n";
                    data_address= getSubtask(word);
                     data_status=(Data_Status)getStatus(word);
                }

            } else {
                    cerr << "WHY " <<getKind(var_label)<< "?";
                    exit(1);
                    data_address=getSubtask(var_label);
            }
            if (has_label==1 and data_status==DS_present) {

                Length_t payload_length=0;
                 Word_List tdata;
//                 cout << "ADDRESS: "<<data_address<<"\n";
                    tdata=data_store.mget(data_address);
                    payload_length=tdata.size();
                Header_t packet_header = mkHeader(P_data,0,0,payload_length, getReturn_to(getHeader(request)), NA,0,packet_label);
                Word_List packet_payload=tdata;
                Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
                cout << "DISPATCHING DATA PACKET: \n";
                cout << ppPacket(packet)<<endl;
#endif // VERBOSE
                if (getTo(packet_header) != service){
                    tx_fifo.push(packet);
                } else {
                    data_fifo.push(packet);
                }

            } else {
                if (getKind(var_label)==K_L ){
                    pending_requests_fifo.push(request);
#ifdef VERBOSE
                cout << "Pushed request onto pending_requests_fifo ... BROKEN!"<<endl;
#endif // VERBOSE
                }
            }
        }
/*
 * We can no longer postpone dealing with this hack
 * What happened up to now for K_L was simply cycling the requests:
 * If a data request could not be honoured the request is pushed onto the
 * pending_requests_fifo; in parallel requests are shifted off that fifo
 * and unshifted (!) onto the request_fifo
 * What we should do is have a table of the requests, just like for K_D.
 * As K_L is actually software-only and as such not very important in SystemC,
 * we can have a similar requests_table but bigger.
 * */
        // FIXME: implement this as a requests table rather than just commenting it out!
        /*
        while (pending_requests_fifo.size()>0){
            Packet_t pending_request =  pending_requests_fifo.shift();
            request_fifo.unshift(pending_request);
        }
         */

}// funct: SC_dispatch_data_packets :: do_proc()



template <typename ADDR_T, typename DATA_T> void SC_dispatch_data_packets <ADDR_T, DATA_T>:: restart_subtask(MemAddress regaddr) {

#ifdef VERBOSE
//        if (debug_all or service==debug_service ){
            cout << "" <<service<< " restart_subtask(" <<regaddr<< ")"<<endl;
//        }
#endif // VERBOSE
        MemAddress subtask_address=register_set[regaddr].subtask_address;
        subtask_list.status(subtask_address,STS_new);

#if VM==1
        MemAddress code_address= register_set[regaddr].code_address;
        Word subtask_word=(code_address & 0x0000FFFF) + ( (subtask_address << 16) & 0xFFFF0000);
//        cout << "restart_subtask VM==1: pushing" << subtask_word<< "\n";
        subtask_fifo.push(subtask_word);
#else // VM==0
//        cout << "restart_subtask VM=0: pushing " << subtask_address << "\n";
        subtask_fifo.push(subtask_address);
#endif // VM

    }



} //namespace: SC_SBA


#endif /* SC_DISPATCH_DATA_PACKETS_H_ */

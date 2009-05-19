/*
********************************************************************************
                 |
  File Name      | SC_core_control.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Service Core class (MULTITHREAD==0)
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

#ifndef SC_CORE_CONTROL_H_
#define SC_CORE_CONTROL_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: SERVICE MANAGER
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This is core control for single-threaded tile's manager
/*!
   Detailed description here...
*/
template <typename ADDR_T, typename DATA_T>
class SC_core_control :
            public sc_module {
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;   //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T, RegisterEntry>register_set;   //!< write/read port for data memory access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
    port_SC_Fifo_if     <Packet_t>          tx_fifo;            //!< write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;          //!< write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          request_fifo;       //!< write port to access request_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< write port to access subtask_code_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_reference_fifo;//!< write port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;       //!< write port to access subtask_fifo
    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< write port for access to pending_subtasks_fifo
    port_SC_Deque_if    <Word>              results_store;      //!< Fifo access to the results store
    port_SC_reg_if      <bool>              ack_ok;             //!< access to ack_ok boolean
    port_SC_reg_if      <Packet_Type>       core_return_type;   //!< ...
    port_SC_reg_if      <Subtask     >      current_subtask;
    port_SC_reg_if      <uint             > n_args;
    port_SC_reg_if      <uint             > opcode;
    port_SC_reg_if      <SBA::Core_Status>  core_status;    //!< write/read port for code_status access
    port_SC_RequestTable_if  request_table;  //!< port for access to request_table
    // following access for clean_up() only
    port_SC_Memory_if   <ADDR_T, SBA::Word> symbol_table;   //!< port for access to symbol_table
    // following access for clean_up() only
    port_SC_Stack_if                        data_address_stack;//!< w/r port for data_address_stack
    port_SC_Deque_if    <SBA::MemAddress>   arg_addresses;  //!< read write port for arg_addresses memory
    port_SC_reg_if      <SBA::Service>      service;        //!< for reading service id

    // ---------------------------- LOCALS -------------------------------------
    uint scid;  // Should this be an SC_Register in the service_manager, accessed through the port?

    // ---------------------------- METHODS ------------------------------------
    void                do_proc(); // the SC_thread

    //Helper methods used in do_proc() thread
    SBA::MemAddresses   build_value_address_list(); // for initializing arg_addresses
    void                send_ack();
    void                clean_up();


    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const {
        return "SC_core_control";
    }

    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_core_control);

    SC_core_control(sc_module_name nm) : sc_module(nm) {
        SC_THREAD(do_proc);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
}
;//class: SC_core_control

//==============================================================================
// BUILD_VALUE_ADDRESS_LIST()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
SBA::MemAddresses SC_core_control<ADDR_T, DATA_T> :: build_value_address_list() {
    MemAddresses  addresses;
    // SC_Register type (e.g. current_subtask) has implicit cast overloaded to return its data
//    cout << service << " build_value_address_list(): "<<current_subtask<<" has "<<subtask_list.nargs(current_subtask)<< " args\n";
    unsigned int nargs=subtask_list.nargs(current_subtask);
    if (nargs>0) {
//        cout << "build_value_address_list(): getting arg addresses for "<<current_subtask<<"\n";
        Subtask_Argument_List&  args = subtask_list.arguments(current_subtask);
//        cout << service << " build_value_address_list(): " << args.size() <<" arg addresses\n";
//        cout << service << " build_value_address_list(): " << subtask_list.nargs(current_subtask) <<" actual args\n";

        for (MemAddresses::iterator iter_=args.begin() ; iter_!=args.end() ; iter_++) {
            MemAddress address=*iter_;
            addresses.push_back(address);
            nargs--;
            if (nargs==0) {
                break;
            }
        }//for
//        cout << service << " build_value_address_list(): " << addresses.size() <<" addresses\n";
    }//if
    return addresses;
} //build_value_address_list()

//==============================================================================
// SEND_ACK()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T> :: send_ack() {
#ifdef VERBOSE
    //if (debug_all or service==debug_service ){
    cout << "" <<service<< " ACK: send_ack()"<<endl;
    //}
#endif // VERBOSE

    Service to=getName( subtask_list.ack_to(current_subtask) );
    Service return_to=service;
    Word return_as=subtask_list.ack_to(current_subtask);
    Length_t  payload_length=1;
    Prio_t prio=0;
    Redir_t  redir=0;
    Word ack_to=subtask_list.ack_to(current_subtask);
    Header_t packet_header = mkHeader(P_data,prio,redir,payload_length,to,return_to,ack_to,return_as);
    Word_List packet_payload;
    packet_payload.push_back(return_as);
    Packet_t packet = mkPacket(packet_header,packet_payload);

    if (to != service ) {
#ifdef VERBOSE
        cout << "" <<service<< " send_ack(): Sending packet via NoC: " <<to<< "<>" <<service<< ""<<endl;
#endif // VERBOSE
        tx_fifo.push(packet);
    }//if (to != service )

    else {
        demux_packets_by_type(packet,data_fifo,subtask_code_fifo,request_fifo,subtask_reference_fifo);
        //demux_packets_by_type(packet);
    }
}//send_ack()

//==============================================================================
// CLEAN_UP()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T>:: clean_up() {
//    Subtask_Argument_List& args=subtask_list.arguments(current_subtask);

#ifdef VERBOSE
//if (debug_all or service==debug_service ){
//    cout << "" <<service<< ": clean_up "<<current_subtask<<": ADDRESSES: " <<args.size()<< ""<<endl;
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
//    cout << ""
    <<service<< ": clean_up "<<current_subtask<<": ADDRESSES: " <<arg_addresses.size()<< ""<<endl;
//}
#endif // VERBOSE
		unsigned int nargs=arg_addresses.size();
//    for (Word_List::iterator iter_=args.begin();iter_!=args.end();iter_++) {
    	for (unsigned int iter_=0;iter_!=nargs;iter_++) {
//        Word arg_address=*iter_;
        MemAddress arg_address=arg_addresses.at(iter_);
#ifdef VERBOSE
//if (debug_all or service==debug_service ){
        cout << "" <<service<< ": clean_up:  ADDRESS: " <<arg_address<< ""<<endl;
//}
#endif // VERBOSE

        if (getStatus(symbol_table[arg_address])==DS_present and arg_address>NREGS+DATA_OF)  {
            symbol_table[arg_address]=setStatus(symbol_table[arg_address],DS_cleared);
            data_address_stack.push(arg_address);

#ifdef VERBOSE
//if (debug_all or service==debug_service ){
            cout << "" <<service<< ": clean_up(): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
            cout << "" <<service<< ": clean_up: REMOVED " <<arg_address<< ""<<endl;
//}
#endif // VERBOSE
            //WN: What does remove mean in context of a memory
            // is anything else needed, other than pushing address back in stack?
            // followign has been commented until clarification
            //data_store.remove(arg_address);
        }//if
    }//for

}//clean_up()


//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_core_control<ADDR_T, DATA_T> :: do_proc() {
#ifdef VERBOSE
//        if (debug_all or service==debug_service ){
    cout << "" <<service<< " core_control(): core_status = " <<core_status<< ""<<endl;
//        }
#endif // VERBOSE

    while (true) {
//    	OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" core_control(): core_status = " <<core_status<< endl;
        uint redir=0;
        //Subtask_Status sts=subtask_list.status(current_subtask);
        //-------------------------
        if (core_status == CS_idle) { //  and pending_subtasks_fifo.size()>0
            current_subtask=pending_subtasks_fifo.shift();
#ifdef VERBOSE
            cout << "" <<service<< " core_control(): set core_status to CS_Ready"<<endl;
#endif // VERBOSE
            //SC_Register type (e.g. current_subtask) has implicit cast overloaded to return its data
            if (subtask_list.status(current_subtask)==STS_pending) {
#ifdef VERBOSE
                cout << "" <<service<< " core_control(): set to status for subtask "<< current_subtask<< " to STS_processing"<<endl;
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_processing);
            }
            core_status = CS_ready;
        } else {//if (core_status != CS_idle)
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            <<name()<<" core_control(): Waiting for ServiceCore (core_status=="<<core_status<<")\n";
#endif
            if (core_status != CS_done) {
                wait(core_status.value_changed_event());
            }
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            <<name()<<" core_control(): done waiting (core_status=="<<core_status<<")\n";
#endif
        }
        //--------------------------
        if (core_status == CS_ready) {//1
            n_args=subtask_list.nargs(current_subtask);

            // build_value_address_list()??
            arg_addresses=build_value_address_list();
            Symbol_t called_as=subtask_list.called_as(current_subtask);
            opcode=getName(called_as) & F_Opcode;
            scid=(getName(called_as) & F_SCId) >> FS_SCId;
#ifdef VERBOSE
            cout << "" <<service<< " CORE CONTROL: " <<arg_addresses[0]<< ""<<endl;
#endif // VERBOSE
            results_store.clear();
            core_status=CS_busy; //2
        }//if (core_status == CS_ready)

        //--------------------------
        if (core_status == CS_done) {//3
#ifdef VERBOSE
            cout << service <<" core_control(): ServiceCore is done!\n";
#endif
            if (subtask_list.status(current_subtask)==STS_processing) {
                subtask_list.status(current_subtask,STS_processed);
            }

            Service to=subtask_list.to(current_subtask);
            Service return_to=subtask_list.return_to(current_subtask);
            Word return_as=subtask_list.return_as(current_subtask);
            uint prio=0;
            redir=subtask_list.redir(current_subtask);
            //return_to=subtask_list.return_to(current_subtask);
            Word ack_to=subtask_list.ack_to(current_subtask);
#ifdef VERBOSE
//    if (debug_all or service==debug_service                 ){
            cout << "" <<service<< " core_control(): REDIR: " <<redir<< "; ACK_TO: " <<ack_to<< ""<<endl;
            cout << "" <<service<< " core_control(): RETURN_TO: " <<return_to<< " (" <<current_subtask<< ")"<<endl;
            cout << "" <<service<< " core_control(): RETURN_AS: " <<return_as<< " (" <<current_subtask<< ")"<<endl;
//    }
#endif // VERBOSE
            Length_t payload_length;

            Word_List           packet_payload;
            //deque<Word>         packet_payload;
            //SC_Deque <Word> packet_payload("packet_payload");// TODO: Does this need to be an SC_Deque?

            uint mode=subtask_list.mode(current_subtask);
            uint offset=subtask_list.offset(current_subtask);
            uint fsize=subtask_list.fsize(current_subtask);

            if (mode==M_normal) {
                //WN: packet_payload should be of type Word_List (=List<Word>), while results_store
                // returns deque<Word>. So temp deque plus swapping to do the job TODO: Workaround this?
//                deque<Word> temp_deque2 = results_store.read_all();
//                temp_deque2.swap(packet_payload);
                Word_List results(results_store.read_all());
                packet_payload=results;
                //packet_payload=results_store.read_all(); //<-- TODO: Reading complete FIFO? Should this be available in hardware?
                //WN
                payload_length=packet_payload.size();
            } else {//if (mode!=M_normal)
                uint reg_addr = subtask_list.reg(current_subtask);
                Symbol_t reg_symbol = mkSymbol(K_D,0,0,0,0,((mode << FS_Mode)+(reg_addr << FS_Reg)),service);
#ifdef VERBOSE
//                if (debug_all or service==debug_service){
                //  cout << "core_control(): Mode " <<mode<< ": buffering " <<ppPayload(results_store)<< " in reg " <<reg_addr<< ""<<endl;
//                }
#endif // VERBOSE
                payload_length=0;

                // WN: Since data_store.mput expects "Data" (=List<Word>), and results_store.read_all()
                // returns deque<Word>, so we use the swap functionality and temporary variables
                // to work around this.
                // TODO: Anyway to avoid this?
//                deque<Word> temp_deque = results_store.read_all();
//                Data   temp_Data;
//                temp_deque.swap(temp_Data);
                // WN
                Data results(results_store.read_all());
                data_store.mput(reg_addr, results);
                register_set[reg_addr].status=RDS_present;
                Word reg_status_symbol=setStatus(reg_symbol,DS_present);
                symbol_table[reg_addr]=reg_status_symbol;

                if (request_table.size(reg_addr)>0) {
                    Word packet_label=request_table.shift(reg_addr);
                    Length_t payload_length=0;
                    Header_t packet_header = mkHeader(P_data,0,0,payload_length, getName(packet_label), NA,0,packet_label);
                    MemAddress data_address = register_set[reg_addr].data_address;
                    Word_List tdata = data_store.mget(data_address);
                    payload_length = tdata.size();
                    Packet_t packet = mkPacket(packet_header,tdata);
                    if (getTo(packet_header) != service) {
                        tx_fifo.push(packet);
                    } else {
                        data_fifo.push(packet);
                    }
                }//if (request_table[reg_addr].size()>0){
            }//else --> if (mode==M_normal)

            Header_t packet_header = mkHeader(core_return_type,prio,redir,payload_length,to,return_to,ack_to,return_as);
            Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
//if (debug_all or service==debug_service             ){
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<   " core_control(): PACKET:\n " <<ppPacket(packet)<< ""<<endl;
            cout << "" <<service<< " core_control(): PACKET:\n " <<ppPacket(packet)<< ""<<endl;
//}
#endif // VERBOSE
            if (to != service ) {
                tx_fifo.push(packet);
            } else {
                demux_packets_by_type(packet,data_fifo,subtask_code_fifo,request_fifo,subtask_reference_fifo);
            }
        }//if (core_status == CS_done)

        //------------------------------------------------------
        if (core_status == CS_done or core_status == CS_managed) {
            redir=subtask_list.redir(current_subtask);
            if (redir==1  ) {
#if VM==1
                if (ack_ok==1) {
                    send_ack();
                }
#else // VM==0
                send_ack(); //<---
#endif // VM
            }//if (redir==1  )
            uint mode=subtask_list.mode(current_subtask);


            Subtask_Status sts=subtask_list.status(current_subtask);
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<   " core_control(): get status for " << current_subtask << ": "<< sts<<endl;
#endif // VERBOSE
            if ((sts==STS_processed or sts==STS_cleanup or sts==STS_inactive) ) { //and mode!=M_stream ) {
                if (sts==STS_processed or sts==STS_inactive) {
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<   " core_control(): clean_up() " << current_subtask << ""<<endl;
#endif // VERBOSE
                    clean_up(); //<---
                    if (sts==STS_processed) {
						subtask_list.status(current_subtask,STS_cleanup);
						sts=STS_cleanup;
                    }
                }

                if (sts==STS_processed or sts==STS_cleanup) {
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<   " core_control(): remove " << current_subtask << ""<<endl;
#endif // VERBOSE
                subtask_list.remove(current_subtask);
#if VM==1
                subtasks_address_stack.push(current_subtask);
#endif // VM
                current_subtask=0;
#ifdef VERBOSE
//if (debug_all or service==debug_service             ){
                cout << "" <<service<< " core_control(): removed subtask " <<current_subtask <<endl;
//}
#endif // VERBOSE
                }
            } else if (sts==STS_blocked ) {
#ifdef VERBOSE
//if (debug_all or service==debug_service             ){
                cout << "" <<service<< " core_control(): reset subtask " <<current_subtask <<" status to STS_new"<<endl;
//}
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_new);
            } else {
            	if (core_status == CS_managed) { // WV16042009: might have to qualify the subtask status
#ifdef VERBOSE
//if (debug_all or service==debug_service             ){
                cout << "" <<service<< " core_control():  clean_up() for subtask " <<current_subtask <<endl;
//}
#endif // VERBOSE
                clean_up() ;

            	} else {
#ifdef VERBOSE
//if (debug_all or service==debug_service             ){
                cout << "" <<service<< " core_control(): no clean-up for subtask " <<current_subtask <<endl;
//}
#endif // VERBOSE
            	}
            }
            results_store.clear();
            core_status= CS_idle;
        }//if (core_status == CS_done or core_status == CS_managed)
    } // end of while(true)
}//SC_core_control :: do_proc()

} //namespace: SC_SBA
#endif /* SC_CORE_CONTROL_H_ */

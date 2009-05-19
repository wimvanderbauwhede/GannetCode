/*
********************************************************************************
                 |
  File Name      | SC_parse_subtask.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'parse_subtask_packets' class
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081031: Created.
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


#ifndef SC_PARSE_SUBTASK_H_
#define SC_PARSE_SUBTASK_H_


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
//	CLASS: PARSE SUBTASK
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This is the "SC_parse_subtask" module
/*!
   Detailed description here...
*/
template <typename ADDR_T, typename DATA_T>
class SC_parse_subtask : public sc_module {
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if     <Packet_t>          tx_fifo;        //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          data_fifo;      //!< delayed write port for accessing tx_fifo
    port_SC_Fifo_if     <Packet_t>          request_fifo;   //!< write port to access request_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< write port to access subtask_code_fifo
    port_SC_Fifo_if     <Packet_t>          subtask_reference_fifo;//!< write port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;   //!< read port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, DATA_T>    data_store;   //!< write/read port for data memory access
    port_SC_Memory_if   <ADDR_T,RegisterEntry> register_set;   //!< write/read port for register-set access
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
    port_SC_Stack_if                        data_address_stack;//!< w/r port for data_address_stack
    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store;   //!< spec. w/r port for access to code_store (read-only access needed actually)
    port_SC_Memory_if   <ADDR_T, Word>      symbol_table;   //!< port for access to symbol_table
    port_SC_reg_if      <Service>           service;        //!< for reading service id
    port_SC_Fifo_if     <Subtask>           pending_subtasks_fifo;//!< write port for access to pending_subtasks_fifo


    // ---------------------------- METHODS ------------------------------------
    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const {
        return "SC_parse_subtask_packets";
    }

    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_parse_subtask);

    SC_parse_subtask(sc_module_name nm) : sc_module(nm) {
        SC_THREAD(do_proc);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
}
;//class: SC_parse_subtask

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_parse_subtask<ADDR_T, DATA_T> :: do_proc() {
    while (true) {
        Word subtask_word=(Word)subtask_fifo.shift();

#if VM==1
                    Subtask parent_subtask=(subtask_word & 0xFFFF0000)>>16;
                    MemAddress code_address= subtask_word & 0x0000FFFF;
#else // VM==0

        Subtask parent_subtask=(Subtask)(subtask_word & 0x0000FFFF);
        MemAddress code_address=(MemAddress)(subtask_word & 0x0000FFFF);
#endif // VM
// Next line costs a cycle!
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): start parsing " <<parent_subtask<< endl;


#ifdef VERBOSE
//            if (debug_all or service==debug_service ){
        cout << "" <<service<< " parse_subtask(): parsing " <<parent_subtask<< " (" <<code_address<< ")"<<endl;
//            }

#endif // VERBOSE
        Word_List subtask=code_store.mget(code_address);
        Word service_symbol=subtask.shift();
        subtask_list.called_as(parent_subtask,service_symbol);
        subtask_list.nargs(parent_subtask,getNArgs(service_symbol));
        unsigned int nargs_absent=getNArgs(service_symbol);
        subtask_list.nargs_absent(parent_subtask,nargs_absent);

        uint mode=getMode(service_symbol);
        uint reg_addr=0;
        if (mode!=M_normal ) {
            reg_addr=getReg(service_symbol);
            subtask_list.mode(parent_subtask,mode);
            subtask_list.reg(parent_subtask,reg_addr);
            if ( register_set[reg_addr].status==RDS_absent) {
#ifdef VERBOSE
//if (debug_all or service==debug_service){
                cout << "parse_subtask(): Mode " <<mode<< ": reg " <<reg_addr<< ""<<endl;
//}
#endif // VERBOSE
                register_set[reg_addr].data_address=reg_addr;
                register_set[reg_addr].code_address=code_address;
                //WN: RegisterSet has no member named subtask_address? Line below commented until clarification
                //register_set[reg_addr].subtask_address=parent_subtask;
                register_set[reg_addr].subtask_address=parent_subtask;
            }//if

        }//if mode!=M_normal
#ifdef VERBOSE
//if (debug_all or service==debug_service){

        cout << "" <<service<< " parse_subtask(): NARGS:" <<getNArgs(service_symbol)<< ""<<endl;
        cout << "" <<service<< " parse_subtask(): Subtask length: " <<subtask.size()<< ""<<endl;
        cout << "" <<service<< " parse_subtask(): stack size " <<data_address_stack.size()<< ""<<endl;
//}
#endif // VERBOSE

        uint i=0;
        while (subtask.size()>0) {
            Word elt=subtask.shift();
            if (data_address_stack.size()==0) {
                cerr << "" <<service<< " parse_subtask(): ADDRESS STACK (" <<DATA_SZ<< ") OVERFLOW for subtask " <<parent_subtask<< "";
                exit(1);
            }
//            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): pop stack " << endl;
            MemAddress data_address;
            if (getKind(elt)!=K_C) {
                data_address=data_address_stack.pop();
            } else {
                data_address= (elt & F_Reg) >> FS_Reg;
            }
//            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): popped stack " << endl;
#ifdef VERBOSE
//    if (debug_all or service==debug_service     ){
            //cout << "" <<service<< " parse_subtask(): data address " <<data_address<< " for " <<ppSymbol(elt)<< ""<<endl;
            //cout << "" <<service<< " parse_subtask(): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
//    }
#endif // VERBOSE
            Word arg_symbol=elt;
            arg_symbol=setSubtask(arg_symbol,parent_subtask);
            arg_symbol=setStatus(arg_symbol,DS_absent);
            //OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): no extra time to here? " << endl;
            symbol_table[data_address]=arg_symbol;
            //subtask_list.arguments(parent_subtask)[i]=data_address;
//             subtask_list.arguments(parent_subtask).push_back(data_address);
//            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): no extra time to here?? " << endl;
            subtask_list.add_arg(parent_subtask,data_address);
            i=i+1;

            if ( getQuoted(elt)==0 ) {
#ifdef VERBOSE
//    if (debug_all or service==debug_service){
                if (not ((getKind(elt)==K_R or getKind(elt)==K_C) or (getKind(elt) == K_D or getKind(elt) == K_L) )) {
                    std::cerr << "Should be obsolete: " << ppSymbol(elt) << "\n";
                    exit(0);
                }
//    }
#endif // VERBOSE
                Word var_label = elt;
                var_label = setName(var_label,service);
                var_label = setSubtask(var_label,data_address);
                arg_symbol=setStatus(arg_symbol,DS_requested);
                symbol_table[data_address]=arg_symbol;
                uint store=getName(elt);
                Packet_type_t packet_type=P_reference;

                if (getKind(elt)== K_L  or getKind(elt)== K_U ) {
                    store=S_LET;
                    packet_type=P_request;
                } else if (getKind(elt)== K_D) {
                    packet_type=P_request;
#ifdef VERBOSE
//    if (debug_all or service==debug_service){
                    cout << "parse_subtask(): Mode: K_D REQ: subtask " <<getSubtask(elt)<< ""<<endl;
                    cout << "parse_subtask(): Mode: K_D REQ (" <<getMode(elt)<< ") from reg " <<reg_addr<< ""<<endl;
//    }
#endif // VERBOSE
                }
                Length_t payload_length=1;
                Prio_t prio=0;
                Redir_t redir=0;
                Word ack_to=0;
                Header_t requestpacket_header = mkHeader(packet_type,prio,redir,payload_length,store,service,ack_to,var_label);
                Word_List requestpacket_payload;
                requestpacket_payload.push_back(elt);
                Packet_t request_packet = mkPacket(requestpacket_header,requestpacket_payload);
                if (store != service ) {
                    tx_fifo.push(request_packet);
                } else {
                    demux_packets_by_type(request_packet,data_fifo,subtask_code_fifo,request_fifo,subtask_reference_fifo);
                }
            }//if ( getQuoted(elt)==0)
            else if (getQuoted(elt)==1) {
                Word_List elt_val;
                elt_val.push_back(elt);
                if ((getKind(elt) == K_B or getKind(elt) == K_Q) and getExt(elt)==1) {
#ifdef VERBOSE
//                    if (debug_all or service==debug_service ){
                    cout << "Quoted Extended Builtin"<<endl;
//                    }
#endif // VERBOSE
                    for (int i=0;i<getSubtask(elt);i++) {
                        Word elt_sym=subtask.shift();
                        elt_val.push_back(elt_sym);
                    }//for
                }//if
                else {// do nothing
                }
//                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): store constant"<< endl;
                data_store.mput(data_address,elt_val);
//                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): update symbol table"<< endl;
                symbol_table[data_address]=setStatus(symbol_table[data_address],DS_present);
//                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): update subtask list"<< endl;
                subtask_list.decr_nargs_absent(parent_subtask);
                nargs_absent--;

//                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): done updating subtask list"<< endl;
            }//else if
#ifdef VERBOSE
//        if (debug_all or service==debug_service ){
            cout << "" <<service<< " parse_subtask(): subtask length is " <<subtask.size()<< ""<<endl;
//        }
#endif // VERBOSE
        }//while (subtask.size()>0)
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): processed subtask"<< endl;
        if ( subtask_list.status(parent_subtask)==STS_new) {
//            cout << service<< " parse_subtask(): subtask_list.status("<< parent_subtask <<")==STS_new\n";
//        	if ( subtask_list.nargs_absent(parent_subtask)==0) {
            if ( nargs_absent==0) {
//                cout << service<< " parse_subtask(): nargs_absent==0\n";
                subtask_list.status(parent_subtask,STS_pending);
#ifdef VERBOSE
//if (debug_all or service==debug_service){
                cout  <<service<< " parse_subtask() push SUBTASK " <<parent_subtask<< " onto pending_subtasks_fifo"<<endl;
//}
#endif // VERBOSE
                pending_subtasks_fifo.push(parent_subtask);
            }
        }
#ifdef VERBOSE
//    if (debug_all or service==debug_service ){
//         cout << "" <<service<< " parse_subtask(): " <<parent_subtask<< " status is <" <<subtask_list.status(parent_subtask)<< ">"<<endl;
//    }
#endif // VERBOSE
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" parse_subtask(): end parsing " <<parent_subtask<< endl;

    }//while(true)

}// funct: SC_parse_subtask :: do_proc()

} //namespace: SC_SBA


#endif /* SC_PARSE_SUBTASK_H_ */




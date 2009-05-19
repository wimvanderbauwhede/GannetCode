/*
********************************************************************************
                 |
  File Name      | SC_store_subtask_code.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'Sstore_subtask_code' class
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


#ifndef SC_STORE_SUBTASK_CODE_H_
#define SC_STORE_SUBTASK_CODE_H_


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
//	CLASS: STORE SUBTASK CODE
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This is the "store subtask code" module
/*!
   Detailed description here...
*/
template <typename ADDR_T, typename DATA_T>
class SC_store_subtask_code : public sc_module {
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_SubtaskList_if                  subtask_list;   //!< w/r port for subtask_list access
    port_SC_Fifo_if     <Packet_t>          subtask_code_fifo;//!< w/r port to access subtask_code_fifo
    port_SC_Fifo_if     <Word>              subtask_fifo;   //!< write port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, uint>      code_status;    //!< port for access to code_status (table)
    port_SC_Memory_if   <ADDR_T, DATA_T>    code_store;   //!< write/read port for code_store
    port_SC_reg_if      <SBA::Service >     service;

//    sc_port<SC_fifo_delayed_put_if<PACKET_T> >  subtask_code_fifo;  //!< write port to access subtask_code_fifo


    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    void activate_subtask_helper(CodeAddress task_address,Name_t service_id,Word_List& packet,bool is_subtask_packet);
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const {
        return "SC_store_subtask_code";
    }


    // ---------------------------- CONSTRUCTOR --------------------------------

    SC_HAS_PROCESS(SC_store_subtask_code);

    SC_store_subtask_code(sc_module_name nm) : sc_module(nm) {
        SC_THREAD(do_proc);
        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
}
;//class: SC_store_subtask_code

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_store_subtask_code <ADDR_T, DATA_T> :: do_proc() {
#ifdef VERBOSE
    //if (debug_all or service==debug_service ){
    cout << "\n" <<service<< " store_subtask_code()"<<endl;
    //}

#endif // VERBOSE
//        uint fifo_len = subtask_code_fifo.size();
//        for(uint i=1;i<=fifo_len ;i++) {
    while (true) {
        Packet_t  subtask_code_packet=subtask_code_fifo.shift();
#ifdef VERBOSE
//if (debug_all or service==debug_service ){
        cout << "" <<service<< " store_subtask_code(): packet:"<<endl;
        cout << ppPacket(subtask_code_packet)<<endl;
//}
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": enter "<<service<<" store_subtask_code()\n";
#endif // VERBOSE
        Word_List subtask_code=getPayload(subtask_code_packet);
#ifdef VERBOSE
//if (debug_all or service==debug_service    ){
        cout << "subtask code:"<<endl;
        cout << ppPayload(subtask_code)<<endl;
//}
#endif // VERBOSE

        Word code_label= subtask_code_packet[2];
        CodeAddress code_address=getCodeAddress(code_label);
        Name_t service_id=getName(code_label);
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" store_subtask_code(): storing code\n";
        // SC_Memory
        code_store.mput(code_address,subtask_code);
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" store_subtask_code(): stored code\n";
        // SC_Memory
        code_status[code_address]=code_status[code_address]|2;
#ifdef VERBOSE
        //if (debug_all or service==debug_service    ){
        cout << "" <<service<< " store_subtask_code(): " <<code_label<< " at " <<code_address<< ""<<endl;
        cout << ppPayload(code_store.mget(code_address))<<endl;
        //}
#endif // VERBOSE
        bool is_subtask_packet = (getPacket_type(getHeader(subtask_code_packet))==P_subtask);
        if (is_subtask_packet or code_status[code_address]&1==1) { // 00=Absent,01=ActReq, 10=Present
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": "<<service<<" store_subtask_code(): activate "<<code_address<<" now!\n";
#endif
            code_status[code_address]=code_status[code_address]&2;
            activate_subtask_helper(code_address,service_id,subtask_code_packet,is_subtask_packet);
        }//if
//#ifdef VERBOSE
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" store_subtask_code(): stored " <<code_label<< " at " <<code_address<< ""<<endl;
//#endif
    }//while(true)
#ifdef VERBOSE
    cout << "\n";
#endif // VERBOSE
}// funct: SC_store_subtask_code :: do_proc()


template <typename ADDR_T, typename DATA_T>
void SC_store_subtask_code <ADDR_T, DATA_T> ::activate_subtask_helper(CodeAddress task_address,Name_t service_id,Word_List& packet,bool is_subtask_packet) {
#ifdef VERBOSE
    if ((service!=service_id)) {
        cout << "" <<service<< " store_subtask_code(): service_id " <<service_id<< " <> service " <<service<< ""<<endl;
    }
#endif // VERBOSE

    /*
    #ifdef VERBOSE
            cout << "" <<service<< " SUBTASK STACK SIZE: " <<subtasks_address_stack.size()<< ""<<endl;
    #endif // VERBOSE
            if (subtasks_address_stack.size()==0){
                  std::cerr << "SUBTASK STACK OVERFLOW\n"; exit(0);
            }
            CodeAddress subtask_address=subtasks_address_stack.pop();
    */

    CodeAddress subtask_address=task_address;

#ifdef VERBOSE
//if (debug_all or service==debug_service ){
    cout << "" <<service<< " store_subtask_code(): address: " <<subtask_address<< ""<<endl;
//}
    if (is_subtask_packet) {
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" store_subtask_code(): update subtask list for "<<subtask_address<<endl;
    }
#endif // VERBOSE
    if (is_subtask_packet) {
    subtask_list.add(subtask_address);
    }
    /*
    #if VM==1
            Word         subtask_word=(subtask_address << 16) + task_address;
    #else // VM==0
    */
    Word subtask_word=(Word)task_address;
    /*
    #endif // VM
    #if VM==1
            subtask_fifo.push_back(subtask_word);
    #else // VM==0
    */
    if (code_status[task_address]>=2) {
        subtask_fifo.push(subtask_word);
        code_status[task_address]=2;
    } else {
#ifdef VERBOSE
        cout << service <<" store_subtask_code(): DEFER ACTIVATION "<<task_address<<":"<<code_status[task_address]<<endl;
#endif // VERBOSE
        code_status[task_address]=1;
    }
    /* #endif // VM
     */
    if (is_subtask_packet) {
    subtask_list.return_as(subtask_address,getReturn_as_p(packet));
    subtask_list.return_to(subtask_address,getReturn_to_p(packet));
    subtask_list.to(subtask_address,getReturn_to_p(packet));
#ifdef VERBOSE
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" store_subtask_code(): set TO:" << (int)getReturn_to_p(packet)<<" for "<< subtask_address << endl;
#endif // VERBOSE
    subtask_list.redir(subtask_address, getRedir_p(packet));
    subtask_list.ack_to(subtask_address, getAck_to_p(packet));
    subtask_list.code_address(subtask_address,task_address);
    subtask_list.service_id(subtask_address,service_id);
    }
    //service_id=service_id;

}

} //namespace: SC_SBA


#endif /* SC_STORE_SUBTASK_CODE_H_ */





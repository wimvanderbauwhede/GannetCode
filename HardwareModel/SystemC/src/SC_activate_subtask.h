/*
********************************************************************************
                 |
  File Name      | SC_activate_subtask.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'activate_subtask_code' class
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


#ifndef SC_ACTIVATE_SUBTASK_H_
#define SC_ACTIVATE_SUBTASK_H_



//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: ACTIVATE SUBTASK
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "SC_activate_subtask" module
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_activate_subtask : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    port_SC_SubtaskList_if              subtask_list;   //!< w/r port for subtask_list access
    port_SC_Fifo_if     <Packet_t>      subtask_reference_fifo;  //!< read port to access subtask_reference_fifo
    port_SC_Fifo_if     <Word>          subtask_fifo; //!< write port to access subtask_fifo
    port_SC_Memory_if   <ADDR_T, uint>  code_status;    //!< port for access to code_status (table)
    port_SC_Memory_if   <ADDR_T, DATA_T>code_store;   //!< write/read port for code_store
    port_SC_reg_if      <SBA::Service >     service;

    //sc_port<SC_fifo_delayed_get_if<PACKET_T> >  pr_ref_fifo_d;  //!< read port to access subtask_reference_fifo
    //sc_port<SC_fifo_delayed_put_if<PACKET_T> >  pw_staskfifo_d; //!< write port to access subtask_fifo

	// ---------------------------- METHODS ------------------------------------
    void do_proc(); // the thread
    void activate_subtask( CodeAddress task_address, Name_t service_id, Word_List& packet);

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_activate_subtask_code"; }
	// ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_activate_subtask);
	SC_activate_subtask(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
	}
private:
};//class: SC_activate_subtask






//==============================================================================
//  ACTIVATE_SUBTASK_HELPER()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_activate_subtask <ADDR_T, DATA_T>::activate_subtask( CodeAddress task_address,
                                                                    Name_t      service_id,
                                                                    Word_List& packet)
{

#ifdef VERBOSE
//        if ((service!=service_id)){
    cout << "" <<service<< " activate_subtask(): service_id " <<(int)service_id<< " <> service " <<service<< ""<<endl;
//        }
#endif // VERBOSE

    CodeAddress subtask_address=task_address;
#ifdef VERBOSE
//if (debug_all or service==debug_service){
    cout << "" <<service<< " activate_subtask(): address: " <<subtask_address<< ""<<endl;
//}
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" activate_subtask(): update subtask list for "<<subtask_address<<endl;
#endif // VERBOSE

    subtask_list.add(subtask_address);

    Word subtask_word=(Word)task_address;
    if (code_status[task_address]>=2 ) {
        subtask_fifo.push(subtask_word);
        code_status[task_address]=2;
    } else {
#ifdef VERBOSE
        cout << service << " activate_subtask(): DEFER ACTIVATION "<<task_address<<":"<<code_status[task_address] <<endl;
#endif
        code_status[task_address]=1;
    }

    subtask_list.return_as(subtask_address,getReturn_as_p(packet));
    subtask_list.return_to(subtask_address,getReturn_to_p(packet));
#ifdef VERBOSE
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" activate_subtask(): now setting TO:"<< (int)getReturn_to_p(packet) << " for "<<subtask_address<<endl;
#endif // VERBOSE

    subtask_list.to(subtask_address,getReturn_to_p(packet));
    subtask_list.redir(subtask_address, getRedir_p(packet));
    subtask_list.ack_to(subtask_address, getAck_to_p(packet));

    subtask_list.code_address(subtask_address,task_address);
    subtask_list.service_id(subtask_address,service_id);
#ifdef VERBOSE
OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" activate_subtask(): updated subtask list for "<<subtask_address<<endl;
#endif
    //service_id=service_id;

}//activate_subtask()

//==============================================================================
//	DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_activate_subtask <ADDR_T, DATA_T> :: do_proc()
{
#ifdef VERBOSE
//if (debug_all or service==debug_service){
    cout << "" <<service<< " activate_subtask()"<<endl;
//}
#endif // VERBOSE
    //while (subtask_reference_fifo.size()>0){
    while(true)
    {
        Packet_t subtask_ref_packet=subtask_reference_fifo.shift();
// next line costs a cycle!
//        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" activate_subtask(): shifted packet off subtask_reference_fifo\n";

//        cout << service << " activate_subtask(): shifted packet off subtask_reference_fifo\n";
        Word_List ref_label_symbol_l=getPayload(subtask_ref_packet);
        Word ref_label_symbol=ref_label_symbol_l[0];

#ifdef VERBOSE
            cout << ppSymbol(ref_label_symbol)<<endl;
#endif // VERBOSE
        CodeAddress code_address=getCodeAddress(ref_label_symbol);
        Name_t service_id=getName(ref_label_symbol);
        activate_subtask(code_address,service_id,subtask_ref_packet);
    }//while(true)
}// funct: SC_activate_subtask :: do_proc()

} //namespace: SC_SBA

#endif /* SC_ACTIVATE_SUBTASK_H_ */





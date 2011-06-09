/*
********************************************************************************
                 |
  File Name      | SC_store_result_packets_gw.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 26-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'store_result_packets' class for Gateway
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_STORE_RESULT_PACKETS_GW_H_
#define SC_STORE_RESULT_PACKETS_GW_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;


//==============================================================================
//  CLASS: STORE RESULT PACKETS(GATEWAY)
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! TThe SystemC 'store_result_packets' class for Gateway
    /*!
       Detailed description here...
    */
template <typename ADDR_T, typename DATA_T>
class SC_store_result_packets_gw : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Stack_if                        tasks_stack;    //!<
    port_SC_Fifo_if     <Packet_t>          rx_fifo;      //!<
    port_SC_reg_if      <SBA::Core_Status>  core_status;    //!<
    port_SC_reg_if		<uint	>       	io_mech;
    port_SC_reg_if      <SBA::Service >     service;

    //port_SC_Memory_if   <ADDR_T, DATA_T>    result_store;//!< read/write port for accessing result_store in the tile
    //port_SC_reg_if      <bool>              finished;       //!<

    // ---------------------------- METHODS ------------------------------------
    void do_proc();
    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_store_result_packets_gw"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_store_result_packets_gw);

    SC_store_result_packets_gw(sc_module_name nm) : sc_module(nm)
    {
        SC_THREAD(do_proc);
        //dont_initialize();

        // creation debug message..
        const_debug_msg(name(),kind());
    }
private:
	void vmif_send(Word_List& result,uint taskid);
};//class: SC_store_result_packets_gw

//==============================================================================
//  DO_PROC()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_store_result_packets_gw<ADDR_T, DATA_T>:: do_proc()
{
#include "../gensrc/Gateway/store_result_packets.cc"
	/*
//	 cout << "SC_store_result_packets_gw::do_proc()\n";
 //if (rx_fifo.size()>0 ){
//            while (rx_fifo.size()>0		){
			 while (true) {
                Packet_t data_packet=rx_fifo.shift();
#ifdef VERBOSE
//                cout <<"\nGateway: got result packet:\n--------\n"<<ppHeader(data_packet)<<"\n--------\n";
#endif // VERBOSE

                Word label= getReturn_as(getHeader(data_packet));
                Word_List result=getPayload(data_packet);
                uint task_id=getTask(label);
#ifdef VERBOSE
                cout << "\n" << service << ":" << "Storing value " <<ppPayload(result)<< " for " <<task_id<< " ...\n";
#endif // VERBOSE

#ifdef VERBOSE
                        cout << "Gateway: Interface send()"<<endl;
#endif // VERBOSE
                        vmif_send(result,task_id);
                        tasks_stack.push(task_id);
                        if (tasks_stack.size()==MAX_NTASKS){
                            core_status = CS_idle;
                            if (io_mech==(uint)1){
                            	//FIXME: refactor
                                cerr << "MOVE TO VMIF";
                                exit(1);
                                //finished=true; //<--- should this be included in SC model?
                            }
#ifdef VERBOSE
                            //cout << tx_fifo.size()<<endl;
#endif // VERBOSE
                        }
            }
//        }
*/

}// funct: SC_store_result_packets_gw :: do_proc()


//==============================================================================
//  vmif_send()
//==============================================================================
template <typename ADDR_T, typename DATA_T>
void SC_store_result_packets_gw <ADDR_T, DATA_T> :: vmif_send(Word_List& result,uint taskid) {
	OSTREAM << "DONE: "<< sc_time_stamp() << "\n";
    //OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
    //<<name()<<": DONE\n";
#ifndef NO_SOCKET
        if (io_mech==0 ){
            uint fd=iodescs[taskid]; // part of Interface obj
        } else {
#endif
                if (result.size()>1                    ){
                    Word_List result_payload=result;
#ifdef VERBOSE
                    cout << "RESULT (VMIF): ( ";
#endif // VERBOSE
                    if (result.size()==2){
                        deque<Int> int_result=to_signed_int_list(result_payload);
                        bool first=true;
//                    	const Int lo = -0x22FFFFFFL;
//                    	const Int hi = 0xDD000000UL;
                        for(deque<Int>::iterator iter_=int_result.begin();iter_!=int_result.end();iter_++) {
                        	Int elt=*iter_;
//                            if ((elt > lo and elt < hi) or not first){
                        	if (first) {
                        		cout << "0x"<<hex << elt << " ";
                        		first=false;
                        	} else {
                            	cout << dec << elt  << " ";
                        	}
//                            	first=false;
//                            }
                        }
                    } else {
                    	for(Word_List::iterator iter_=result.begin();iter_!=result.end();iter_++) {
                    	                        	Word elt=*iter_;
                    	                            cout << elt  << "\n";
                    	                        }

                    }
#ifdef VERBOSE
                    cout << ")";
#endif // VERBOSE
                    cout << "\n";
                } else {
                        deque<Int> int_result_list=to_signed_int_list(result);
                        Int int_result=int_result_list[0];
                        cout << "RESULT: " <<int_result<< ""<<endl;
                }


#ifndef NO_SOCKET
    }
#endif
} // vmif_send()

} //namespace: SC_SBA

#endif /* SC_STORE_RESULT_PACKETS_GW_H_ */

/*
********************************************************************************
                 |
  File Name      | SC_receive_packets.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 31-Oct-2008. DComputing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC 'receive packets' class
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

#ifndef SC_RECEIVE_PACKETS_H_
#define SC_RECEIVE_PACKETS_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"
//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//==============================================================================
//	CLASS: RECEIVE PACKETS
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! This is the "receive packets" module
    /*!
       Detailed description here...
    */
class SC_receive_packets : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_Fifo_if<Packet_t>   tranceiver_rx_fifo; //!< read port for tranceiver rx fifo

    port_SC_Fifo_if<Packet_t>   data_fifo;                 //!< write port
    port_SC_Fifo_if<Packet_t>   subtask_code_fifo;         //!< write port
    port_SC_Fifo_if<Packet_t>   request_fifo;              //!< write port
    port_SC_Fifo_if<Packet_t>   subtask_reference_fifo;    //!< write port

    port_SC_reg_if<SBA::Service > service;
	// ---------------------------- METHODS ------------------------------------
	void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_receive_packets"; }

	// ---------------------------- CONSTRUCTOR --------------------------------

	SC_HAS_PROCESS(SC_receive_packets);

	SC_receive_packets(sc_module_name nm) : sc_module(nm)
	{
		SC_THREAD(do_proc);
		// creation debug message..
        const_debug_msg(name(),kind());
	}
private:
	void demux_packets_by_type(SBA::Packet_t&);
};//class: SC_receive_packets

//==============================================================================
//	DO_PROC()
//==============================================================================
void SC_receive_packets :: do_proc()
    {
#ifdef VERBOSE
//if (debug_all or service==debug_service ){
	cout << "" <<service<< " receive_packets(): " << tranceiver_rx_fifo.size() << "\n";
	//cout << "" << " receive_packets(): " << tranceiver_rx_fifo.size() << "\n";
//}
#endif // VERBOSE
    	// receive forever
        while (true){
            Packet_t  rx_packet= tranceiver_rx_fifo.shift();
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" receive_packets(): received packet from TRX"<< endl;
            cout << "" <<service<< " receive_packets(): received packet from TRX\n";
#endif
//			demux_packets_by_type(rx_packet,data_fifo,subtask_code_fifo,request_fifo,subtask_reference_fifo);
			demux_packets_by_type(rx_packet);
        }
    }// funct: SC_receive_packets :: do_proc()

void SC_receive_packets :: demux_packets_by_type(Packet_t& packet) {
	Packet_type_t packet_type=getType(getHeader(packet));
#ifdef VERBOSE
	cout << "demux_packets_by_type(): Got packet of type "<<(int)packet_type<<"\n";
#endif
         switch (packet_type) {
         case P_data   :
         {
            data_fifo.push(packet);
          break;
         }
         case P_code :
         {
#ifdef VERBOSE
        	 cout << "Pushed packet onto subtask_code_fifo\n";
#endif
            subtask_code_fifo.push(packet);
          break;
         }
         case P_subtask :
         {
#ifdef VERBOSE
        	 cout << "Pushed packet onto subtask_code_fifo\n";
#endif
            subtask_code_fifo.push(packet);
          break;
         }
         case P_request :
         {
#ifdef VERBOSE
         	cout << "Pushed packet onto request_fifo\n";
#endif
         	request_fifo.push(packet);
          break;
         }
         case P_reference   :
         {
#ifdef VERBOSE
         	cout << "Pushed packet onto subtask_reference_fifo\n";
#endif
            subtask_reference_fifo.push(packet);
             break;}
           default:
            cerr << "Packet Type " << getType(getHeader(packet))<< " not recognised";
            exit(1);
        }
    } // func: demux_packets_by_type



} //namespace: SC_SBA
#endif /* SC_RECEIVE_PACKETS_H_ */

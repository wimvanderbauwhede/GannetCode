/*
 * SC_Helpers.h
 *
 *  Created on: 11-Dec-2008
 *      Author: StudentAdmin
 */

#ifndef SC_HELPERS_H_
#define SC_HELPERS_H_

//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : SC_concatenate
//* Object              : concatenate string and integer, to allow dynamic creation of names for array of sc_modules
//* Input Parameters    : std::string const& name: the string portion
//*                       int                i   : the integer portion
//*
//* Output Parameters   : const char *
//*---------------------------------------------------------------------------------------------------------


const char * SC_concatenate(std::string const& name, int i)
{
    std::stringstream s;    // stringstream object allows convenient concatenation of string and integer
    s << name << i;         // concatenation
    string str = s.str();   // convert stringstream object to string object (to allow conversion to char *)
    return str.c_str();     // using c_str() convert to char * (which is compatible with sc_module constructor argument (string object is not)
}

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : demux_packets_by_type
//* Object              : demux packet type and store in appropriate fifo
//* Input Parameters    : reference to packet
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------
using namespace SC_SBA;

void demux_packets_by_type(Packet_t& packet, port_SC_Fifo_if<Packet_t>& data_fifo, port_SC_Fifo_if<Packet_t>& subtask_code_fifo, port_SC_Fifo_if<Packet_t>& request_fifo, port_SC_Fifo_if<Packet_t>& subtask_reference_fifo) {
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


#endif /* SC_HELPERS_H_ */

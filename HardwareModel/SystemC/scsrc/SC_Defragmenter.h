/*
********************************************************************************
                 |
  File Name      | SC_Defragmenter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 25-Oct-2010. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC SC_Defragmenter class: Gannet packet fragmentation 
  				 | into fixed-size packets for the Quarc NoC
-----------------|--------------------------------------------------------------
  Modifications  | WV20101025: Created
                 | 
********************************************************************************

  (c) 2008-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/

#ifndef  SC_DEFRAGMENTER_H_
#define  SC_DEFRAGMENTER_H_

#include "SC_SBA.h"

using namespace std;

//==============================================================================
//  CLASS: Defragmenter
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

/*
This module sits between the Service Manager and the rx_fifo of the Transceiver
So instead of

    // bind service_mngr port to rx_fifo write export of transceiver
    service_manager.transceiver_rx_fifo.bind   (transceiver.xpwr_rxfifo);

we get

    // bind service_mngr port to rx_fifo write export of defragmenter
    service_manager.transceiver_rx_fifo.bind   (defragmenter.xpwr_rxfifo);
    // bind defragmenter port to rx_fifo write export of transceiver
    defragmenter.transceiver_rx_fifo.bind   (transceiver.xpwr_rxfifo);

so the defragmenter must have an xpwr_rxfifo and a transceiver_rx_fifo
*/

class Ctrl {
public:
	int length;
	int offset;
	int frag;
	Ctrl(int l_,int o_) : length(l_),offset(o_),frag(0) {};
	Ctrl() : length(0),offset(0),frag(0) {};
};
	
class SC_Defragmenter : public sc_module {
    public:
    port_SC_Fifo_if<Packet_t> transceiver_rx_fifo;
    port_SC_Config_if cfg;
    sc_export< SC_Fifo_if<Packet_t> > xpwr_rxfifo;
	SBA::Service service;
	// overload sc_module.kind() to return a more appropriate class name 
	virtual const char* kind() const { return "SC_Defragmenter"; }                                                                                                                                               
    // ---------------------------- CONSTRUCTOR --------------------------------                                                                                                        
    SC_HAS_PROCESS(SC_Defragmenter);
    SC_Defragmenter(sc_module_name nm,SBA::Service s_) :
        sc_module(nm),
        service(s_),
        rx_fifo("rx_fifo")
    {
        xpwr_rxfifo    (rx_fifo.xpwr_1);
        SC_THREAD(defragment);
        // creation debug message...
		const_debug_msg(name(),kind());
		//filling mem (for debug)
		for (unsigned int i=0;i<NSERVICES;i++) {
			Ctrl tctl;
			buffer_ctl[i]=tctl;
			for (int j=0;j<MAX_PACKET_SZ;j++) {
				packet_buffer[i][j]=j*1000+i;
			}
		}
	}
	
    private:
    SC_Fifo<Packet_t,1> rx_fifo;  
	Word packet_buffer[NSERVICES+1][MAX_PACKET_SZ];
	Ctrl buffer_ctl[NSERVICES+1];
    void defragment();
	SBA::ServiceAddress getSrc(Word w);
	SBA::ServiceAddress getDst(Word w);
};/* class: SC_Defragmenter */

//==============================================================================
//  Methods
//==============================================================================

SBA::ServiceAddress SC_Defragmenter::getDst(Word qph) {
	return qph&0xffff;
}

SBA::ServiceAddress SC_Defragmenter::getSrc(Word qph) {
	return (qph>>16) &0xffff;
}

void SC_Defragmenter::defragment() {
	bool defragmentation=QUARC_FRAG?true:false;
	while(true) {
		Packet_t rx_packet =transceiver_rx_fifo.shift();
		Word qph = rx_packet.shift(); // strip Quarc header
		SBA::ServiceAddress src_addr=getSrc(qph);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Defragmenter: got packet"<< endl;
#endif

		if ( (getLength(rx_packet)<=1 && getPacket_type(rx_packet)!=P_fragment ) || defragmentation==false) {
			// "single-flit" packets 
			rx_fifo.push(rx_packet);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Defragmenter: pushed unfragmented packet to RX fifo"<< endl;
#endif

		} else {
//			To_t to=getTo(rx_packet);
//			Return_to_t from= getReturn_to(rx_packet);
			Packet_type_t packet_type=getPacket_type(rx_packet);

			if (packet_type!=P_fragment) { // it's the "header flit"
				Length_t plength=getLength(rx_packet);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Defragmenter: stored head fragment. Expecting "<< plength-1 << " Words or "<< (plength-1)/3+1<<" fragments"<< endl;
#endif

				for (int i=0;i<FIXED_PACKET_SIZE;i++) {
					packet_buffer[src_addr][i]=rx_packet[i];
				}
				Ctrl ctl(plength-1,FIXED_PACKET_SIZE);
				buffer_ctl[src_addr]=ctl;

			} else { // it's a fragment
				int poffset=buffer_ctl[src_addr].offset;
				int plength=buffer_ctl[src_addr].length;
				int fragc=buffer_ctl[src_addr].frag;
				fragc++;
				for (int i=1;i<FIXED_PACKET_SIZE;i++) {
					packet_buffer[src_addr][i+poffset-1]=rx_packet[i];
					plength--;
				}
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Defragmenter: stored body fragment "<< fragc<< " at "<<poffset<<", remainder:"<<plength<< endl;
#endif
				if (plength<=0) { // can be 0, -1, -2
					Packet_t rx_packet_defrag;
					for (int i=0;i<poffset+FIXED_PACKET_SIZE-1+plength;i++) {
//#ifdef SC_VERBOSE
//            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<": "<<packet_buffer[src_addr][i]<< endl;
//#endif
            			rx_packet_defrag.push(packet_buffer[src_addr][i]);
					}
            		rx_fifo.push(rx_packet_defrag);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Defragmenter: pushed defragmented packed onto RX fifo"<< endl;
#endif

				} else {
					Ctrl ctl(plength,poffset+FIXED_PACKET_SIZE-1);
					ctl.frag=fragc;
					buffer_ctl[src_addr]=ctl;
				}
			}
		}
	}
}	
} // namespace SC_SBA
#endif // SC_DEFRAGMENTER_H_


	/*
			The sophisticated way is to store the packets
			in their final destination and complete them as they come in.
			To make it completely fool-proof we'd need to have the Return_as
			in every packet. But I assume that the fragments are never interleaved,
			so what we need is a lookup Return_to->Return_as. Hmm.
			Assuming we never need large amounts of these (worst case is NSERVICES+1)
			I guess we can simply use a list.
			lookup.push((return_as<<8)+return_to);

			for (int i=0;i<NSERVICES;i++) {
				long w=lookup[i];
				if (w&8==return_to) {
					return_as=w>>8;
					break;
				}
			}

			Somewhere we'll have to store the current offset and the total or remaining length, as extra fields in the record for all code and data packets
			The whole thing is rather invasive.
			*/
			/*
			The next best thing is to store the packets in a RAM, as a map; to keep it fast & simple,
			we allocate a fixed space (i.e. the MAX packet size); we have 2 RAMs, one for data, one for code.
			And we have 2 register files (small RAMs) that keep the offset and the length. Make that 2x16bits.
			This is of course a lot slower as we double-buffer.

			It's worth keeping in mind that for a SW solution we would simply have long packets and deep buffers.

			*/


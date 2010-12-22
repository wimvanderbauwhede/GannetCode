/*
********************************************************************************
                 |
  File Name      | SC_Fragmenter.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 25-Oct-2010. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC SC_Fragmenter class: Gannet packet fragmentation 
  				 | into fixed-size packets for the Quarc NoC
-----------------|--------------------------------------------------------------
  Modifications  | WV20101025: Created
                 | 
********************************************************************************

  (c) 2008-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

*/

#ifndef  SC_FRAGMENTER_H_
#define  SC_FRAGMENTER_H_

#include "SC_SBA.h"

using namespace std;

//==============================================================================
//  CLASS: Fragmenter
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

/*
This module sits between the Service Manager and the tx_fifo
So instead of

    // bind service_mngr port to tx_fifo write export of transceiver
    service_manager.transceiver_tx_fifo.bind   (transceiver.xpwr_txfifo);

we get

    // bind service_mngr port to tx_fifo write export of fragmenter
    service_manager.transceiver_tx_fifo.bind   (fragmenter.xpwr_txfifo);
    // bind fragmenter port to tx_fifo write export of transceiver
    fragmenter.transceiver_tx_fifo.bind   (transceiver.xpwr_txfifo);

so the fragmenter must have an xpwr_txfifo and a transceiver_tx_fifo
*/

class SC_Fragmenter : public sc_module {
    public:
    port_SC_Fifo_if<Packet_t> transceiver_tx_fifo;
    port_SC_Config_if cfg;
    sc_export< SC_Fifo_if<Packet_t> > xpwr_txfifo;
	SBA::Service service;
	// overload sc_module.kind() to return a more appropriate class name 
	virtual const char* kind() const { return "SC_Fragmenter"; }                                                                                                                                               
    // ---------------------------- CONSTRUCTOR --------------------------------                                                                                                        
    SC_HAS_PROCESS(SC_Fragmenter);
    SC_Fragmenter(sc_module_name nm,SBA::Service s_) :
        sc_module(nm),
        service(s_),
        tx_fifo("tx_fifo")
    {
        xpwr_txfifo    (tx_fifo.xpwr_1);
        SC_THREAD(fragment);
        // creation debug message...
		const_debug_msg(name(),kind());                                                                               
	}
	
    private:
    SC_Fifo<Packet_t,1> tx_fifo;  
    Word_List create_quarc_packet(SBA::ServiceAddress src_addr,SBA::ServiceAddress dst_addr, Packet_t& p);
    void fragment();

};/* class: SC_Fragmenter */

//==============================================================================
//  Methods
//==============================================================================

Word_List SC_Fragmenter::create_quarc_packet(SBA::ServiceAddress src_addr,SBA::ServiceAddress dst_addr, Packet_t& p) {
	Word_List qp;
	// Header: Src|Dst
	Word qhw= (src_addr<<16)+dst_addr;
	qp.push(qhw);
	for (Word_List::iterator i_=p.begin();i_!=p.end();i_++){
		qp.push(*i_);
	}
	return qp;
}

// TODO: this is specific for 4-Word packets.
//It would be better to have FIXED_PACKET_SZ

// We change the system: Quarc packets have a header consisting of From and To, 8 bytes each, the assumption is that we have FIXED_PACKET_SZx36bits (but we use 5x32 for convenience)
void SC_Fragmenter::fragment() {
	bool fragmentation=QUARC_FRAG?true:false;
	while(true) {
		Packet_t p=tx_fifo.shift(); // the packet to fragment
		Header_t h = getHeader(p);
//		Return_to_t return_to=getReturn_to(h);
		To_t dst_service_id=getTo(h);
		SBA::ServiceAddress dst_addr        = cfg.services(dst_service_id).address;
		SBA::ServiceAddress src_addr        = cfg.services(service).address;

//		if ((int)return_to!=(int)service) {
//#ifdef SC_VERBOSE
//            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: Return_to != From:"<<(int)return_to<<"<>"<<service<<endl;
//#endif
//            exit(1);
//		}
		Length_t gplength=getLength(h);
		Packet_t pf;
		// assertions du pauvre
		if (gplength!=p.size()-3) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: Length field incorrect! "<< (int)gplength <<"!="<<p.size()-3<<endl;
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: Packet_type "<< (int)(getPacket_type(h))<<endl;
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: To "<< (int)(getTo(h))<<endl;
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: Return_to "<< (int)(getReturn_to(h))<<endl;
#endif
			exit(1);
		}

		if (gplength>1 && fragmentation==true) { // 3 Words for the Header
			Payload_t pl=getPayload(p);

		    Word w1 = pl.shift();
		    gplength--;

		    Word_List wl; wl.push(w1);
		    Packet_t p1=mkPacket(h,wl);
//		    pf.push(h[0]);
//		    pf.push(h[1]);
//		    pf.push(h[2]);
//		    pf.push(w1);
			Word_List qp=create_quarc_packet(src_addr,dst_addr,p1);
		    transceiver_tx_fifo.push(qp);

#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: pushed head fragment to TX fifo"<< endl;
#endif

		    Header_t hf=setPacket_type(h,P_fragment);
		    hf=setLength(hf,1);
		    int pad = (FIXED_PACKET_SZ-1) - (gplength % (FIXED_PACKET_SZ-1));
		    Word wz=0;
		    for (int i=0;i<pad;i++) {
		    	pl.push(wz);
		    	gplength+=1;
		    }
//		    if (pad==2 ) {
//		    	pl.push(wz);
//		    	pl.push(wz);
//		    	gplength+=2;
//		    }
//		    if (pad==1) {
//		    	pl.push(wz);
//		    	gplength+=1;
//		    }
		    if(pad>(FIXED_PACKET_SZ-2)) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: padding incorrect! "<< pad <<endl;
#endif
			exit(1);
		    }

		    if ( gplength % (FIXED_PACKET_SZ-1) !=0) {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: Length incorrect! "<< gplength <<";"<<gplength % (FIXED_PACKET_SZ-1) <<endl;
#endif
			exit(1);

		    } else {
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: Payload length is now "<< gplength <<"; #frags:"<<gplength / (FIXED_PACKET_SZ-1) <<endl;
#endif

		    }
		    while (gplength!=0) {
		    	gplength-=(FIXED_PACKET_SZ-1);
		        Word w1 = pl.shift();
		        Word w2 = pl.shift();
		        hf[1]=w1;
		        hf[2]=w2;
		        Word_List wl;
		        for (int i=0;i<(FIXED_PACKET_SZ-3);i++) {
		        	Word w3 = pl.shift();
		        	wl.push(w3);
		        }
//		        pf.push(w1);pf.push(w2);pf.push(w3);
		        Packet_t p=mkPacket(hf,wl);
//		        transceiver_tx_fifo.push(p);
				Word_List qp=create_quarc_packet(src_addr,dst_addr,p);
			    transceiver_tx_fifo.push(qp);

#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: pushed body fragment "<<gplength/3<<" to TX fifo"<< endl;
#endif
		    }

#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: transmitted packet to TX fifo"<< endl;
#endif

//#ifdef SC_VERBOSE
//		    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<": orig length:"<<p.size()<<";reconstructed: "<< pf.size()<<endl;
//		    for (int i=0;i<pf.size();i++) {
//		    	if (i<p.size() ) {
//	            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<": "<< (pf[i]==p[i]) <<"\t"<< pf[i]<<"<>"<<p[i] << endl;
//		    	} else {
//		    		OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<": extra words:"<< pf[i]<< endl;
//		    	}
//		    }
//#endif
/*
			if (pl.size()==2) {
				Word w1 = pl.shift();
				Word w2 = pl.shift();
				Word w3 = 0;
				hf[1]=w1;
				hf[2]=w2;
				Word_List wl; wl.push(w3);
				Packet_t p=mkPacket(hf,wl);
				transceiver_tx_fifo.push(p);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: pushed padded (1) body fragment to TX fifo"<< endl;
#endif
			} else if (pl.size() == 1) {
				Word w1 = pl.shift();
				Word w2 = 0;
				Word w3 = 0;
				hf[1]=w1;
				hf[2]=w2;
				Word_List wl; wl.push(w3);
				Packet_t p=mkPacket(hf,wl);
				transceiver_tx_fifo.push(p);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: pushed padded (2) body fragment to TX fifo"<< endl;
#endif

			}
*/
		} else { // no need for fragmentation
			Word_List qp=create_quarc_packet(src_addr,dst_addr,p);
			transceiver_tx_fifo.push(qp);
#ifdef SC_VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" SC_Fragmenter: pushed unfragmented packet to TX fifo"<< endl;
#endif
		}
	}
}	
} // namespace SC_SBA
#endif // SC_FRAGMENTER_H_

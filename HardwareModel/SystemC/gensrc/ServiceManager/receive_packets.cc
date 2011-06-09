#ifdef VERBOSE
        if (debug_all or service==debug_service){
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " receive_packets(): " <<transceiver_rx_fifo.size()<< ";" <<endl; 
        }
#endif // VERBOSE
        while (true) {
            Packet_t rx_packet= transceiver_rx_fifo.shift();
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " receive_packets(): received packet from TRX" <<endl; 
#endif // VERBOSE
#ifdef SC_VERBOSE                        
          int to=(rx_packet.front()>>8)&0xFF;
          int return_to=rx_packet.front()&0xFF;
          int ctrl=(rx_packet.front()>>26)&0x7;
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") To:"<<to<<"; Return-to:"<<return_to<<";Ctrl:"<<ctrl<<endl;
#endif                        
            
            demux_packets_by_type(rx_packet);
        }

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
            demux_packets_by_type(rx_packet);
        } 

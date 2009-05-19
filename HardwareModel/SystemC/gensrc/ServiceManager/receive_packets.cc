#ifdef VERBOSE
        if (debug_all or service==debug_service){
            
        }
#endif // VERBOSE
        while (true) {
            Packet_t rx_packet= transceiver_rx_fifo.shift();
#ifdef VERBOSE
            
#endif // VERBOSE
            demux_packets_by_type(rx_packet);
        } 

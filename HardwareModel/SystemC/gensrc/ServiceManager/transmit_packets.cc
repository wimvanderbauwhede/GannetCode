        cerr << "OBSOLETE: write directly to TRX fifo";
        exit(1);

#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " transmit_packets()"<<endl;
        }
#endif // VERBOSE
        while (true) {
            Packet_t packet=tx_fifo.shift();
            tx_fifo.push(packet);
        }


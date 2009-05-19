       
#ifdef VERBOSE
        if (tx_fifo.size()>0 ){
            cout << "\n" << "Gateway:" << "Sending: \n";
            cout << "\nGateway: TX Fifo contains " <<tx_fifo.size()<< " packets"<<endl;
        }
#endif // VERBOSE
            while (true) {
                Packet_t packet = tx_fifo.shift();
#ifdef VERBOSE
                 cout << " packet <"<<(int) getType(getHeader(packet)) <<"> <"<<getReturn_as(getHeader(packet))<<"> to TX for "<< (int)getTo(getHeader(packet))<<"\n"; 
#endif // VERBOSE
                transceiver.tx_fifo.push(packet);
                transceiver.run();
            }
#ifdef VERBOSE
#endif // VERBOSE
        

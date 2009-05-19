#ifdef VERBOSE
         uint print_ok=0; 
        if (transceiver.rx_fifo.status==1 ){
        
            print_ok=1;
            cout << "\nGateway: " << service << " (" <<address<< "):\n";
            cout << "Gateway: TYPE: ";
        }
#endif // VERBOSE
        
        while (transceiver.rx_fifo.status==1            ){
            Packet_t  rx_packet= transceiver.rx_fifo.shift();
#ifdef VERBOSE
             cout << (int)getType(getHeader(rx_packet)) << " ";
#endif // VERBOSE
            
            if ( getType(getHeader(rx_packet)) == P_data){
                rx_fifo.push(rx_packet);
#if DATA==1                
            } else if ( getType(getHeader(rx_packet)) == P_request){
                request_fifo.push(rx_packet);
#endif        
            } else {  
                
#ifdef VERBOSE
                cout << ppPacket(rx_packet)<<endl;
#endif // VERBOSE
                 cerr << "Gateway can only receive data/result" <<  getType(getHeader(rx_packet)) << endl;//.to_uint 
                 exit(0);
            } 
            
        } 
        
        

        Packet_type_t packet_type=getType(getHeader(packet));
#ifdef VERBOSE
if (debug_all or service==debug_service){
        cout << service <<" demux_packets_by_type(): Got packet of type "<<(int)packet_type<<"\n";  
}           
#endif // VERBOSE
         switch (packet_type) {
         case P_data :
         {
            data_fifo.push(packet);
          break;
         }
         case P_code :
         {
            subtask_code_fifo.push(packet);
          break;
         }
         case P_subtask :
         {
            subtask_code_fifo.push(packet);
          break;
         }
         case P_request :
         {
            request_fifo.push(packet);
          break;
         }
         case P_reference :
         {
            subtask_reference_fifo.push(packet);
             break;}
           default:
            cerr << "Packet Type " <<packet_type<< " not recognised";
            exit(1);
        }

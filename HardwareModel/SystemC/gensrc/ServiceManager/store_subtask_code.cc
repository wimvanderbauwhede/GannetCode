#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "\n" <<service<< " store_subtask_code()"<<endl;
        }
#endif // VERBOSE

        uint fifo_len = subtask_code_fifo.size();
        for(uint i=1;i<=fifo_len ;i++) {
            Packet_t subtask_code_packet=subtask_code_fifo.shift();
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_subtask_code(): packet:"<<endl;
                cout << ppPacket(subtask_code_packet)<<endl;
            }
#endif // VERBOSE

            Word_List subtask_code=getPayload(subtask_code_packet);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "subtask code:"<<endl;
                cout << ppPayload(subtask_code)<<endl;
            }
#endif // VERBOSE
             Word code_label= subtask_code_packet[2];
            CodeAddress code_address=getCodeAddress(code_label);
            Name_t tservice_id=getName(code_label);
            code_status[code_address]=(code_status[code_address]==1)?1:0;
            code_store.mput(code_address,subtask_code);
            code_status[code_address]=code_status[code_address]|2;

            bool is_subtask_packet = (getPacket_type(getHeader(subtask_code_packet))==P_subtask);
            if (is_subtask_packet or code_status[code_address]&1==1){
#ifdef VERBOSE
                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " store_subtask_code(): activate " <<code_address<< " now!" <<endl; 
#endif // VERBOSE
                code_status[code_address]=code_status[code_address]&2;
                activate_subtask_helper(code_address,tservice_id,subtask_code_packet,is_subtask_packet);
            }
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " store_subtask_code(): stored " <<code_label<< " at " <<code_address<< "" <<endl; 
            }
#endif // VERBOSE
        }
#ifdef VERBOSE
        cout << "\n";
#endif // VERBOSE

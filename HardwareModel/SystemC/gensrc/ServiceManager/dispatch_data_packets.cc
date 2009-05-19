#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " dispatch_data_packets()"<<endl;
        }
#endif // VERBOSE
        Packet_Fifo pending_requests_fifo;
        while (true) {
            Packet_t request=request_fifo.shift();
            Word packet_label= getReturn_as(getHeader(request));
            Word var_label=getPayload(request)[0];
            uint offset=0;
            uint fsize=0;
            if (getExt(var_label)==1){
                Word var_label_ext=getPayload(request)[1];
                offset=getOffset(var_label_ext);
                fsize=getSize(var_label_ext);
            }
            
            MemAddress data_address=0;

            bool has_label=0;
            DS_t data_status=DS_absent;
             uint mode;
            if (getKind(var_label)==K_D){
                uint reg_address=getReg(var_label);
                mode=getMode(var_label);
                data_address=reg_address;
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "dispatch_data_packets(): Mode: CACHE: reg address: " <<reg_address<< " mode " <<mode<< ""<<endl;
                }
#endif // VERBOSE
                if (getStatus(symbol_table[reg_address])==DS_present){
                    has_label=1;
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "dispatch_data_packets(): Mode: CACHE: data address " <<data_address<< "";
                         cout << " status "<<(int)data_status << endl;                        
                    }
#endif // VERBOSE
                } else {
                    request_table.push(reg_address,packet_label);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "dispatch_data_packets(): Mode: CACHE: queue request for reg " <<reg_address<< " (" <<register_set[reg_address].data_address<< ")"<<endl;
                    }
#endif // VERBOSE
                }
                data_status=getStatus(symbol_table[reg_address]);
                if (mode==M_stream                ){
                    if (register_set[reg_address].status==RDS_present){
                        restart_subtask(reg_address);
                        register_set[reg_address].status=RDS_absent;
                    }
                }

            } else if (getKind(var_label)==K_L){
                if (lookup_table.count(getName(var_label))==1){
                    has_label=1;
                    Word word=lookup_table.read(getName(var_label));
                    data_address= getSubtask(word);
                     data_status=(Data_Status)getStatus(word);
                }
            } else {
                cerr << "WHY " <<getKind(var_label)<< "?";
                exit(1);
                data_address=getSubtask(var_label);
            }
            
            cout << "dispatch_data_packet(): has_Label=" <<has_label<< ""<<endl;
             cout << "dispatch_data_packet(): data_status="<<(int)data_status<<"\n";
            if (has_label==1 ){
                Length_t payload_length=0;
                if (getReturn_to(getHeader(request))!=getName(packet_label)){
                }
                 Word_List tdata;
                tdata=getField(data_store.mget(data_address),offset,fsize);
                payload_length=tdata.size();
                Header_t packet_header = mkHeader(P_data,0,0,payload_length, getReturn_to(getHeader(request)), NA,0,packet_label);
                Word_List packet_payload=tdata;
                Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
                cout << ppPacket(packet)<<endl;
#endif // VERBOSE
                if (getTo(packet_header) != service){
                    tx_fifo.push(packet);
                } else {
                    data_fifo.push(packet);
                }

            } else {
#if SYSC_FIXME==0                
                if (getKind(var_label)==K_L){
                    pending_requests_fifo.push(request);
                }
#endif // SYSC_FIXME                
            } // of request there or not
        } // of while
#if SYSC_FIXME==0
        while (true) {
            Packet_t pending_request =  pending_requests_fifo.shift();
            request_fifo.unshift(pending_request);
        }
#endif // SYSC_FIXME       

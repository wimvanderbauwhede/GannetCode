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
            uint data_status=DS_absent;
             uint mode;
            uint eosreq=2;
            uint eos=0;
            uint skip=0;
            if (getKind(var_label)==K_D){
                uint reg_address=getReg(var_label);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: symbol: " <<var_label<< " reg address: " <<reg_address<< ""<<endl;
                }
#endif // VERBOSE
                data_address=reg_address;
                mode=getMode(var_label);
                Word reg_symbol=symbol_table[reg_address];
                data_status=getStatus(reg_symbol);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: reg address: " <<reg_address<< " mode " <<mode<< " status " <<data_status<< ""<<endl;
#ifdef SC_VERBOSE                    
                      OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " STREAM request\n";
#endif                     
                }
#endif // VERBOSE
                if (mode==M_eos){
#ifdef VERBOSE
                    uint ds_eos=DS_eos;
                    cout << "" <<service<< " dispatch_data_packets(): EOS REQUEST " <<reg_address<< ": " <<data_status<< "<>" <<ds_eos<< " " <<(data_status==DS_eos)<< ""<<endl;
#endif // VERBOSE
                    has_label=1;
                    if (data_status==DS_eos                      ){
                      eosreq=1;
                    } else {
                      eosreq=0;
                    }
#ifdef SC_VERBOSE                     
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " EOS request for "<<reg_address<<":"<<(int)data_status<<"=>"<<eos<<"\n";
#endif                    
                } else {

                    if (data_status==DS_present){
                        has_label=1;
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: reg address " <<reg_address<< " status DS_present"<<endl;
                        }
#endif // VERBOSE

                        if (mode==M_stream       ){
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " dispatch_data_packets(): Mode: STREAM: " <<reg_address<< " (" <<register_set[reg_address].data_address<< ")"<<endl;
                            }
#endif // VERBOSE
                                symbol_table[reg_address]=setStatus(reg_symbol,DS_requested);
                            
#ifdef SC_VERBOSE                             
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " EOS: set status for "<<reg_address<<" to DS_requested =>"<<eos<<"\n";
#endif                            
                                restart_subtask(reg_address);
                            }
                    } else if (data_status==DS_eos){
                        eos=1;
                        has_label=1;
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: reg address " <<reg_address<< " status DS_eos"<<endl;
                        }
#endif // VERBOSE
                    } else if (data_status==DS_requested or data_status==DS_absent){
                        if (mode==M_stream){
                            skip=1;
                            has_label=1;
                        } else {
                            request_table.push(reg_address,packet_label);
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: queue request for reg address " <<reg_address<< " (" <<register_set[reg_address].data_address<< ")"<<endl;
                            }
#endif // VERBOSE
                        }    
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
            
            if (has_label==1 ){

                Length_t payload_length=0;
                if (getReturn_to(getHeader(request))!=getName(packet_label)){
                }
                 Word_List tdata;
#ifdef VERBOSE
                cout << "" <<service<< " dispatch_data_packet(): data_address=" <<data_address<< ", eosreq=" <<eosreq<< ""<<endl;
#endif // VERBOSE
                uint ctrl=0;
                if (eosreq==2 and eos==0 and skip==0 ){
                    tdata=getField(data_store.mget(data_address),offset,fsize);
                } else if (eosreq!=2                 ){
                    Bool_t tdatasym = mkBool(eosreq);
#ifdef VERBOSE
                    cout << "" <<service<< " dispatch_data_packets(): EOS REQUEST: " <<ppSymbol(tdatasym)<< " (" <<eosreq<< ")"<<endl;
#endif // VERBOSE
                    tdata.push(tdatasym);
                } else if (eos==1){
#ifdef VERBOSE
                    cout << "" <<service<< " dispatch_data_packets(): data status for " <<data_address<< " is EOS"<<endl;
#endif // VERBOSE
                    ctrl=2;
                } else if (skip==1){
#ifdef VERBOSE
                    cout << "" <<service<< " dispatch_data_packets(): data status for " <<data_address<< " is SKIP"<<endl;
#endif // VERBOSE
                    ctrl=6;
                }
                payload_length=tdata.size();
                
                Header_t packet_header = mkHeader(P_data,ctrl,0,payload_length, getReturn_to(getHeader(request)), NA,0,packet_label);
                Word_List packet_payload=tdata;
                Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
#endif // VERBOSE

#ifdef SC_VERBOSE                
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from dispatch_data_packet()\n";
#endif                
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

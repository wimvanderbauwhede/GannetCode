#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " store_data()"<<endl;
        }
#endif // VERBOSE
        while (true) {
            Packet_t data_packet=data_fifo.shift();
            uint ctrl=getCtrl_p(data_packet);
#ifdef VERBOSE
               
                   cout <<  "" <<service<< " store_data(): Ctrl=" <<ctrl<< ""<<endl;
              
#endif // VERBOSE
#ifdef VERBOSE
                if ((ctrl&1)==1){
                   cout <<  "" <<service<< " store_data(): got ACK"<<endl;
                }
#endif // VERBOSE

            Word label= getReturn_as(getHeader(data_packet));

            MemAddress data_address=getSubtask(label) & F_DataAddress;

#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() ADDRESS: " <<data_address<< ""<<endl;
            }
#endif // VERBOSE
            
            Word data_symbol=symbol_table[data_address];
            uint data_status=getStatus(data_symbol);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() STATUS: ";
                cout << data_status<<endl;
            }
#endif // VERBOSE
            
            if (data_status!=DS_present and data_status!=DS_eos){
                
                
                    
                    Payload_t data_packet_payload=getPayload(data_packet);
                    
                    uint fsize=0;
                    if ((data_address<DATA_OF+NREGS)){
                        fsize=register_set[data_address].fsize;
                    }
                    if (fsize==0){
                        data_store.mput(data_address,data_packet_payload);
                    } else {
                        uint offset=register_set[data_address].offset;
                        Word_List current_content = data_store.mget(data_address);
                        for(uint i=0 ;i<= fsize-1 ;i++) {
                            
                            #ifdef STATIC_ALLOC
                             current_content.at(i+offset,data_packet_payload[i]);
                            #else
                            current_content[i+offset]=data_packet_payload[i];
                            #endif
                        }                    
                        data_store.mput(data_address,current_content);
                    }
                    
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() address " <<data_address<< " STATUS: " <<data_status<< ""<<endl;
                    }
#endif // VERBOSE

                Subtask subtask=getSubtask(data_symbol);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() SUBTASK: " <<subtask<< ""<<endl;
                    }
#endif // VERBOSE
                
                

                if ((data_address<DATA_OF+NREGS)){
                    register_set[data_address].status=RDS_present;
                }             
                       
                uint skip=0;
                uint eos=0;
                uint eosctrl=ctrl&6;
                if (eosctrl==6 ){
                    skip=1;
                } else if (eosctrl==2){
                    eos=1;
                    symbol_table[data_address]=setStatus(data_symbol,DS_eos);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() EOS on " <<data_address<< ""<<endl;
                    }
#endif // VERBOSE
                } else {
                    symbol_table[data_address]=setStatus(data_symbol,DS_present);
                }
                
                            subtask_list.lock();                      
                if (subtask_list.status(subtask)!=STS_deleted                    ){
                    subtask_list.decr_nargs_absent(subtask);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " in list"<<endl;
                    }
#endif // VERBOSE
#ifdef VERBOSE
                    cout << "" <<service<< " store_data() nargs_absent=" <<subtask_list.nargs_absent(subtask)<< " eos=" <<eos<< " skip=" <<skip<< ""<<endl;
#endif // VERBOSE
                    if (subtask_list.nargs_absent(subtask)==0 or eos==1 or skip==1){
                        if (eos==0 and skip==0){
                            subtask_list.status(subtask,STS_pending);
                        } else if (eos==1){
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " store_data() EOS: setting subtask status from " <<subtask_list.status(subtask)<< " to STS_eos"<<endl;
                            }
#endif // VERBOSE
                            subtask_list.status(subtask,STS_eos);
                        } else if (skip==1){
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " store_data() SKIP: setting subtask status from " <<subtask_list.status(subtask)<< " to STS_skip"<<endl;
                            }
#endif // VERBOSE
                            subtask_list.status(subtask,STS_skip);
                        }
                        pending_subtasks_fifo.push(subtask);
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " pushed onto pending_subtasks_fifo"<<endl;
                        }
#endif // VERBOSE
                    } else {
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " still missing " <<subtask_list.nargs_absent(subtask)<< " arg(s)"<<endl;
                        }
#endif // VERBOSE
                    }
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " store_data() done for " <<data_address<< " of subtask " <<subtask<< "" <<endl; 
                    }
#endif // VERBOSE
                }
                            subtask_list.unlock();
            }
        } // of while        

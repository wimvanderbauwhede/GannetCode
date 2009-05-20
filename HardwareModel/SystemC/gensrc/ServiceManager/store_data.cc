#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " store_data()"<<endl;
        }
#endif // VERBOSE
        while (true) {
            Packet_t data_packet=data_fifo.shift();
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                 cout << ppPacket(data_packet) <<"\n";
            }
#endif // VERBOSE

            Word label= getReturn_as(getHeader(data_packet));
            MemAddress data_address=getSubtask(label) & F_DataAddress;

#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() ADDRESS: " <<data_address<< ""<<endl;
            }
#endif // VERBOSE
            if (getStatus(symbol_table[data_address])!=DS_present){
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " store_data() STATUS: ";
                     cout << (int)getStatus(symbol_table[data_address])<<"\n";
                }
#endif // VERBOSE

                if (getLength_p(data_packet)==0){
                    symbol_table[data_address]=setStatus(symbol_table[data_address],DS_present);
                } else { 

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
                    
                    symbol_table[data_address]=setStatus(symbol_table[data_address],DS_present);
                    if ((data_address<DATA_OF+NREGS)){
                        register_set[data_address].status=RDS_present;
                    }                    
                    
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() address " <<data_address<< " STATUS: "<<endl;
                         cout << (int)getStatus(symbol_table[data_address])<<"\n";
                    }
#endif // VERBOSE
                } 
                Subtask subtask=getSubtask(symbol_table[data_address]);
#ifdef VERBOSE
                                if (debug_all or service==debug_service){
                                    cout << "" <<service<< " store_data() SUBTASK: " <<subtask<< ""<<endl;
                                }
#endif // VERBOSE
                            subtask_list.lock();                      
                if (subtask_list.status(subtask)!=STS_deleted){
                    subtask_list.decr_nargs_absent(subtask);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " in list"<<endl;
                    }
#endif // VERBOSE

                    if (subtask_list.nargs_absent(subtask)==0){
                        subtask_list.status(subtask,STS_pending);
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

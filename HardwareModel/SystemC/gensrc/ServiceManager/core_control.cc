#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " core_control(): core_status = " <<core_status<< ""<<endl;
        }
#endif // VERBOSE
 while (true) {        
        uint redir=0;
        if (core_status == CS_idle) {
#ifdef VERBOSE
            cout << "" <<service<< " core_control(): set core_status to CS_Ready"<<endl;
#endif // VERBOSE
            current_subtask=pending_subtasks_fifo.shift();
            if (subtask_list.status(current_subtask)==STS_pending){
#ifdef VERBOSE
            cout << "" <<service<< " core_control(): set to status for subtask " <<current_subtask<< " to STS_processing"<<endl;
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_processing);
            }
            core_status = CS_ready;
        
        
        } else {  
#ifdef VERBOSE
             OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " 
             <<name()<<" core_control(): Waiting for ServiceCore (core_status=="<<core_status<<")\n";
#endif // VERBOSE
             if (core_status != CS_done) { 
                  wait(core_status.value_changed_event());
              }
#ifdef VERBOSE
             OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
             <<name()<<" core_control(): done waiting (core_status=="<<core_status<<")\n";
#endif // VERBOSE
       }  
        
        
        if (core_status == CS_ready){
            n_args=subtask_list.nargs(current_subtask);
            #ifndef STATIC_ALLOC
            arg_addresses=build_value_address_list();
            #else
             arg_addresses=subtask_list.arguments(current_subtask);
             arg_addresses.size(n_args);
            #endif
			Symbol_t called_as=subtask_list.called_as(current_subtask);
            opcode=getName(called_as) & F_Opcode;
            scid=(getName(called_as) & F_SCId) >> FS_SCId;
#ifdef VERBOSE
                cout << "" <<service<< " CORE CONTROL: " <<arg_addresses[0]<< ""<<endl;
#endif // VERBOSE
             results_store.clear();
            core_status=CS_busy;
        }   

        if (core_status == CS_done){
#ifdef VERBOSE
            cout << "" <<service<< " core_control(): ServiceCore is done!"<<endl;
#endif // VERBOSE
 			
            if (subtask_list.status(current_subtask)==STS_processing){
                subtask_list.status(current_subtask,STS_processed);
            }
            Service to=subtask_list.to(current_subtask);
            Service return_to=subtask_list.return_to(current_subtask);
            Word return_as=subtask_list.return_as(current_subtask);
            uint prio=0;
            redir=subtask_list.redir(current_subtask);
            Word ack_to=subtask_list.ack_to(current_subtask);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " core_control(): REDIR: " <<redir<< "; ACK_TO: " <<ack_to<< ""<<endl;
                cout << "" <<service<< " core_control(): RETURN_TO: " <<return_to<< " (" <<current_subtask<< ")"<<endl;
                cout << "" <<service<< " core_control(): RETURN_AS: " <<return_as<< " (" <<current_subtask<< ")"<<endl;
            }
#endif // VERBOSE
             Length_t payload_length;
             Word_List packet_payload;
            uint mode=subtask_list.mode(current_subtask);
            uint offset=subtask_list.offset(current_subtask);
            uint fsize=subtask_list.fsize(current_subtask);
            if (mode==M_normal){
                 Word_List results = (Word_List)results_store;
                packet_payload=getField(results,offset,fsize);
                payload_length=packet_payload.size();
            } else {
                uint reg_addr = subtask_list.reg(current_subtask);
                Symbol_t reg_symbol = mkSymbol(K_D,0,0,0,0,((mode << FS_Mode)+(reg_addr << FS_Reg)),service);

#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "core_control(): Mode " <<mode<< ": buffering "<<endl;
                    if ((getKind(results_store[0])==K_B)){
                        cout << "\tK_B: " <<results_store[1]<< ""<<endl;
                    } else {
                        cout << ppPayload(results_store)<<endl;
                    }
                    cout << " in reg " <<reg_addr<< ""<<endl;
                }
#endif // VERBOSE
                payload_length=0;
                
                 Word_List results=(Word_List)results_store;
                data_store.mput(reg_addr,getField(results,offset,fsize));
                register_set[reg_addr].status=RDS_present;
                Word reg_status_symbol=setStatus(reg_symbol,DS_present);
                symbol_table[reg_addr]=reg_status_symbol;
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                }
#endif // VERBOSE
                if (request_table.size(reg_addr)>0 ){
                    Word packet_label=request_table.shift(reg_addr);
                    MemAddress data_address=register_set[reg_addr].data_address;
                    Word_List tdata=data_store.mget(data_address);
                    Length_t payload_length=tdata.size();
                    Header_t packet_header = mkHeader(P_data,0,0,payload_length, getName(packet_label), NA,0,packet_label);
                    Packet_t packet = mkPacket(packet_header,tdata);
                    if (getTo(packet_header) != service){
                        tx_fifo.push(packet);
                    } else {
                        data_fifo.push(packet);
                    }
                }
            } 
            Header_t packet_header = mkHeader(core_return_type,prio,redir,payload_length,to,return_to,ack_to,return_as);
            Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " core_control(): PACKET:\n " <<ppPacket(packet)<< ""<<endl;
            }
#endif // VERBOSE
            if (to != service){
                tx_fifo.push(packet);
            } else {
                demux_packets_by_type(packet);
            }
        } // of CS_done

        if (core_status == CS_done or core_status == CS_managed){
            redir=subtask_list.redir(current_subtask);
#if VM==1             
#endif // VM           

            if (redir==1  ){
#if VM==1
                    if (ack_ok==1){
                        send_ack();
                    }
#else // VM==0
                    send_ack();
#endif // VM
            } // of redir

            
            Subtask_Status sts=subtask_list.status(current_subtask);
#ifdef VERBOSE
            cout << "" <<service<< " CLEAN-UP " <<current_subtask<< "? STS=" <<sts<< "<>3,4,6"<<endl;
#endif // VERBOSE
            if ((sts==STS_processed or sts==STS_cleanup or sts==STS_inactive) ){
                if (sts==STS_processed or sts==STS_inactive){
#ifdef VERBOSE
                                        cout << "" <<service<< " CLEAN-UP: clean_up() " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                    clean_up();
                    if (sts==STS_processed){
                        subtask_list.status(current_subtask,STS_cleanup);
                        sts=STS_cleanup;
                    }
                }
                if (sts==STS_processed or sts==STS_cleanup){
#ifdef VERBOSE
                    cout << "" <<service<< " CLEAN-UP: remove " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                    subtask_list.remove(current_subtask);
                              
                
#if VM==1
                    subtasks_address_stack.push(current_subtask);
#else // VM==0
#endif // VM
                    current_subtask=0;
                }
            } else if (sts==STS_blocked ){
#ifdef VERBOSE
                cout << "" <<service<< " CLEAN-UP: reset " <<current_subtask<< " status to STS_new"<<endl;
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_new);
            } else {
#ifdef VERBOSE
                cout << "" <<service<< " NO CLEAN-UP " <<current_subtask<< ""<<endl;
                if (core_status == CS_managed){
                    clean_up();
                }
#endif // VERBOSE
            }

            results_store.clear();
            core_status= CS_idle;
        } // of CS_done || CS_managed
  } // end of while(true)        

#ifdef VERBOSE
        if (debug_all or service==debug_service){
        }
#endif // VERBOSE
 while (true) {        
        if (core_status == CS_idle) {
#ifdef VERBOSE
            cout << "" <<service<< " core_control(): set core_status to CS_Ready"<<endl;
#endif // VERBOSE
            current_subtask=pending_subtasks_fifo.shift();
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " core_control(): shifted " <<current_subtask<< " off pending_subtasks_fifo" <<endl; 
#endif // VERBOSE
                        subtask_list.lock();
            uint subtask_status=subtask_list.status(current_subtask);
#ifdef VERBOSE
            cout << "" <<service<< " core_control(): subtask status " <<subtask_status<< ""<<endl;
#endif // VERBOSE
            if (subtask_status==STS_pending){
#ifdef VERBOSE
                cout << "" <<service<< " core_control(): set status for subtask " <<current_subtask<< " to STS_processing"<<endl;
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_processing);
            } else if (subtask_status==STS_eos or subtask_status==STS_skip ){


                if (service!=S_LET and service!=S_IF){
                    if (subtask_status==STS_skip ){
                        core_status = CS_skip;
                    } else {
                        core_status = CS_eos;
                    }
                    core_return_type=P_data;
                } else {
                }        
                    subtask_list.status(current_subtask,STS_processed);
            } else {
#ifdef VERBOSE
                    cout << "" <<service<< " core_control(): status for subtask " <<current_subtask<< " is " <<subtask_list.status(current_subtask)<< ""<<endl;
#endif // VERBOSE
            }
             subtask_list.unlock();
            if (core_status != CS_eos and core_status != CS_skip){
                core_status = CS_ready;
            }
        
        } else {
#ifdef SC_VERBOSE
             OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " 
             <<name()<<" core_control(): Waiting for ServiceCore (core_status=="<<core_status<<")\n";
#endif
             //if ((core_status != CS_done) && (core_status != CS_eos ) ) {
             if ( core_status == CS_busy ) {  
                  wait(core_status.value_changed_event());
              }
#ifdef SC_VERBOSE      
             OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
             <<name()<<" core_control(): done waiting (core_status=="<<core_status<<")\n";
#endif
       }
        
        
        if (core_status == CS_ready or core_status == CS_eos or core_status == CS_skip){
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
            if (core_status==CS_ready){
                core_status=CS_busy;
            }
        }
        if ((core_status == CS_done) or (core_status == CS_done_eos) or (core_status == CS_eos) or (core_status == CS_skip)){
#ifdef VERBOSE
            string ppstatus=(core_status == CS_done)?"CS_done":((core_status == CS_skip)?"CS_skip":"CS_eos");
            cout << "" <<service<< " core_control():  ServiceCore status is " <<ppstatus<< " for <" <<current_subtask<< ">"<<endl;
#endif // VERBOSE
#ifdef SC_VERBOSE            
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " core status is "<<core_status<<" <>EOS1\n";                      
#endif 			
            if (subtask_list.status(current_subtask)==STS_processing){
                subtask_list.status(current_subtask,STS_processed);
            }
            Service to = subtask_list.to(current_subtask);
            Service return_to=subtask_list.return_to(current_subtask);
            Word return_as=subtask_list.return_as(current_subtask);
            uint ctrl=0;
            uint eos=0;
            uint             skip=0;
           uint empty_packet=0;
            if ((core_status==CS_skip)){
                skip=1;
                ctrl=6;
            } else if ((core_status==CS_eos) or (core_status == CS_done_eos) ){
#ifdef SC_VERBOSE                
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") EOS core status:"<<core_status<<";"<<results_store.size()<<endl;
#endif                                
                eos=1;
                ctrl=2;
            }    
            if ((core_status==CS_skip) or (core_status==CS_eos) or (core_status == CS_done_eos)){
                if (core_return_type==P_data){
                    empty_packet=1;
                }
            }
#ifdef VERBOSE
#endif // VERBOSE
            uint redir=subtask_list.redir(current_subtask);
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
            payload_length=0;
            if (mode==M_normal){
                if (empty_packet==0){
                     Word_List results = (Word_List)results_store;
                    packet_payload=getField(results,offset,fsize);
                    payload_length=packet_payload.size();
                } else {
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " core_control(): EOS on " <<current_subtask<< ""<<endl;
                    }
#endif // VERBOSE
                }
            } else {
                uint reg_addr = subtask_list.reg(current_subtask);
                Word reg_symbol=symbol_table[reg_addr];
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " core_control(): Mode " <<mode<< ": buffering; reg: " <<reg_addr<< " <" <<reg_symbol<< "> "<<endl;
                    if (results_store.size()>0){
                        if ((getKind(results_store[0])==K_B)){
                            cout << "\tK_B: " <<results_store[1]<< ""<<endl;
                        } else {
                            cout << ppPayload(results_store)<<endl;
                        }
                    } else {
                        cout << "[]";
                    }
                    cout << " in reg " <<reg_addr<< ""<<endl;
                }
#endif // VERBOSE
                



                
                

                register_set[reg_addr].status=RDS_present;
                 Word reg_status_symbol;
                if (empty_packet==0){
                     Word_List results=(Word_List)results_store;
                    data_store.mput(reg_addr,getField(results,offset,fsize));
                    reg_status_symbol=setStatus(reg_symbol,DS_present);
#ifdef SC_VERBOSE                       
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " set status for reg "<< reg_addr<<" to DS_present <>EOS\n";
#endif                                          
                } else if (eos==1                    ){
                    reg_status_symbol=setStatus(reg_symbol,DS_eos);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " core_control(): REG " <<reg_addr<< " STATUS: EOS"<<endl;
                        cout << "" <<service<< " core_control(): EOS on " <<current_subtask<< ""<<endl;
                    }
#endif // VERBOSE
#ifdef SC_VERBOSE                                         
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " set status for reg "<< reg_addr<<" to DS_eos: EOS!\n";
#endif
                } else if (skip==1 ){
                    reg_status_symbol=setStatus(reg_symbol,DS_requested);
                }
                symbol_table[reg_addr]=reg_status_symbol;
#ifdef SC_VERBOSE                 
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " reg status for "<< reg_addr<<" is "<<(int)getStatus(reg_status_symbol)  <<"\n";
#endif                
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                }
#endif // VERBOSE
                if (request_table.size(reg_addr)>0  and empty_packet==0){
#ifdef VERBOSE
                    cout <<  "" <<service<< " core_control(): PENDING REQUESTS for " <<reg_addr<< ": " <<request_table.size(reg_addr)<< ""<<endl;
#endif // VERBOSE
                    uint     n_pending_reqs= request_table.size(reg_addr);
#ifdef SC_VERBOSE                    
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") pending requests:"<< n_pending_reqs << "\n";
#endif                             
                    for(uint req=1;req<=n_pending_reqs ;req++) {
                    Word packet_label=request_table.shift(reg_addr);
                    MemAddress data_address=register_set[reg_addr].data_address;
                    Word_List packet_payload=data_store.mget(data_address);
                    payload_length=packet_payload.size();
                    Header_t packet_header = mkHeader(P_data,ctrl,0,payload_length, getName(packet_label), NA,0,packet_label);
                    Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef SC_VERBOSE                    
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send queued packet (length "<< (int)payload_length<<", actual "<< packet_payload.size()<<") from core_control()\n";
#endif                    
                    if (getTo(packet_header) != service){
                        tx_fifo.push(packet);
                    } else {
                        data_fifo.push(packet);
                    }
                    }
                }                
            }
            Header_t packet_header = mkHeader(core_return_type,ctrl,redir,payload_length,to,return_to,ack_to,return_as);
            Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " core_control(): PACKET:\n " <<ppPacket(packet)<< ""<<endl;
            }
#endif // VERBOSE
#ifdef SC_VERBOSE            
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from core_control()\n";
#endif            
            if (to != service){
                tx_fifo.push(packet);
            } else {
                demux_packets_by_type(packet);
            }
            core_status = CS_done;
        } // of CS_done || CS_done_eos || CS_eos || CS_skip

        if (core_status == CS_done or core_status == CS_managed ){
            uint redir=subtask_list.redir(current_subtask);
#if VM==1             
#endif // VM           
#ifdef SC_VERBOSE
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") core status is "<<core_status<<" <>EOS2"<<endl;                
#endif
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

            if ((sts==STS_processed or sts==STS_cleanup or sts==STS_inactive) ){
                if (sts==STS_processed or sts==STS_inactive){
#ifdef VERBOSE
                    cout << "" <<service<< " CLEAN-UP: clean_up() " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                    clean_up();
                    if (sts==STS_processed ){
                        subtask_list.status(current_subtask,STS_cleanup);
                        sts=STS_cleanup;
                    }
                }
                if (sts==STS_cleanup ){
#ifdef VERBOSE
                    cout << "" <<service<< " CLEAN-UP: remove " <<current_subtask<< ", set status to STS_deleted"<<endl;
#endif // VERBOSE
                    subtask_list.remove(current_subtask);
                }                                                      
                if (sts==STS_cleanup or sts==STS_inactive ){

#if VM==1
#ifdef VERBOSE
                        cout << "" <<service<< " CLEAN-UP: push " <<current_subtask<< " onto subtasks_address_stack"<<endl;
#endif // VERBOSE
                        if (subtask_list.reg(current_subtask)==0){
                            subtasks_address_stack.push(current_subtask);
                        }
                        
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
                cout << "" <<service<< " CLEAN-UP: STS=" <<sts<< ", NO CLEAN-UP for " <<current_subtask<< ""<<endl;
                if (core_status == CS_managed){
                    clean_up();
                }
#endif // VERBOSE
            }

            results_store.clear();
#ifdef SC_VERBOSE                
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") sets core status to CS_idle\n";
#endif                
            core_status= CS_idle;
        } // of CS_done || CS_managed
        
  } // end of while(true)        

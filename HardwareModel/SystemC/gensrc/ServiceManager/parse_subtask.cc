
        while (true) {
             Word subtask_word=(Word)subtask_fifo.shift();
#if VM==1
                Subtask parent_subtask=(subtask_word & 0xFFFF0000)>>16;
                MemAddress code_address= subtask_word & 0x0000FFFF;
#else // VM==0
                 Subtask parent_subtask=(Subtask)(subtask_word & 0x0000FFFF);
                 MemAddress code_address=(MemAddress)(subtask_word & 0x0000FFFF);
#endif // VM

#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): parsing " <<parent_subtask<< " (" <<code_address<< ")"<<endl;
            }
#endif // VERBOSE
            Word_List subtask=code_store.mget(code_address);
             Word service_symbol=subtask.shift();
            if (getExt(service_symbol)==1){
                 Word service_symbol_ext=subtask.shift();
                uint offset=getOffset(service_symbol_ext);
                uint fsize=getSize(service_symbol_ext);
                subtask_list.offset(parent_subtask,offset);
                subtask_list.fsize(parent_subtask,fsize);
            }
            subtask_list.called_as(parent_subtask,service_symbol);
            subtask_list.nargs(parent_subtask,getNArgs(service_symbol));
            uint    nargs_absent=getNArgs(service_symbol);
            subtask_list.nargs_absent(parent_subtask,nargs_absent);

            uint mode=getMode(service_symbol);
             uint reg_addr=0;
            if (mode!=M_normal ){
                reg_addr=getReg(service_symbol);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(): Mode " <<mode<< ": reg " <<reg_addr<< ""<<endl;
                }
#endif // VERBOSE
                subtask_list.mode(parent_subtask,mode);
                subtask_list.reg(parent_subtask,reg_addr);

                if ( register_set[reg_addr].status==RDS_absent){
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "parse_subtask(): Mode " <<mode<< ": reg " <<reg_addr<< ""<<endl;
                    }
#endif // VERBOSE
                     register_set[reg_addr].data_address=reg_addr;register_set[reg_addr].code_address=code_address;register_set[reg_addr].subtask_address=parent_subtask;
                }
            }
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): NARGS:" <<subtask_list.nargs(parent_subtask)<< ""<<endl;
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): Subtask length: " <<subtask.size()<< ""<<endl;
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): stack size " <<data_address_stack.size()<< ""<<endl;
            }
#endif // VERBOSE


            uint i=0;
            while (subtask.size()>0){
                 Word elt=subtask.shift();
                if (data_address_stack.size()==0){
                    cerr << "" <<service<< " parse_subtask(" <<parent_subtask<< "): ADDRESS STACK (" <<DATA_SZ<< ") OVERFLOW for subtask " <<parent_subtask<< "";
                    exit(1);
                }
#ifdef VERBOSE
                if (debug_all or service==debug_service                ){
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): STATUS: " <<subtask_list.status(parent_subtask)<< ""<<endl;
                }
#endif // VERBOSE
                 MemAddress data_address;
                if (getKind(elt)!=K_C){
                    data_address=data_address_stack.pop();
                } else {
                    data_address= getReg(elt);
                    if (getExt(elt)==1){
                         Word elt_ext=subtask.shift();
                        uint offset=getOffset(elt_ext);
                        uint fsize=getSize(elt_ext);
                        register_set[data_address].offset=offset;
                        register_set[data_address].fsize=fsize;
                    }
                }
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): data address " <<data_address<< " for " <<ppSymbol(elt)<< ""<<endl;
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
                }
#endif // VERBOSE
                Word arg_symbol=elt;
                arg_symbol=setSubtask(arg_symbol,parent_subtask);
                arg_symbol=setStatus(arg_symbol,DS_absent);

                #ifndef STATIC_ALLOC
                subtask_list.arguments(parent_subtask).push_back(data_address);
                #else
                 subtask_list.arguments(parent_subtask)[i]=data_address;
                #endif
                i=i+1;
                if ( getQuoted(elt)==0){
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        if (not ((getKind(elt)==K_R or getKind(elt)==K_C) or (getKind(elt) == K_D or getKind(elt) == K_L) )){
                             std::cerr << "Should be obsolete: " << ppSymbol(elt) << "\n";exit(0);
                        }
                    }
#endif // VERBOSE
                    Word var_label = elt;
                    var_label = setName(var_label,service);
                    var_label = setSubtask(var_label,data_address);
                    arg_symbol=setStatus(arg_symbol,DS_requested);
                    symbol_table[data_address]=arg_symbol;
#ifdef VERBOSE
if (debug_all or service==debug_service){
    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): LABEL for " <<ppSymbol(elt)<< " is " <<ppSymbol(arg_symbol)<< ""<<endl;
}
#endif // VERBOSE
                    uint store=getName(elt);
                    Packet_type_t packet_type=P_reference;

                    if (getKind(elt)== K_L  or getKind(elt)== K_U ){
                        store=S_LET;
                        packet_type=P_request;
                    } else if (getKind(elt)== K_D){
                        packet_type=P_request;
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): Mode: K_D REQ: subtask " <<getSubtask(elt)<< ""<<endl;
                            cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): Mode: K_D REQ (" <<getMode(elt)<< ") from reg " <<reg_addr<< ""<<endl;
                        }
#endif // VERBOSE

                    }

                    Length_t payload_length=1;
                    Ctrl_t ctrl=0;
                    Redir_t redir=0;
                    Word ack_to=0;
                    Header_t requestpacket_header = mkHeader(packet_type,ctrl,redir,payload_length,store,service,ack_to,var_label);
                     Word_List requestpacket_payload; requestpacket_payload.push_back(elt);
                    Packet_t request_packet = mkPacket(requestpacket_header,requestpacket_payload);
#ifdef SC_VERBOSE                    
          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "<<service<<" ("<<name()<<") send packet from parse_subtask()\n";
#endif                    
                    if (store != service){

                        tx_fifo.push(request_packet);
#ifdef VERBOSE
#endif // VERBOSE
                    } else {
#ifdef VERBOSE
#endif // VERBOSE
                        demux_packets_by_type(request_packet);
                    }


                 } else {

                     Word_List elt_val;
                     elt_val.push_back(elt);
                    if ((getKind(elt) == K_B or getKind(elt) == K_Q) and getExt(elt)==1){
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "Quoted Extended Builtin"<<endl;
                        }
#endif // VERBOSE
                         for (int i=0;i<getSubtask(elt);i++) {
                             Word elt_sym=subtask.shift();
                            elt_val.push_back(elt_sym);
                        }
                    } else {

                    }
#ifdef VERBOSE
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): storing " <<ppPayload(elt_val)<< " in address " <<data_address<< ""<<endl;
#endif // VERBOSE
                    data_store.mput(data_address,elt_val);

                    arg_symbol=setStatus(arg_symbol,DS_present);
                    symbol_table[data_address]=arg_symbol;
#ifdef SC_VERBOSE                    
  OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": " << name() << " set status for "<< data_address<<" to DS_present <>EOS\n";
#endif                                        
#ifdef VERBOSE
if (debug_all or service==debug_service){
    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): LABEL for " <<ppSymbol(elt)<< " is " <<ppSymbol(symbol_table[data_address])<< ""<<endl;
}
#endif // VERBOSE
                    
                    subtask_list.decr_nargs_absent(parent_subtask);
                    nargs_absent=nargs_absent-1;
                } // of quoted or not
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): subtask length is " <<subtask.size()<< "; status is " <<subtask_list.status(parent_subtask)<< ""<<endl;
                }
#endif // VERBOSE
            }

            subtask_list.lock();
            if ( subtask_list.status(parent_subtask)==STS_new and subtask_list.nargs_absent(parent_subtask)==0){
 

                
                                                
                    subtask_list.status(parent_subtask,STS_pending);
                    pending_subtasks_fifo.push(parent_subtask);
            }
#ifdef VERBOSE
if (debug_all or service==debug_service){
    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): subtask status " <<subtask_list.status(parent_subtask)<< ""<<endl;
}
#endif // VERBOSE

               subtask_list.unlock();            

#ifdef VERBOSE
if (debug_all or service==debug_service){
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< " parse_subtask(" <<parent_subtask<< "): end parsing " <<parent_subtask<< "" <<endl; 
}
#endif // VERBOSE
        } // of while subtask fifo not empty

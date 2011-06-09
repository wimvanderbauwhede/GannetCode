#ifdef VERBOSE
        if ((service!=tservice_id)){
            cout << "" <<service<< " activate_subtask_helper(): service_id " <<tservice_id<< " <> service " <<service<< ""<<endl;
        }
#endif // VERBOSE
#if VM==1
#ifdef VERBOSE
            cout << "" <<service<< " SUBTASK STACK SIZE: " <<subtasks_address_stack.size()<< ""<<endl;
#endif // VERBOSE
            if (subtasks_address_stack.size()==0){
                  std::cerr << service << " SUBTASK STACK ("<< SUBTASKS_SZ <<") OVERFLOW\n"; exit(0);
            }
            CodeAddress             subtask_address=subtasks_address_stack.pop();
#else // VM==0
            CodeAddress subtask_address=task_address;
#endif // VM
#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " activate_subtask_helper(): address: " <<subtask_address<< ""<<endl;
        }
#endif // VERBOSE
        if (is_subtask_packet ){
            subtask_list.add(subtask_address);
        }
    
#if VM==1
            Word subtask_word=(subtask_address << 16) + task_address;
#else // VM==0
             Word subtask_word=(Word)task_address;
#endif // VM
#if VM==1
            subtask_fifo.push(subtask_word);
#else // VM==0
            if (code_status[task_address]>=2 ){
                subtask_fifo.push(subtask_word);
                code_status[task_address]=2;
            } else {
#ifdef VERBOSE
                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "DEFER ACTIVATION " <<task_address<< ":" <<code_status[task_address]<< "" <<endl; 
#endif // VERBOSE
                code_status[task_address]=1;
            }
#endif // VM
        if (is_subtask_packet){
            subtask_list.return_as(subtask_address,getReturn_as_p(packet));
            subtask_list.return_to(subtask_address,getReturn_to_p(packet));
            subtask_list.to(subtask_address,getReturn_to_p(packet));
            subtask_list.redir(subtask_address, getRedir_p(packet));
            subtask_list.ack_to(subtask_address, getAck_to_p(packet));
            subtask_list.code_address(subtask_address,task_address);
            subtask_list.service_id(subtask_address,tservice_id);
        }        

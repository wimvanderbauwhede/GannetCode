        uint nargs=arg_addresses.size();
#ifdef VERBOSE
        if (debug_all or service==debug_service){
                OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": " << "" <<service<< ": clean_up " <<current_subtask<< ":  ADDRESSES: " <<nargs<< "" <<endl; 
        }
#endif // VERBOSE
        if (nargs>0){
        for(uint iter_=0;iter_<=nargs-1 ;iter_++) {
            MemAddress arg_address=arg_addresses[iter_];
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< ": clean_up:  ADDRESS: " <<arg_address<< ""<<endl;
            }
#endif // VERBOSE
             //symbol_table.lock();
            Word arg_symbol=symbol_table[arg_address];
            uint arg_status=getStatus(arg_symbol);
            if (arg_status==DS_present or  arg_status==DS_eos ){
              if (arg_address>NREGS+DATA_OF){
                arg_symbol=setStatus(arg_symbol,DS_cleared);
                data_address_stack.push(arg_address);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< ": clean_up " <<current_subtask<< ": push ADDRESS " <<arg_address<< " onto STACK"<<endl;
                    cout << "" <<service<< ": clean_up(): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
                    cout << "" <<service<< ": clean_up: REMOVED " <<arg_address<< ""<<endl;
                }
#endif // VERBOSE
                data_store.remove(arg_address);
              } else {
                arg_symbol=setStatus(arg_symbol,DS_absent);
              }
              symbol_table[arg_address]=arg_symbol;
            }
             //symbol_table.unlock();
        } // of for        
        }
        

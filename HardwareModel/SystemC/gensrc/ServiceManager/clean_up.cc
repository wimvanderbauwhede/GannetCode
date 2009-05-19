        uint nargs=arg_addresses.size();
#ifdef VERBOSE
        if (debug_all or service==debug_service){
                
        }
#endif // VERBOSE
            for(uint iter_=0;iter_<=nargs-1 ;iter_++) {
            MemAddress arg_address=arg_addresses[iter_];
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< ": clean_up:  ADDRESS: " <<arg_address<< ""<<endl;
            }
#endif // VERBOSE
            if (getStatus(symbol_table[arg_address])==DS_present and arg_address>NREGS+DATA_OF){
                symbol_table[arg_address]=setStatus(symbol_table[arg_address],DS_cleared);
                data_address_stack.push(arg_address);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< ": clean_up(): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
                    cout << "" <<service<< ": clean_up: REMOVED " <<arg_address<< ""<<endl;
                }
#endif // VERBOSE

                data_store.remove(arg_address);

            } 
        } // of for        
        

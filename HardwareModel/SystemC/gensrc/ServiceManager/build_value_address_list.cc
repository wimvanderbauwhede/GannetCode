        MemAddresses  addresses;
        uint nargs=subtask_list.nargs(current_subtask);
#ifdef VERBOSE
                cout << "" <<service<< " build_value_address_list(): " <<nargs<< "<>" <<subtask_list.arguments(current_subtask).size()<< ""<<endl;
#endif // VERBOSE

        if (nargs>0){
            Subtask_Argument_List& args=subtask_list.arguments(current_subtask);
            for(MemAddresses::iterator iter_=args.begin();iter_!=args.end();iter_++) {
            	MemAddress address=*iter_;
                addresses.push_back(address);
                nargs=nargs-1;
                if (nargs==0){
                    break;
                }
            }
        }
        return addresses;

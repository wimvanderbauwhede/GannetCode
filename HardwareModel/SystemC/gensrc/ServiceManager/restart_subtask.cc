#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " restart_subtask(" <<regaddr<< ")"<<endl;
        }
#endif // VERBOSE
        MemAddress subtask_address=register_set[regaddr].subtask_address;
        subtask_list.status(subtask_address,STS_new);

#if VM==1
            MemAddress code_address= register_set[regaddr].code_address;
            Word subtask_word=(code_address & 0x0000FFFF) + ( (subtask_address << 16) & 0xFFFF0000);
#ifdef VERBOSE
            cout << "restart_subtask VM=1: pushing " <<subtask_address<< ""<<endl;
#endif // VERBOSE
            subtask_fifo.push(subtask_word);
#else // VM==0
#ifdef VERBOSE
            cout << "restart_subtask VM=0: pushing " <<subtask_address<< ""<<endl;
#endif // VERBOSE
            subtask_fifo.push(subtask_address);
#endif // VM


#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " activate_subtask()"<<endl;
        }
#endif // VERBOSE
        while (true) {
            Packet_t subtask_ref_packet=subtask_reference_fifo.shift();
            Word_List ref_label_symbol_l=getPayload(subtask_ref_packet);

            Word ref_label_symbol=ref_label_symbol_l[0];

#ifdef VERBOSE
            cout << ppSymbol(ref_label_symbol)<<endl;
#endif // VERBOSE
            CodeAddress code_address=getCodeAddress(ref_label_symbol);
            Name_t tservice_id=getName(ref_label_symbol);
            activate_subtask_helper(code_address,tservice_id,subtask_ref_packet,true);
        } // of while

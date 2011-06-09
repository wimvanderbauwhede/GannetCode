#ifdef VERBOSE
#endif // VERBOSE
#ifdef VERBOSE
#endif // VERBOSE
                uint     ntdcs=vmif.receive(core_status);
                if (ntdcs!=0 and tasks_stack.size()>0 ){
                    core_status = CS_busy;
                    uint task_id=tasks_stack.pop();
                    vmif.iodescs[task_id]=ntdcs;
                    Bytecode                         tdc=vmif.tdcs.shift();
                    TaskDescription task_description(tdc,task_id);
                    Packet_List                 task_description_packet_list=task_description.Packets;
                     Word_List nullwl;
                     nullwl.push_back((Word)0);
                    result_store.mput(task_id,nullwl);
#if QUIT==1
                        exit(1);
#endif // QUIT              
                    for(Packet_List::iterator iter_=task_description_packet_list.begin();iter_!=task_description_packet_list.end();iter_++) {
                    	Packet_t task_description_packet=*iter_;
#ifdef VERBOSE
                        cout << "GATEWAY SENDS PACKET:"<<endl;
                        cout << ppPacket(task_description_packet)<<endl;
#endif // VERBOSE
                        tx_fifo.push(task_description_packet);
                    }                                                                                         
                }         
        

            while (true) {
                Packet_t data_packet=rx_fifo.shift();
                Word label= getReturn_as(getHeader(data_packet));
                Word_List result=getPayload(data_packet);
                
                uint task_id=getTask(label);
#ifdef VERBOSE
                cout << "\n" << service << ":" << "Storing value " <<ppPayload(result)<< " for " <<task_id<< " ...\n";
#endif // VERBOSE
#ifdef VERBOSE
                        cout << "Gateway: Interface send()"<<endl;
#endif // VERBOSE
                         vmif_send(result,task_id); 
                        tasks_stack.push(task_id);
                        uint stack_full = MAX_NTASKS;
                        if (tasks_stack.size()==stack_full){
                            core_status = CS_idle;
#ifdef VERBOSE
#endif // VERBOSE
                        }                     
            } 

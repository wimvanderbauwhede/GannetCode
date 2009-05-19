// ServiceCoreLibrary.rb
//   
// :title: Service-based SoC project - Service Core Library 
//
//    This version of Gannet is for compile-time decomposition and lexical scoping.
//    It does not work with the "old" system any more!!
//    This is the version to be ported to SystemC
//    -"values" is deprecated, we pass on addresses!
//    -The language services are minimal:
//    LET x
//    ASSIGN x
//    LAMBDA 
//    APPLY 
//    IF x
//    We'll keep BEGIN, though I see no use for it
//    -List manipulation services need a total rework. Initially, we'll support
//    LIST
//    HEAD
//    TAIL
//    LENGTH
//    CONCAT
//    and maybe CONS
//    
//    -ALU services are as before: x
//    +,-,*,/;<,==,>
//    Guess we should add AND, OR, NOT
//    
//
///* ***** BEGIN LICENSE BLOCK *****
// * Version: AFL 2.1
// *
// * The contents of this file are subject to the Academic Free License Version
// * 2.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://opensource.org/licenses/afl-2.1.php
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
// *
// * ***** END LICENSE BLOCK ***** */
//
//
//// $Id: ServiceCoreLibrary.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from SBA/ServiceCoreLibrary.rb by ./r2n.pl ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#ifndef SYSC
#include "ServiceCoreLibrary.h" 
#include "System.h" 
#include "Tile.h" 
#include "ServiceCore.h" 
#endif


using namespace std;
using namespace SC_SBA;
				
#define	FIXME_SERV_MGR 0
				


#ifndef NO_SERVICES

 Word_List SCLib::string2symbol(string str)  {
        uint  npad=NBYTES - (str.size() % NBYTES);
        uint nwords=(str.size()+npad)/NBYTES;
         str.resize(NBYTES*nwords,0);
        Symbol_t sheader = mkSymbol(K_Q,T_s&1,1,1,0,nwords,npad);
        Word_List sym;
        sym.push_back(sheader);
        for(uint i=0;i<nwords;i++) {        
            Word strword=0;
            for (uint j=0;j<NBYTES;j++) {
                strword+=(Word)str[NBYTES*i+j]<<8*(NBYTES-j-1);
            }
            cout << strword <<"\n";
            sym.push_back(strword);
        }        
        return sym;
    }


 string SCLib::sym2str(Word_List sym)  {
         Word header=sym.front();sym.pop_front();
        uint nwords=getSubtask(header);
        uint padding=getName(header);

        string str;
        str.reserve(nwords*NBYTES-padding);
        for(uint i=0;i<nwords;i++) {    
            uint npad=(i==nwords-1)?padding:0;
            Word strword=sym.front();sym.pop_front();
            for (uint j=0;j<NBYTES-npad;j++) {
                char byte=(char)((strword>>8*(NBYTES-j-1))&255);
                str+=byte;
            }
        }       
        
        return str;
    }


 Int SCLib::sym2int(Word_List sym)  {
         Word result=sym.front();sym.pop_front();
#if WORDSZ==64
         int64_t int_result=(int64_t)result;
#else // WORDSZ==32
         int32_t int_result=(int32_t)result;
#endif // WORDSZ                
        return int_result;
    }
    

 Word SCLib::sym2uint(Word_List sym)  {
         Word result=sym.front();sym.pop_front();
        return result;
    }
    

 bool SCLib::sym2bool(Word_List sym)  {
         Word result=sym.front();sym.pop_front();
#if WORDSZ==64
         int64_t int_result=(int64_t)result;
#else // WORDSZ==32
         int32_t int_result=(int32_t)result;
#endif // WORDSZ                
        return (int_result!=0);
    }
                    

 float SCLib::sym2flt(Word_List sym)  {
         Word result=sym.front();sym.pop_front();
#if WORDSZ==64
             double flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
#else // WORDSZ==32
             float flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
#endif // WORDSZ
        return flt_result;
    }
#endif // NO_SERVICES        



 Result SCLib::sba_GATEWAY(void* parent,MemAddresses& addresses)  {
         Result res; res.push_back((Word)1); return res;
    }



#ifndef NO_SERVICES    

 Int SCLib::div(Int m,Int n)  {
        Int q=0;
            Int sm=1;
            
            if (m<0){
                sm=-1;
            }
            
            Int sn=1;
            if (n<0){
                sn=-1;
            }
            
            Int           um=m*sm;
            Int     un=n*sn;
            Int modmn= um % un;
            q=(um-modmn)/un;
            if (2*modmn>un){
                q+=1;
            }  
        return q*sn*sm;
    }
    
    

 Word_List SCLib::ls_ALU(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    


#ifdef VERBOSE
        cout << "ALU CORE: processing subtask " <<parent.current_subtask<< ""<<endl;
#endif // VERBOSE

                        
         Word_List result_list;
        Int fp=0;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
            if (getDatatype(parent.symbol_table[address]) != T_i ){
                fp=1;
                break;
            }
        }
        Uint operation=parent.opcode;


#ifdef VERBOSE
    cout << "ALU (" <<parent.service<< ") CORE: " <<addresses.size()<< " addresses"<<endl;
#endif // VERBOSE
	MemAddress address=addresses[0];
 	Word result=sba_tile.data_store.mget(address)[1];
    Word res_symbol=sba_tile.data_store.mget(address)[0];
    

 result_list.push_back(res_symbol);

#if WORDSZ==64
                 Int int_result=(Int)result;
#else // WORDSZ==32
                         Int int_result=(Int)result;
#endif // WORDSZ                
#ifdef VERBOSE
                cout << "ALU CORE: arg 1: Found int " <<result<< " (" <<T_i<< ")  " <<address<< ""<<endl;
#endif // VERBOSE

        if (operation==A_not){
            result=1-result;
        } else {
            int ii=0; 
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
                ii+=1;
                if (ii>1){
                    Word tres=sba_tile.data_store.mget(address)[1];

                         Int int_tres=(Int)tres;
#ifdef VERBOSE
                        cout << "ALU CORE: arg " <<ii<< ": Found int " <<tres<< " (" <<T_i<< ")  " <<address<< ""<<endl;
#endif // VERBOSE

                 switch (operation) {
                 case A_plus :
                 {
                     int_result+=int_tres;                    
                  break;
                 }
                 case A_minus :
                 {
                     int_result-=int_tres; 
                  break;
                 }
                 case A_times   :
                 {
                     int_result*=int_tres;
                  break;
                 }
                 case A_over :
                 {
                     int_result=div(int_result,int_tres);
                  break;
                 }
                 case A_lt :
                 {
                     int_result=(int_result<int_tres)?1:0;
                  break;
                 }
                 case A_gt :
                 {
                     int_result=(int_result>int_tres)?1:0;
                  break;
                 }
                 case A_eq :
                 {
                     int_result=(int_result==int_tres)?1:0;
                     break;}
                 default:
                    cerr << "Unknown ALU CORE service: " <<operation<< "";
                    exit(1);
                       exit(0);
                } ;
            }
            }
        }
                 result=(Uint)int_result;                
#ifdef VERBOSE
        cout << "ALU CORE RESULT: (uint" <<WORDSZ<< ") " <<result<< ""<<endl;
        cout << "ALU (" <<parent.service<< ") CORE (" <<parent.current_subtask<< "):  result: " <<result<< ""<<endl;
#endif // VERBOSE
 
         result_list.push_back(result);
        return result_list;
    } // of ALU
    
#if 0 // SKIP ls_COUNTER

 Word_List SCLib::ls_COUNTER(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result_list;

        MemAddress address=addresses[0];
        Word count_upto=sba_tile.data_store.mget(address)[1];
        Word res_symbol=sba_tile.data_store.mget(address)[0];
         result_list.push_back(res_symbol);
        if (parent.state_register[0]<count_upto){
            parent.state_register[0]+=1;
            parent.state_register[1]+=parent.state_register[0];
            parent.core_status=CS_busy;
        }
         result_list.push_back(parent.state_register[1]);
        return result_list;
    } // of COUNTER
    
#endif // 0
#if 0 // SKIP ls_COUNTDOWN

 Word_List SCLib::ls_COUNTDOWN(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result_list;
        MemAddress address=addresses[0];
        Word count_downfrom=sba_tile.data_store.mget(address)[1];
         const Word res_symbol = 0xDD000001UL;
         result_list.push_back(res_symbol);
        if (parent.state_register[0]==0){
            parent.state_register[1]=count_downfrom;
            parent.state_register[0]=1;
        } else {
            parent.state_register[1]-=1;
            if (parent.state_register[1]==0){
            parent.state_register[0]=0;
            }
        }

         result_list.push_back(parent.state_register[1]);
        return result_list;
    } // of COUNTDOWN

#endif // 0
#if 0 // SKIP ls_FIB

 Word_List SCLib::ls_FIB(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         const Word res_symbol = 0xDD000001UL;        
         Word_List result_list;
         result_list.push_back(res_symbol);
        MemAddress opaddr=addresses[0];
        Word op=sba_tile.data_store.mget(opaddr)[1];

        Word numval=0;
        if (op==0){
                MemAddress num1addr=addresses[1];
        Word_List num1=sba_tile.data_store.mget(num1addr);
        MemAddress num2addr=addresses[2];
        Word_List        num2=sba_tile.data_store.mget(num2addr);
            parent.state_register[0]=num1[1];
            parent.state_register[1]=num2[1];
        } else if (op==1){
            numval=parent.state_register[0];
        } else if (op==2){
            numval=parent.state_register[1];
        } else {
        }
         result_list.push_back(numval);
        return result_list;
    } // of COUNTDOWN
            
#endif // 0
#if 0 // SKIP ls_THREAD

 Word_List SCLib::ls_THREAD(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result_list;
        MemAddress address=addresses[0];
        Word count_upto=sba_tile.data_store.mget(address)[1];
        Word res_symbol=sba_tile.data_store.mget(address)[0];

    parent.state_register[1]=count_upto; 
        
        if (parent.state_register[0]==0){
            parent.core_status=CS_busy;
            parent.state_register[0]==1;
#ifdef VERBOSE
            cout << "THREAD: Starting core thread..."<<endl;
#endif // VERBOSE
                       
#ifdef THREADED_CORE
                pthread_t tid;
                pthread_attr_t attr;
                void* tstatus;
                
                pthread_attr_init(&attr);
                pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
                pthread_create(&tid, &attr, SCLib::run_dct, (void*)parent_ptr);        
#endif
                                                 
        } else if (parent.state_register[0]==1){
#ifdef VERBOSE
                        cout << "THREAD: Core is busy ..."<<endl;
#endif // VERBOSE
            parent.core_status=CS_busy;
        } else {
#ifdef VERBOSE
                    cout << "THREAD: Core has finished ..."<<endl;
#endif // VERBOSE
        }                                            
         result_list.push_back(count_upto);
        return result_list;
    } // of THREAD

       
        
#endif // 0
#if 0 // SKIP ls_DELAY

 Word_List SCLib::ls_DELAY(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         const Word res_symbol = 0xDD000001UL;        
         Word_List result_list;
         result_list.push_back(res_symbol);        
        MemAddress delay_address=addresses[0];
        Word delay_val=sba_tile.data_store.mget(delay_address)[1];
        MemAddress res_address=addresses[1];
        Word res_val=sba_tile.data_store.mget(res_address)[1];

        if (parent.state_register[0]==0){
#ifdef VERBOSE
#endif // VERBOSE
            parent.core_status=CS_busy;
            parent.state_register[0]=1;
            parent.state_register[1]=0;

        // Using POSIX threads, purely for experimenting. We really want HW threads of course

        } else if (parent.state_register[0]==1){
            parent.core_status=CS_busy;
        } else { 
            parent.core_status=CS_done;
            parent.state_register[0]=0;
#ifdef VERBOSE
#endif // VERBOSE
        }
         result_list.push_back(parent.state_register[1]);
        return result_list;
    } // of DELAY     

#endif // 0
#if 0 // SKIP ls_HWTHREAD

 Word_List SCLib::ls_HWTHREAD(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result_list;
        Word res_data=0;

        if (parent.state_register[0]==0){
            parent.core_status=CS_busy;
            parent.state_register[0]==1;
             Word_List input_data;
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
                input_data.push_back(sba_tile.data_store.mget(address)[1]);
            }

            
                parent.state_register[1]=7188;
                parent.state_register[0]=2;
        } else if (parent.state_register[0]==1){
            parent.core_status=CS_busy;
        } else { 
            parent.state_register[0]=0;
            res_data=parent.state_register[1];
        }        
         result_list.push_back(res_data);
        return result_list;
    } // of HWTHREAD 

    
    
#endif // 0

 Word_List SCLib::ls_BEGIN(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
                result=sba_tile.data_store.mget(address);
        }
#ifdef VERBOSE
         cout << parent.service<< " CORE: Passing on result\n";
#endif // VERBOSE
        return result;
    } // of BEGIN
    
    
#if 0 // SKIP ls_DATA

 Word_List SCLib::ls_DATA(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

        Word status=1;
         Word_List status_list;
         status_list.push_back(status);
    cerr << "ls_DATA broken!";
    exit(1);
     
    } // of DATA
#endif // 0

 Word_List SCLib::ls_IF(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

    parent.ack_ok=1;
    if (parent.subtask_list.waiting_for_ack(parent.current_subtask)==1){
        parent.ack_ok=1;
        parent.core_status=CS_managed;
        parent.subtask_list.status(parent.current_subtask,STS_processed);
         Word_List empty_list; return empty_list;
    } else {
        Word service=parent.subtask_list.called_as(parent.current_subtask);
        uint     operation=getName(service);
        MemAddress  valaddress=0;
        if (operation==A_RETURN or operation==A_RETURNTC){
#ifdef VERBOSE
            cout << "IF CORE: " <<parent.current_subtask<< ": (RETURN \n";
#endif // VERBOSE
            valaddress=addresses[0];

        } else { 
#ifdef VERBOSE
            cout << "IF CORE: " <<parent.current_subtask<< ": (IF \n";
#endif // VERBOSE
            Word  condval;
            MemAddress condaddr=addresses[0];
                condval=sba_tile.data_store.mget(condaddr)[1];
            if (condval>0){
                condval=1;
            } else {
                condval=0;
            }
#ifdef VERBOSE
            cout << "IF CORE: CONDVAL:" <<condval<< ""<<endl;
            cout << "IF CORE: LABEL1:<" <<addresses[1]<< ">"<<endl;
            cout << "IF CORE: LABEL2:<" <<addresses[2]<< ">"<<endl;
#endif // VERBOSE
            if (condval==1){
                valaddress=addresses[1];
            } else if (condval==0){
                valaddress=addresses[2];
            }
           
        } 
        Word_List result_list=sba_tile.data_store.mget(valaddress);
        Word result=result_list[0];
        Word label=parent.symbol_table[valaddress];
#ifdef VERBOSE
        cout << "IF CORE: LABEL:<" <<label<< ">"<<endl;
        cout << "IF CORE: KIND <";
         cout <<(int)getKind(label);
        cout << ">"<<endl;
#endif // VERBOSE
        int labelquoted=getQuoted(label);
        if (labelquoted==1){
                    if (getKind(result) == K_S or getKind(result) == K_U){
                        cerr << "IF CORE: ERROR: IF arg can't be " <<getKind(result)<< "";
                        exit(1);
                    } else if (getKind(result) == K_L or getKind(result) == K_D){
                        cerr << "IF CORE: ERROR: IF arg can't be " <<getKind(result)<< "";
                        exit(1);
                        parent.core_return_type= P_request;
                        parent.subtask_list.to(parent.current_subtask,getKind(result));
                    } else if (getKind(result) == K_B or getKind(result) == K_Q                         ){
                        parent.core_return_type=P_data;
                    } else if (getKind(result) == K_R   ){

                        
                        Word send_ack_to=0;
                        if (operation==A_RETURN or operation==SC_IF){
                            send_ack_to=setName(label,S_IF);
                            send_ack_to=setSubtask(send_ack_to,valaddress);
                            parent.ack_ok=0;
                        
                            parent.symbol_table[valaddress]=setStatus(label,DS_requested);
                            parent.subtask_list.status(parent.current_subtask,STS_blocked);
                            parent.subtask_list.incr_nargs_absent(parent.current_subtask);
                            parent.subtask_list.waiting_for_ack(parent.current_subtask,1);
#ifdef VERBOSE
                                cout << "IF CORE: BLOCKED " <<parent.current_subtask<< " for ACK: " <<operation<< ""<<endl;
#endif // VERBOSE
                        } else {
                            parent.subtask_list.redir(parent.current_subtask,0);
                            parent.subtask_list.status(parent.current_subtask,STS_inactive);
#ifdef VERBOSE
                            cout << "IF CORE: TAILCALL: " <<operation<< ""<<endl;
#endif // VERBOSE
                        }
                        parent.core_status=CS_managed;
                        Packet_Type packet_type=P_reference;
                        Prio_t prio=0;
                        Length_t payload_length=1;
                        To_t to=getName(result);
                        Return_to_t return_to=parent.subtask_list.return_to(parent.current_subtask);
                        Word return_as=parent.subtask_list.return_as(parent.current_subtask);
                        Redir_t                         redir=0;
                        if (operation==A_RETURN or operation==SC_IF ){
                            redir=1;
                        }
                        Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,send_ack_to,return_as);
                        
                        Word_List  ref_packet_payload=result_list;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
#ifdef VERBOSE
                             cout << "IF CORE: REDIR "<<parent.current_subtask<<" TO "<<(int)getName(result)<<endl; 
#endif // VERBOSE
                            parent.core_return_type= P_reference;
                            parent.subtask_list.to(parent.current_subtask,getName(result));
                        
                        if (to!=S_IF){
                            parent.transceiver_tx_fifo.push(ref_packet);
                        } else {
#ifdef VERBOSE
                            cout << "IF CORE: LOCAL CALL"<<endl;
#endif // VERBOSE
                            parent.subtask_reference_fifo.push(ref_packet);
                        }
                        

                    } else {
                        cerr << "IF CORE: TROUBLE: don't know <" <<getKind(result)<< ">";
                        exit(1);
                    }          
        } else {
            parent.core_return_type=P_data;
        }
#ifdef VERBOSE
        cout << "IF CORE: (" <<ppSymbol(result_list[0])<< ")"<<endl;
        cout << "IF CORE: SUBTASK: " <<parent.current_subtask<< ""<<endl;
        cout << "IF CORE: " <<parent.core_return_type<< " TO: " <<parent.subtask_list.to(parent.current_subtask)<< ""<<endl;
#endif // VERBOSE
        return result_list;
        }
    } // of ls_IF




#if 0 // SKIP ls_S_IF

 Word_List SCLib::ls_S_IF(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

        Word operation=parent.opcode;
        MemAddress  valaddress=0;
        if (operation==A_RETURN ){
            valaddress=addresses[0];
        } else { 
            MemAddress condaddr=addresses[0];
            Word condval=sba_tile.data_store.mget(condaddr)[1];
            if (condval!=0){
                valaddress=addresses[1];
            } else {
                valaddress=addresses[2];
            }           
        } 
        
        Word_List result_list=sba_tile.data_store.mget(valaddress);
        Word result=result_list[0];
        if (getKind(result) == K_D){
            parent.core_return_type= P_request;
        } else if (getKind(result) == K_B                         ){
            parent.core_return_type=P_data;
        } else if (getKind(result) == K_R   ){
            parent.core_return_type= P_reference;
        }          
        
        return result_list;
        
    } // of ls_S_IF        

#endif // 0

 Word_List SCLib::ls_RAND(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

    Word_List result_list;        
        MemAddress address0=addresses[0];
        Word min_val=sba_tile.data_store.mget(address0)[1];
        MemAddress address1=addresses[1];
        Word max_val=sba_tile.data_store.mget(address1)[1];
         const Word res_symbol = 0xDD000001UL;
         result_list.push_back(res_symbol);
         srandom(parent.state_register[0]);        
         Word rnd_val = random();
         Word res_val=min_val + max_val + (rnd_val % max_val);  
         result_list.push_back(res_val);
        parent.state_register[0]=res_val;
#ifdef VERBOSE
        cout << "RAND CORE RESULT: " <<res_val<< ""<<endl;
#endif // VERBOSE
        return result_list;
        
    } // of ls_RAND     


#if 0 // SKIP ls_RND_MATRIX

 Word_List SCLib::ls_RND_MATRIX(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result_list;        
        Word min_val=0;
        Word max_val=255;
        for(int i=0;i<=63 ;i++) {
             Word res_val=min_val+(random() % max_val);
             result_list.push_back(res_val);
#ifdef VERBOSE
            cout << "RAND MATRIX CORE RESULT"<<endl;
#endif // VERBOSE
        }
        return result_list;
        
    } // of ls_RND_MATRIX     


#endif // 0
#if 0 // SKIP ls_PROC_MATRIX

 Word_List SCLib::ls_PROC_MATRIX(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

         Word_List result_list;
        Uint operation=parent.opcode;
        bool first=true;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
            Word_List matrix=sba_tile.data_store.mget(address);
            if (first            ){
                for(Word_List::iterator iter_=matrix.begin();iter_!=matrix.end();iter_++) {
                	Word elt=*iter_;
                     result_list.push_back(elt);
                    }
                first=false;
            }
        }    
        return result_list;
    } // of ls_PROC_MATRIX     
 
#endif // 0

 Word_List SCLib::ls_LET(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

        Word service_word=parent.subtask_list.called_as(parent.current_subtask);
        
    	  Name_t service=getName(service_word);
        parent.core_return_type=P_data;
#ifdef VERBOSE
         int ppservice=(int)service;
        cout << "LET (" <<parent.service<< ") CORE: " <<parent.current_subtask<< ": (" <<ppservice<< "<>" <<SC_LET<< "\n";
#endif // VERBOSE
  
        if (service==SC_LET or service==A_LETTC){
#ifdef VERBOSE
        cout << "LET (" <<parent.service<< ") CORE: " << parent.current_subtask << "\n";

            cout << "LET (" <<parent.service<< ") CORE: " <<  "TO: " <<  parent.subtask_list.to(parent.current_subtask) << "\n";
            cout << "LET (" <<parent.service<< ") CORE: " <<  "RETURN TO: " <<  parent.subtask_list.return_to(parent.current_subtask) << "\n";
            cout << "LET (" <<parent.service<< ") CORE: " <<  "RETURN AS: " <<  parent.subtask_list.return_as(parent.current_subtask) << "\n";
            cout << "LET (" <<parent.service<< ") CORE: " <<  "CALLED AS: " <<  parent.subtask_list.called_as(parent.current_subtask) << "\n";
#endif // VERBOSE
            bool last=false;
            uint         nargs=parent.n_args;
            uint argct=nargs;
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
                argct-=1;
                last=(argct==0);
                Word      label=parent.symbol_table[address];
#ifdef VERBOSE
                cout << "LET (" <<parent.service<< ") CORE: " <<  "LABEL:" <<  label << "\n";
#endif // VERBOSE
                
                if (getQuoted(label)==1){
                    label=setQuoted(label,0);
                    Word numval=sba_tile.data_store.mget(address)[0];
                    label=setStatus(label,DS_requested);
                    parent.subtask_list.incr_nargs_absent(parent.current_subtask);
                    parent.symbol_table[address]=label;
                    if (getKind(numval)!=K_R                  ){
                    }
#ifdef VERBOSE
                        cout << "LET (" <<parent.service<< ") CORE: BLOCKED " <<parent.current_subtask<< ""<<endl;
#endif // VERBOSE
                    parent.subtask_list.status(parent.current_subtask,STS_blocked);
                    To_t to=getName(numval);
                    Return_to_t return_to=S_LET;
                    Word var_label = setSubtask(label,address);
                    var_label = setName(var_label,S_LET);
                    Word return_as=var_label;
                    Word ack_to=0;
                    Packet_Type packet_type=P_reference;
                    Prio_t prio=0;
                    Redir_t redir=0;
                    Word_List reslist;
                    reslist.push_back(numval);
                    Length_t payload_length=1;
                    bool use_redir=true;
                    
                    if ((last and use_redir)      ){
                        parent.core_status=CS_managed;
                        Word send_ack_to=setName(label,S_LET);
                        send_ack_to=setSubtask(send_ack_to,address);
                        parent.ack_ok=0;
                        if (service!=A_LETTC                        ){
                            parent.subtask_list.waiting_for_ack(parent.current_subtask,1);
                        }
                        return_as=parent.subtask_list.return_as(parent.current_subtask);
                        return_to=parent.subtask_list.return_to(parent.current_subtask);
                        Header_t ref_packet_header = mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as);
                        Word_List  ref_packet_payload=reslist;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
                        if (to!=S_LET){
                            parent.transceiver_tx_fifo.push(ref_packet);
                        } else {
                            parent.subtask_reference_fifo.push(ref_packet);
                        }
                    } else {
                        parent.core_status=CS_managed;
                        Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,ack_to,return_as);
                        Word_List  ref_packet_payload=reslist;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
                        if (to!=S_LET){
                            parent.transceiver_tx_fifo.push(ref_packet);
                        } else {
                            parent.subtask_reference_fifo.push(ref_packet);
                        }
                    } // of if last
                    if ( service!=A_LETTC                    ){
                        return reslist;
                    } else { 
                        break;
                    }    
                } // of if quoted
            } // of for
                    
            MemAddress labeladdr=addresses[nargs-1];
            Word_List result=sba_tile.data_store.mget(labeladdr);
            parent.core_return_type=P_data;
            parent.subtask_list.status(parent.current_subtask,STS_processing);
            if (parent.subtask_list.waiting_for_ack(parent.current_subtask)==1){
#ifdef VERBOSE
                cout << "LET CORE: ACK REDIR: " <<parent.subtask_list.redir(parent.current_subtask)<< ""<<endl;
#endif // VERBOSE
                parent.ack_ok=1;
                parent.core_status=CS_managed;
                parent.subtask_list.status(parent.current_subtask,STS_processed);
                parent.subtask_list.waiting_for_ack(parent.current_subtask,0);
            }

            
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress labeladdr=*iter_;
#ifdef VERBOSE
                cout << "LET-ASSIGN\tFound " <<labeladdr<< ""<<endl;
#endif // VERBOSE
                Word_List var_label_l=sba_tile.data_store.mget(labeladdr);
                    if (var_label_l.size()>0        ){
                    Word     var_label=var_label_l[0];
                    if (getKind(var_label)==K_L){
                        MemAddress  var_address=getSubtask(var_label);
                        Name_t  var_name=getName(var_label);
                    
                        bool has_label=false;
                        
                        if (sba_tile.lookup_table.count(var_name)==1){
                            Word word=sba_tile.lookup_table.read(var_name);
                            if (getSubtask(word)==var_address){
                                has_label=true;
                                sba_tile.lookup_table.erase(var_name);
                            }
                        }
                        
                        if (has_label==true){
                            parent.data_address_stack.push(var_address);
                        }                                        
                    }
                    }                        
                    
            }
            return result;
        } else { 
            Word result=0;
             #ifndef STATIC_ALLOC
            List<Word_List>             value_list;
             #else
             List<Word_List,MAX_NARGS> value_list;
             #endif
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
#ifdef VERBOSE
                    Word_List try_array=sba_tile.data_store.mget(address);
                    cout << "LET (" <<parent.service<< ") CORE: " <<address<< "\t" <<ppPayload(try_array)<< "\n";
#endif // VERBOSE
                    value_list.push_back(sba_tile.data_store.mget(address));
            }  
            Word_List var=value_list[0];
            Word var_name=var[0];
            result=var_name;
             switch (service) {
             case A_ASSIGN  :
             {
                MemAddress sym_address=addresses[0];
                MemAddress data_address=addresses[1];
#ifdef VERBOSE
                cout << "ASSIGN addresses: " <<sym_address<< " => " <<data_address<< ""<<endl;
#endif // VERBOSE
                Word var_label=parent.symbol_table[sym_address];
                Name_t var_name=getName(var_label);
                bool has_label=false;
                MemAddress var_address=0;
                
                if (sba_tile.lookup_table.count(var_name)==1){
                    Word word = sba_tile.lookup_table.read(var_name);
                    has_label=true;
                    var_address= getSubtask(word);
                }                    
                if (has_label==false){
                    var_address=parent.data_address_stack.pop();
                    Word word=setSubtask(var_label,var_address);
                    word=setStatus(word,DS_present);
#ifdef VERBOSE
                    cout << "ASSIGN: STS=" <<parent.subtask_list.status(parent.current_subtask)<< ""<<endl;
#endif // VERBOSE
                    sba_tile.lookup_table.write(getName(word),word);
                    Word_List var_value=sba_tile.data_store.mget(data_address);
#ifdef VERBOSE
                    cout << "ASSIGN: storing" << "\n" << ppPayload(var_value) << "\n " <<var_address<< ""<<endl;
#endif // VERBOSE
                    sba_tile.data_store.mput(var_address,var_value);
                    result=word;
#ifdef VERBOSE
                } else {
                    cout << " [WARNING: overwriting <" <<var_label<< "> (" << "\n" << ppPayload(sba_tile.data_store.mget(var_address)) << "\n)] "<<endl;
#endif // VERBOSE
                }
                 Word_List result_list; result_list.push_back(result); return result_list;
              break;
             }
             case A_UPDATE  :
             {
                MemAddress sym_address=addresses[0];
                Word var_label=parent.symbol_table[sym_address];
                var_name=getName(var_label);
                
                bool has_label=false;
                MemAddress var_address=0;
                
                if (sba_tile.lookup_table.count(var_name)==1){
                    Word word=sba_tile.lookup_table.read(var_name);
                    has_label=true;
                    var_address= getSubtask(word);
                }
                
                Word_List result_list;
                if (has_label==true){
                    MemAddress newval_address=addresses[1];
                    Word_List newval= sba_tile.data_store.mget(newval_address);
                    sba_tile.data_store.mput(var_address,newval);
                    result_list=newval;
                } else {
                    parent.subtask_list.status(parent.current_subtask,STS_pending);
                    parent.core_status=CS_managed;
                }
                return result_list;
              break;
             }
             case A_READ  :
             {
                MemAddress sym_address=addresses[0];
                Word var_label=parent.symbol_table[sym_address];
                var_name=getName(var_label);
                bool has_label=false;
                MemAddress var_address=0;
                
                if (sba_tile.lookup_table.count(var_name)==1){
                    Word word=sba_tile.lookup_table.read(var_name);
                    has_label=true;
                    var_address= getSubtask(word);
                }                



                
                Word_List   result_list;
                if (has_label==true){
                    Word_List var_value=sba_tile.data_store.mget(var_address);
                    result_list=var_value;
#ifdef VERBOSE
                    cout << "READ: returning" << "\n" << ppPayload(result_list)<<endl;
#endif // VERBOSE
                } else {
                    parent.subtask_list.status(parent.current_subtask,STS_pending);
                    parent.core_status=CS_managed;
                }
                
                return result_list;


        break;}
        default: std::cerr<< "Service "<<service<< " has no implementation\n";exit(0);
            } // of non-LET services case block
           
#ifdef VERBOSE
            cout <<  ") => ";
            cout << ppSymbol(result) << "\n";
#endif // VERBOSE
            parent.core_return_type=P_data;
            parent.subtask_list.status(parent.current_subtask,STS_cleanup);
              Word_List result_list; result_list.push_back(result); return result_list;
        } // of not SC_LET
    } // of ls_LET        
    
    
#if 0 // SKIP ls_LAMBDA

 Word_List SCLib::ls_LAMBDA(void* parent_ptr, MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

#ifdef VERBOSE
        cout << "LAMBDA CORE (" <<parent.current_subtask<< "): \n";
#endif // VERBOSE
        Word_List result;
        Word_List value_list;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
                Word tval=sba_tile.data_store.mget(address)[0];
                value_list.push_back(tval);
        }
        for(Word_List::iterator iter_=value_list.begin();iter_!=value_list.end();iter_++) {
        	Word value=*iter_;
            result.push_back(value);
        }
#ifdef VERBOSE
        cout << "\nLAMBDA CORE (" <<parent.current_subtask<< "): result:" << "\n" << ppPayload(result)<<endl;
#endif // VERBOSE
        return result;
    } // of LAMBDA




#endif // 0
#if 0 // SKIP ls_APPLY

 Word_List SCLib::ls_APPLY(void* parent_ptr, MemAddresses& addresses)  {
                // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

        Word   service_word=parent.subtask_list.called_as(parent.current_subtask);
        uint       nargs=parent.n_args;
    	Name_t   service=getName(service_word);
    	 bool use_redir; bool use_unique;
        if (service==SC_APPLY){
#ifdef VERBOSE
            cout << "APPLY CORE (" <<parent.current_subtask<< "): \n";
#endif // VERBOSE
            use_redir=true;
            use_unique=true;
        } else if (service==A_APPLYTC){
#ifdef VERBOSE
            cout << "APPLY TAILCALL"<<endl;
#endif // VERBOSE
            use_redir=false;
            use_unique=false;
        } else {
            cerr << "WRONG ALIAS FOR APPLY: " <<service<< "";
            exit(1);
        }
         if (parent.subtask_list.waiting_for_ack(parent.current_subtask)==1){
            if (parent.lookup_table.count(parent.current_subtask) ){
                Word lref=parent.lookup_table.read(parent.current_subtask);
#ifdef VERBOSE
                cout << "APPLY: remove " <<lref<< " for " <<parent.current_subtask<< ""<<endl;
#endif // VERBOSE
                parent.lookup_table.erase(parent.current_subtask);
                parent.lookup_table.erase(lref);
            }
            parent.ack_ok=1;
            parent.core_status=CS_managed;
#ifdef VERBOSE
            cout << "WARNING: core_status=CS_managed; returning empty list"<<endl;
#endif // VERBOSE
             Word_List empty_list; return empty_list;
        } else {
            bool unique=true;
            Word result=0;
            Word_List  lambda_function_args;
    
    
            MemAddress lambda_def_address=addresses[0];
                Word lambda_label=parent.symbol_table[lambda_def_address];
#ifdef VERBOSE
                Kind_t data_kind=getKind(lambda_label);
                Task_t  data_status=getTask(lambda_label);
#endif // VERBOSE
                    Word_List    lambda_function=sba_tile.data_store.mget(lambda_def_address);
#ifdef VERBOSE
                    cout << "LAMBDA FUNCTION:" << "\n" << ppPayload(lambda_function)<<endl;
                    cout << "LEN: " <<lambda_function.size()<< ""<<endl;
#endif // VERBOSE
                    if (lambda_function.size()>1   ){
                        uint ext=0;
                        for(Word_List::iterator iter_=lambda_function.begin();iter_!=lambda_function.end();iter_++) {
                        	Word itemw=*iter_;
                             if (ext==0) {
                            ext=getExt(itemw);
                            if (getKind(itemw) == K_S ){
                                cerr << "APPLY CORE: ERROR: LAMBDA definition list args can only be U or R";
                                exit(1);
                            }
                            if (getKind(itemw) == K_U){
#ifdef VERBOSE
                                cout << "ARG: " << itemw << "\n";
#endif // VERBOSE
                                lambda_function_args.push_back(itemw);
                            } else if (getKind(itemw)==K_R){
#ifdef VERBOSE
                                cout << "REF: " << itemw << "\n";
#endif // VERBOSE
                                ext=0;
                                if (parent.lookup_table.count(itemw)==0){

#ifdef VERBOSE
                                    cout << "APPLY: activating code " <<itemw<< " for " <<parent.current_subtask<< ""<<endl;
#endif // VERBOSE
                                    parent.lookup_table.write(parent.current_subtask,itemw);
                                    parent.lookup_table.write(itemw,parent.current_subtask);
                                } else {
                                    unique=false;
                                }
                                break;
                            }
                             } // do "next"
                        }
                    }                      
                    
                    if (use_unique and not unique ){
                        Word_List   result_list;
#ifdef VERBOSE
                        cout << "APPLY CORE: DEFER: task is not unique, setting subtask <" <<parent.current_subtask<< "> to " <<STS_pending<< ""<<endl;
#endif // VERBOSE
                        parent.subtask_list.status(parent.current_subtask,STS_pending);
                        parent.core_status=CS_managed;
                        return result_list;
                    } 
#ifdef VERBOSE
                 int pp_data_kind=(int)data_kind;
                 int pp_data_status=(int)data_status;
                cout <<  "APPLY CORE: " <<parent.current_subtask<< ": CALLER ARG: (" <<pp_data_kind<< ") " <<lambda_label<< "=>" <<lambda_def_address<< "=> <" <<pp_data_status<< ">\n";
                cout <<  "APPLY CORE: " <<parent.current_subtask<< ": CALLER VAL: " << ppPayload(sba_tile.data_store.mget(lambda_def_address)) << "\n";
#endif // VERBOSE
            
            
#ifdef VERBOSE
            cout << "APPLY CORE: " << "\nREWRITING R-code\n";
#endif // VERBOSE
            uint root_ref=1;
            for(Word_List::iterator iter_=lambda_function.begin();iter_!=lambda_function.end();iter_++) {
            	Word ref_symbol_word=*iter_;
                 
                 if(getKind(ref_symbol_word)==K_R) {
                
                if (root_ref==1 ){
                    result=ref_symbol_word;
                    root_ref=0;
                }
#ifdef VERBOSE
                cout << "APPLY CORE: " << "code for " << ref_symbol_word << "\n";
#endif // VERBOSE
                
                CodeAddress lambda_function_definition_address=getCodeAddress(ref_symbol_word);

#ifdef VERBOSE
                cout << "APPLY CORE: CODE ADDRESS: " <<lambda_function_definition_address<< ""<<endl;
#endif // VERBOSE
                
                Word_List lambda_function_definition=sba_tile.code_store.mget(lambda_function_definition_address);

#ifdef VERBOSE
                cout << "LAMBDA FUNCTION PACKET PAYLOAD:" << "\n" << ppPayload(lambda_function_definition)<<endl;
#endif // VERBOSE

 
                
                Word_List  appl_function;
                
                uint ext=0;
                uint ii=0;
#ifdef VERBOSE
                    cout << "LAMBDA FUNCTION LEN: " <<lambda_function_definition.size()<< ""<<endl;
#endif // VERBOSE
                for(uint i=0;i<=lambda_function_definition.size()-1 ;i++) {
                    Word symbol_word=lambda_function_definition[i];
                    if (ext==0){
                        if ( getKind(symbol_word)!=K_U){
                            appl_function.push(symbol_word);
#ifdef VERBOSE
                            cout << "X:appl_function[" <<ii<< "]: " <<ppSymbol(symbol_word)<< ""<<endl;
#endif // VERBOSE
                        } else {      
                            for(uint j=0;j<=lambda_function_args.size()-1 ;j++) {
                                if (setQuoted(symbol_word,0) == setQuoted(lambda_function_args[j],0)){
                                    uint first=1;

                                    MemAddress data_address=addresses[j+1];
                                    Word data_label=parent.symbol_table[data_address];
                                    Word_List                                     symbol_word_list=sba_tile.data_store.mget(data_address);
                                    for(Word_List::iterator iter_=symbol_word_list.begin();iter_!=symbol_word_list.end();iter_++) {
                                    	Word val_word=*iter_;

                                        if (first==1){
                                            Word newsym=setQuoted(val_word,getQuoted(symbol_word)|getQuoted(val_word));
                                            appl_function.push(newsym);
                                            first=0;
                                        } else {
                                            appl_function.push(val_word);
                                        }                             
#ifdef VERBOSE
                                        cout << "U:appl_function[" <<ii<< "]: " <<appl_function[appl_function.size()-1]<< ""<<endl;
#endif // VERBOSE
                                        ii=ii+1;
                                    }
                                    ii=ii-1;
                                } 
                            }
                        }                                   
                        if (getExt(symbol_word)==1 and (getKind(symbol_word)==K_B or getKind(symbol_word)==K_Q) ){
                            ext=getSubtask(symbol_word);
#ifdef VERBOSE
                            cout << "EXT: " <<ppSymbol(symbol_word)<< " => ext=" <<ext<< ""<<endl;
#endif // VERBOSE
                             ii=ii-1;
                        }      
                    } else { 
                        ext-=1;
                        ii=ii+1;
                        appl_function.push(lambda_function_definition[i]);
#ifdef VERBOSE
                        cout << "E(" <<ext<< "):appl_function[" <<ii<< "]: " <<ppSymbol(appl_function[appl_function.size()-1])<< ""<<endl;
#endif // VERBOSE
                    }                      
                    ii=ii+1;
                } 
                uint plength=appl_function.size();
                Header_t code_packet_header = mkHeader(P_code,0,0,plength,0,0,0,0);
                code_packet_header=setTo(code_packet_header,getName(ref_symbol_word));
                code_packet_header=setReturn_as(code_packet_header,ref_symbol_word);
                Word_List code_packet_payload=appl_function;
                Packet_t code_packet = mkPacket(code_packet_header,code_packet_payload);
#ifdef VERBOSE
                cout << "APPLY CORE: CODE PACKET" << "\n" << ppPacket(code_packet)<<endl;
#endif // VERBOSE
                parent.transceiver_tx_fifo.push(code_packet);
                 } // to emulate next
            }  // of for               
            Symbol_t result_symbol = mkSymbol(K_B,0,0,1,0,0,0);
            
            if (use_redir){
#ifdef VERBOSE
                        cout << "APPLY CORE REDIR/wait for ACK"<<endl;
#endif // VERBOSE
                        Word send_ack_to=setName(lambda_label,S_APPLY);
                        send_ack_to=setSubtask(send_ack_to,lambda_def_address);
                        parent.ack_ok=0;
                        parent.symbol_table[lambda_def_address]=setStatus(lambda_label,DS_requested);
                        parent.subtask_list.status(parent.current_subtask,STS_blocked);
                        parent.subtask_list.incr_nargs_absent(parent.current_subtask);
                        parent.subtask_list.waiting_for_ack(parent.current_subtask,1);
                        parent.core_status=CS_managed;
                        Packet_Type packet_type=P_reference;
                        Prio_t prio=0;
                        Length_t payload_length=1;
                         Word_List result_list;result_list.push_back(result);
                        Name_t to=getName(result);
                        Return_to_t return_to=parent.subtask_list.return_to(parent.current_subtask);
                        Word return_as=parent.subtask_list.return_as(parent.current_subtask);
                        send_ack_to=setName(lambda_label,S_APPLY);
                        send_ack_to=setSubtask(send_ack_to,lambda_def_address);
                        Header_t ref_packet_header = mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as);
                        Word_List  ref_packet_payload=result_list;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
#ifdef VERBOSE
                        cout << ppPacket(ref_packet)<<endl;
#endif // VERBOSE
                        if (to!=S_APPLY){
                            parent.transceiver_tx_fifo.push(ref_packet);
                        } else {
                            parent.subtask_reference_fifo.push(ref_packet);
                        }
                        result_symbol=setSubtask(result_symbol,1);
            }            

            if (not use_redir                                      ){
           
                parent.core_return_type= P_reference;
                result_symbol=result;
                parent.subtask_list.to(parent.current_subtask,getName(result_symbol));

            }   
          
        
             Word_List result_symbol_l;result_symbol_l.push_back(result_symbol);return result_symbol_l;
        } 
    } // of ls_APPLY




#endif // 0

 Word_List SCLib::ls_IO(void* parent_ptr,MemAddresses& addresses)  {
            // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

        Word         service_word=parent.subtask_list.called_as(parent.current_subtask);
    	Name_t service=getName(service_word);
        parent.core_return_type=P_data;
        MemAddress port_address=addresses[0];
        Word_List port_symbol=sba_tile.data_store.mget(port_address);
        Word builtin_symbol=port_symbol[0];
        Word port = port_symbol[1];
         Word_List eof; eof.push_back(builtin_symbol); eof.push_back((Word)(0));      
         Word_List pass; pass.push_back(builtin_symbol); pass.push_back((Word)(1));
#if WORDSZ==64
         Word min1=(Uint64)(-1);        
#else // WORDSZ==32
         Word min1=(Uint32)(-1);
#endif // WORDSZ 
    Word_List fail; fail.push_back(builtin_symbol); fail.push_back(min1);
    FILE* fd;
        if (service==A_FOPEN){
            MemAddress filename_address=addresses[1];
            Word_List filename_bytes=sba_tile.data_store.mget(filename_address);
            string filename=sym2str(filename_bytes);
#ifdef VERBOSE
            cout << "CORE FOPEN: FILENAME: " <<filename<< ""<<endl;
#endif // VERBOSE
            
            if (addresses.size()==3){
                MemAddress flag_address=addresses[2];
                string flag=sym2str(sba_tile.data_store.mget(flag_address));
                 fd=fopen(filename.c_str(),flag.c_str());
            } else {
                 fd=fopen(filename.c_str(),"r");
            }
             parent.lookup_table.write(port,(Uint64)fd);
            return port_symbol;
        } else if (service==A_FCLOSE){
            if (parent.lookup_table.count(port)){
            	 fd=(FILE*)(parent.lookup_table.read(port));
            	 fclose(fd);
            	return pass;
            } else {
            	return fail;
            }        
        } else if (service==A_IOREAD ){
            MemAddress nbytes_address=addresses[1];
            Word nbytes=sba_tile.data_store.mget(nbytes_address)[1];
            if (nbytes!=0 ){
                std::cerr <<  "CORE IOREAD: reading chunks of " <<nbytes<< " bytes is not yet implemented";
            } 
             char* nil=NULL;
             char inp[255]; // the 255 is very ad-hoc
            if ((not parent.lookup_table.count(port))){
            	 std::cerr << port << ": ";
                 fgets(inp,255,stdin);
            } else {
                 fd=(FILE*)(parent.lookup_table.read(port));
                if (not feof(fd)  ){                     fgets(inp,255,fd);
                }
            }
             
            if (inp!=nil){
                Word_List inp_sym = string2symbol(inp);
                return inp_sym;
            } else {
#if WORDSZ==64
                Symbol_t Word emptyK_B = mkSymbol(K_B,T_i,1,1,0,0,0);
#else // WORDSZ==32
                 Word emptyK_B =0xCC000000UL;                                  
#endif // WORDSZ
                 Word_List emptysymbol;emptysymbol.push_back(emptyK_B);
                return emptysymbol;
            }
        } else if (service==A_IOWRITE){
            MemAddress        data_address=addresses[1];
            Word_List data_symbol = sba_tile.data_store.mget(data_address);
             string data;            
            Kind_t kind=getKind(data_symbol[0]);
            Datatype_t datatype=getDatatype(data_symbol[0]);
            if ((kind==K_B and getExt(data_symbol[0])==1 and getSubtask(data_symbol[0])==0) ){
                return eof;
            } else {
                 if ((not parent.lookup_table.count(port))){
                     return fail;
                 } else {            
                      FILE* fd=(FILE*)(parent.lookup_table.read(port));
                    if (datatype==(T_s&1) and kind==K_Q){
                        string data=sym2str(data_symbol);
                         fprintf(fd,"%s",data.c_str());
                    } else if ( datatype==T_i){
                        Int            data=sym2int(data_symbol);
                         fprintf(fd,"%d",data);
                    } else if ( datatype==T_f){
                        Float             data=sym2flt(data_symbol);
                         fprintf(fd,"%f",data);
                    } else if ( datatype==(T_b&1) and kind==K_Q ){
                        bool            data=sym2bool(data_symbol);
                         fprintf(fd,"%u",data);
                    } else {
                        cerr << "CORE IOWRITE: datatype " <<datatype<< "";
                        exit(1);
                    }                
                     return pass;
                 }
             }
        } else if (service==A_DISPLAY){
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
                Word_List data=sba_tile.data_store.mget(address);
#ifndef STATIC_ALLOC
                 	for(Word_List::iterator ai=data.begin(); ai!=data.end(); ++ai) {
                 		std::cout <<">>>"<< *ai << "\n";
#else
                 	for(unsigned int ai=0;ai<data.size(); ++ai) {
                 	std::cout <<">>>"<< data.back() << "\n";
                 	data.pop_back();
#endif                 		
                 		}
                 	std::cout << "----------------\n";
            }    
            return pass;
        } else {
            cerr << "CORE StreamIO does not support " <<service<< "";
            exit(1);
        }
    } // of IO
   


#if 0 // SKIP cs_DCT

 Word_List SCLib::cs_DCT(void* parent_ptr,MemAddresses& addresses)  {
                // Set up context
	SC_ServiceCore* servicecore_ptr=(SC_ServiceCore*)parent_ptr;
	SC_ServiceCore& parent=*servicecore_ptr;
	SC_ServiceCore& sba_tile=parent;	    

            
             Word_List result_list;
        
    
        MemAddress address=addresses[0];
        Word nruns=sba_tile.data_store.mget(address)[1];
        Word res_symbol=sba_tile.data_store.mget(address)[0];
            
     result_list.push_back(res_symbol);
#ifdef VERBOSE
        cout << "DCT: " <<nruns<< " runs"<<endl;
#endif // VERBOSE
     SCLib::calc_dct(nruns);
#ifdef VERBOSE
                    cout << "DCT: done"<<endl;
#endif // VERBOSE
             result_list.push_back(nruns);
            return result_list;
            
        }


#endif // 0

Result SCLib::none(void* parent,MemAddresses& addresses)  {
     Result res; res.push_back((Word)0); return res;
}        
        
#endif // NO_SERVICES

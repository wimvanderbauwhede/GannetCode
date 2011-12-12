# SBACore.rb
#   
# :title: Gannet Service-based SoC project - Service Core Library 
#    
#--
#
# *
# *  (c) 2004-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#++

# -----------------------------------------------------------------------------

=begin #inc
#include <dlfcn.h>
#include <fstream>
#include <sstream>
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/ServiceCore.h" //skipcc
#include "System.h" //skiph
#include "Tile.h" //skiph
#include "ServiceCore.h" //skiph
#include "SBACore.h"
=end #inc 

=begin
# This module contains the implementations of all SBA services. 
# Services starting with 'ls_' are language services
=end

require "SBA/Packet.rb"

# module SBA_SCLib
# no more _SCLib

#C++ using namespace std;

#skipcc
#C++ namespace SBA {
#C++ namespace SBACore {
#endskipcc

#skiph
#C++ using namespace SBA; 
#endskiph
class SBACore  #skip
  include SBA #skip
    def initialize(verbose) #skip
        @v=(verbose==1) #skip
    end #skip
#ifndef NO_SERVICES    
if WORDSZ==64    
    # Helper functions for FPU
    def word2dbl(result) #t double (Word)
        dbl=w #skip
=begin #C++        
        u_int64_t* result_p=&result;
        void* tmpd=(void*)result_p;
        double* tmp_dbl_p = (double*) tmpd;
        double dbl_result=*tmp_dbl_p;
=end #C++        
        return dbl_result;
    end
        
    def dbl2word(dbl) #t Word (double)
        w=dbl #skip
=begin #C++         
        double* dbl_p=&dbl;
        void* v_p=(void*)dbl_p;
        Word* w_p=(Word*)v_p;
        Word w= *w_p;
=end #C++ 
        return w;        
    end
else # WORDSZ==32   
     def word2flt( result) #t float (Word)
        flt_result=result #skip 
=begin #C++        
        u_int32_t* result_p=&result;
        void* tmpf=(void*)result_p;
        float* tmp_flt_p = (float*) tmpf;
        float flt_result=*tmp_flt_p;
=end #C++        
        return flt_result;
     end    

    def flt2word( flt) #t Word (float)
        w=flt #skip
=begin #C++        
        float* flt_p=&flt;
        void* v_p=(void*)flt_p;
        Word* w_p=(Word*)v_p;
        Word w= *w_p;
=end #C++        
        return w;
    end
end # WORDSZ    

=begin
=end
    

    # --------------------------------------------------------------------------
    # Helper functions for IO
    # --------------------------------------------------------------------------
    def string2symbol(str) #t Word_List (string)
    # pad str with null bytes
        null=[0].pack("c") #skip
        npad=NBYTES - (str.length % NBYTES) #t uint 
        nwords=(str.length+npad)/NBYTES #t uint
        str=str+null * npad #C++ str.resize(NBYTES*nwords,0);
        sheader=mkSymbol(K_B,T_s,1,1,0,nwords,npad)
=begin #C++
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
=end #C++

=begin
=end
    
#skip
if WORDSZ==64
        # 8 bytes, we don't do Unicode
        wordstr=str.unpack("Q"*nwords)      
else # WORDSZ==32
        # 4 bytes, idem
        wordstr=str.unpack("N"*nwords) 
end # WORDSZ  
# puts wordstr.inspect 
        sym=[sheader]+wordstr 
#endskip
        return sym
    end

    def sym2str(sym) #t string (Word_List)
        header=sym.shift #C++ Word header=sym.front();sym.pop_front();
        nwords=getSubtask(header) #t uint
        padding=getName(header) #t uint

=begin #C++
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
=end #C++               
        

=begin
=end
    
#skip        
if WORDSZ==64
        # 8 bytes, we don't do Unicode
        str=sym.pack("Q"*nwords)       
else # WORDSZ==32
        # 4 bytes, idem
        str=sym.pack("N"*nwords)
end # WORDSZ       
        # here we should remove any NULL bytes from the string. The problem is that we don't know how many
        # so I have to scan the string from the back
        if padding>0
            str=str[0..str.length-1-padding]
        end
#endskip        
        return str
    end

    def sym2int(sym) #t Int (Word_List)
        return getInt(sym)
    end
    
    def sym2uint(sym) #t Word (Word_List)
        return getUInt(sym)
    end
    
    def sym2bool(sym) #t bool (Word_List)
        return getUInt(sym)==1
    end
                    
    def sym2flt(sym) #t float (Word_List)
        header=sym.shift #skip
        raise "Float Ext length must be 1!" unless getSubtask(header)==1 #skip
        result=sym.shift #C++ Word result=sym.front();sym.pop_front();
        if WORDSZ==64
            flt_result=[result].pack("Q").unpack("G")[0] #C++ double flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
        else # WORDSZ==32
            flt_result=[result].pack("N").unpack("g")[0] #C++ float flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
        end # WORDSZ
        return flt_result 
    end
#endif // NO_SERVICES        

# ============================== Actual Service Cores ==============================

#    # this is a dummy to make sure the GATEWAY is registered
#    def sba_GATEWAY(sba_tile,parent) #t Result (na;Base::ServiceCore*)
#        return 1 #C++ Result res; res.push_back((Word)1); return res;
#    end



#ifndef NO_SERVICES    
    # --------------------------------------------------------------------------

    # Helper for ALU integer division
    def div(m,n) #t Int (Int;Int)
        q=0 #t Int
        # In case of pure integer arithmetic, division should be like this:
        # q = m / n
        sm=1 #t Int        
        if m<0
            sm=-1
        end        
        sn=1 #t Int
        if n<0
            sn=-1
        end        
        um=m*sm #t Int          
        un=n*sn #t Int    
        modmn= um % un #t Int
        q=(um-modmn)/un
        if 2*modmn>un
            q+=1
        end  
        return q*sn*sm
    end
    
    # --------------------------------------------------------------------------
    # To make the ALU type-aware (int or float), we need to get the types of the arguments.
    # So we need the labels of the arguments
    # But this is a silly approach: we should simply have an FPU separately!
    
    def ls_ALU(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
        #core
#iv        
        puts "ALU CORE: processing subtask #{parent.current_subtask}"
#ev
        #C++ Word_List result_list;
# FIXME: compiler reserves 0 opcode, runtime doesn't!
        operation=parent.method() #t Uint

#iv        
        puts "ALU (#{parent.service}) CORE: #{parent.nargs()} addresses"
#ev         	
        words=parent.arg(0) #t Word_List
        res_symbol=words[0] #t Word
        int_result=getInt(words) #t Int       
        result=Integer(int_result) #C++ Word result=int_result;
#iv
        puts "ALU CORE: arg 1: Found int #{result} (#{T_i}) @ #{parent.addr(0)}"   
#ev                
        n_args=parent.nargs() #t uint
        if operation==M_SBACore_ALU_not
            result=1-result
        else
            ii=0; #t int            
            for argn in 0..n_args-1 #t uint
#                if argn==n_args
#                    break
#                end    
                ii+=1
                if ii>1
                twords=parent.arg(argn) #t Word_List
                tres_symbol=twords[0] #t Word
                int_tres=getInt(twords) #t Int       
                tres=Integer(int_tres) #C++ Word tres=int_tres;                    
#iv
                        puts "ALU CORE: arg #{ii}: Found int #{tres} (#{T_i}) @ #{parent.addr(argn)}"   
#ev                        
                case operation
                when M_SBACore_ALU_plus
                    puts "ALU CORE operation: +" if @v #skip
                    result=result+tres #C++ int_result+=int_tres;                    
                when M_SBACore_ALU_minus
                    puts "ALU CORE operation: -" if @v #skip
                    result=result-tres #C++ int_result-=int_tres; 
                when M_SBACore_ALU_times  
                    puts "ALU CORE operation: *" if @v #skip 
                    result=result*tres #C++ int_result*=int_tres;
                when M_SBACore_ALU_over
                    puts "ALU CORE operation: /" if @v #skip
                    result=div(result,tres) #C++ int_result=div(int_result,int_tres);
                    # result=result/tres #/
                when M_SBACore_ALU_lt
                    puts "ALU CORE operation: <" if @v #skip
                    result=(result<tres)?1:0 #C++ int_result=(int_result<int_tres)?1:0;
                when M_SBACore_ALU_gt
                    puts "ALU CORE operation: >" if @v #skip
                    result=(result>tres)?1:0 #C++ int_result=(int_result>int_tres)?1:0;
                when M_SBACore_ALU_eq
                    puts "ALU CORE operation: ==" if @v #skip
                    result=(result==tres)?1:0 #C++ int_result=(int_result==int_tres)?1:0;
                    #C++ break;}
                else #C++ default:
                    raise "Unknown ALU CORE service: #{operation}" 
                    #C++   exit(0);
                end #;
            end
            end
        end
        puts "ALU CORE RESULT (signed int): #{result}" if @v #skip      
        result=Integer((result<0)?(2**WORDSZ+result):result) #C++ result=(Uint)int_result;                
        #iv    
        puts "ALU CORE RESULT: (uint#{WORDSZ}) #{result}" 
        puts "ALU (#{parent.service}) CORE (#{parent.current_subtask}):  result: #{result}"
        #ev
        one=1 #t Word
        if result>((one<< FB_Value)-1)
            res_symbol=setExt(res_symbol,1) 
            res_symbol=setNSymbols(res_symbol,1)
            result_list=[res_symbol,result]  #C++ result_list.push_back(res_symbol);result_list.push_back(result);
        else
            res_symbol=setExt(res_symbol,0)
            res_symbol=setValue(res_symbol,result)
            result_list=[res_symbol]  #C++ result_list.push_back(res_symbol);            
        end    
        parent.result(result_list)
    end # of ALU
# ----------------------------------------------------------------------------------------------------
# Five years to see the light :-( 
# Here's the FPU -- Ruby only
            
def ls_FPU(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
    #core
    
    #iv        
        puts "FPU CORE: processing subtask #{parent.current_subtask}"
    #ev
                    
    #C++ Word_List result_list;
    
        return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
        operation=parent.opcode #t Uint
    #iv        
        puts "FPU (#{parent.service}) CORE: #{parent.nargs()} addresses"
    #ev     
        
        words=parent.arg(0) #t Word_List
        res_symbol=words[0] #t Word    
    #C++ result_list.push_back(res_symbol);
        result=getFloat(words) #t double
        puts "FPU CORE: Found double #{result} (#{getDatatype(res_symbol)}<>#{T_i})" if @v  #skip
    
        if operation==M_SBACore_FPU_not
            result=1.0-result
        else
            ii=0; #t int
            n_args=parent.nargs() #t uint
            for argn in 1..n_args-1 #t uint
                ii+=1
                if ii>1
                    twords=parent.arg(argn) #t Word_List
                    tres=getFloat(words) #t double
                    
                    case operation
                    when M_SBACore_FPU_plus
                        puts "FPU CORE operation: +" if @v #skip
                        result=result+tres #C++ result+=tres;                    
                    when M_SBACore_FPU_minus
                        puts "FPU CORE operation: -" if @v #skip
                        result=result-tres #C++ result-=tres; 
                    when M_SBACore_FPU_times  
                        puts "FPU CORE operation: *" if @v #skip 
                        result=result*tres #C++ result*=tres;
                    when M_SBACore_FPU_over
                        puts "FPU CORE operation: /" if @v #skip
                        result=result/tres #C++ result=result/tres;
                    when M_SBACore_FPU_lt
                        puts "FPU CORE operation: <" if @v #skip
                        result=(result<tres)?1:0 #C++ result=(result<tres)?1:0;
                    when M_SBACore_FPU_gt
                        puts "FPU CORE operation: >" if @v #skip
                        result=(result>tres)?1:0 #C++ result=(result>tres)?1:0;
                    when M_SBACore_FPU_eq
                        puts "FPU CORE operation: ==" if @v #skip
                        result=(result==tres)?1:0 #C++ result=(result==tres)?1:0;
                        #C++ break;}
                    else #C++ default:
                        raise "Unknown FPU CORE service: #{operation}" 
                        #C++   exit(0);
                    end #;
                end
            end
        end
            #iv
            puts "FPU CORE RESULT (double): #{result}"  if @v #skip      
            #ev
                        
if WORDSZ==64               
             uint64_result=[result].pack("G").unpack("Q")[0] #skip
             result=uint64_result #C++ Word resultword=(Word)&result;
             
else # WORDSZ==32
            uint32_result=[result].pack("g").unpack("N")[0] #skip
            result=uint32_result #C++ float flt_result=(float)result; Word resultword=(Word)&flt_result;
end # WORDSZ
    #iv    
    puts "FPU CORE RESULT: (uint#{WORDSZ}) #{result}" 
    puts "FPU (#{parent.service}) CORE (#{parent.current_subtask}):  result: #{result}"
    #ev

    result_list=[res_symbol,result]  #C++ result_list.push_back(resultword);
    
    parent.result(result_list)
        
end # of ls_FPU    
        
 

# --------------------------------------------------------------------------    
    
    #--
    # We need at least following "abstract" services:
    # --------------------------------------------------------------------------
    #++
    
    # The BEGIN core takes in all arguments returns the last result 
# I want to support sequencing in BEGIN. Maybe I could create a SEQ service for this very purpose
# A non-scoping seq block would be SEQ, an non-scoping par block would be BEGIN
    def ls_BEGIN(sba_tile,parent) #t void (na;Base::ServiceCore*)  #s/parent/parent_ptr/ 
        #core
        result=[] #C++ Word_List result;
        n_args=parent.nargs() #t uint        
        result=parent.arg(n_args-1)         
        #iv
		if @v #skip
        puts "#{parent.service} CORE: Passing on #{result}" #C++ cout << parent.service<< " CORE: Passing on result\n";
		end #skip
        #ev
        parent.result(result)
    end # of BEGIN
    
    
    # --------------------------------------------------------------------------
    def ls_IF(sba_tile,parent) #t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
    parent.ack_ok=1
    if sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask)==1
        # OK, we received an ACK, time to clean up
        # The problem is that the core must return something. But no other service is expecting a packet. So that's useless.
        # So I'll use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
#        raise "Got ACK"
        puts "IF CORE: got ACK" if @v #skip
        
        parent.ack_ok=1 # we're not redirecting so we can ack
        parent.core_status=CS_managed
        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processed)
        puts "WARNING: core_status=CS_managed; returning empty list" if @v #skip
        parent.result( [] ) #C++ Word_List empty_list; parent.result(empty_list);
    else
        service=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
        opcode=getOpcode(service) #t uint
        scid=getSCId(service) #t uint
        operation =opcode+(scid<< FS_SCId) #t uint  
#        puts "OP:",operation,  M_SBACore_IF_return,M_SBACore_IF_returntc,M_SBACore_IF_if
        valn=0 #t uint 
        if operation==M_SBACore_IF_return or operation==M_SBACore_IF_returntc
            #iv
            print "IF CORE: #{parent.current_subtask}: (RETURN \n"
            #ev
            valn=0

        else # must be IF
            #iv
            print "IF CORE: #{parent.current_subtask}: (IF \n"
            #ev
            condval_wl=parent.arg(0) #t Word_List
            condval=condval_wl[0]&1 #t int                
            if condval>0
                condval=1
            else
                condval=0
            end
            #iv
            puts "IF CORE: CONDVAL:#{condval}"
            puts "IF CORE: LABEL1:<#{parent.addr(1)}>"
            puts "IF CORE: LABEL2:<#{parent.addr(2)}>"
            #ev
            label='Error' #skip
            if condval==1
                valn=1
            elsif condval==0
                valn=2
            end
           
        end # RETURN or IF
        if parent.nargs()>0
            valaddress=parent.addr(valn) #t MemAddress
            result_list=parent.arg(valn) #t Word_List
            result=result_list[0] #t Word
            puts ppSymbol(result) if @v #skip        
            label=sba_tile.service_manager.symbol_table[valaddress] #t Word
            #iv
            puts "IF CORE: LABEL:<#{label}>"         
            print "IF CORE: KIND <"
            print getKind(label) #C++ cout <<(int)getKind(label);
            puts ">"
            #ev
            labelquoted=getQuoted(label) #t int
            if labelquoted==1
                # It's quoted, so it's an expression, variable, data or quoted expression.    
=begin            
    Since we now don't unquote on store, refs are always quoted. We rely on the label to tell us what is stored    
=end            
                if getKind(result) == K_S or getKind(result) == K_A
                    # If it's an expression => Error
                    raise "IF CORE: ERROR: IF arg can't be #{getKind(result)}"
                elsif getKind(result) == K_L or getKind(result) == K_D
                    # If it's a variable or data => request & redirect result
                    # i.e. create a request packet
                    parent.core_return_type= P_request                               
                    sba_tile.service_manager.subtask_list.to(parent.current_subtask,getKind(result))                
                elsif getKind(result) == K_B # or getKind(result) == K_Q                         
                    puts "BUILTIN"  if @v #skip
                    puts parent.ack_ok if @v #skip
                    # If it's a quoted expression =>  Just return it
                    parent.core_return_type=P_data                                
                elsif getKind(result) == K_R   
#                        puts "IF CORE REDIR"  if @v #skip
                    # CORE decides to redirect
                    # Now what? I thought the IF core _always_ redirects. ack_to is the label for the ACK
                    # So it contains the name of the IF service and the address of the argument, i.e valaddress
                    # See parse_subtask_packet, it's the same code
                    
                    # WV21082008: We need IFTC and RETURNTC: there's no need for an ACK in a tail-called loop
                    # as there is nothing to clean up anyway
                    send_ack_to=0 #t Word
                    if operation==M_SBACore_IF_return or operation==M_SBACore_IF_if
                        puts "IF CORE wait for ACK"  if @v #skip
                        send_ack_to=setName(label,S_SBACore_IF) 
                        send_ack_to=setSubtask(send_ack_to,valaddress)
                        parent.ack_ok=0 # If a core redirects, it doesn't send an ACK
                    
                        # Now fool the subtask:
                        # set status of argument to "requested"
                        sba_tile.service_manager.symbol_table[valaddress]=setStatus(label,DS_requested)                        
                        # set status of subtask to "blocked"                       
                        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_blocked)
                        # for new status calculation
                        sba_tile.service_manager.subtask_list.incr_nargs_absent(parent.current_subtask)
                        # set "waiting for ACK" flag
                        sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,1)
                        #iv
                            puts "IF CORE: BLOCKED #{parent.current_subtask} for ACK: #{operation}"
                        #ev                            
                    else
                        sba_tile.service_manager.subtask_list.redir(parent.current_subtask,0)
                        # WV 15042009: STS_processed leads to a race condition between activate and clean-up
                        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_inactive)                            
                    #iv
                        puts "IF CORE: TAILCALL: #{operation}"
                    #ev
                    end
                    parent.core_status=CS_managed
                    packet_type=P_reference #t Packet_Type
                    prio=0 #t Ctrl_t
                    payload_length=1 #t Length_t
                    to=getName(result) #t To_t
                    return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask) #t Return_to_t
                    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
                    redir=0 #t Redir_t                        
                    if operation==M_SBACore_IF_return or operation==M_SBACore_IF_if 
                        redir=1
                    end
                    ref_packet_header= mkHeader(packet_type,prio,redir,payload_length,to,return_to,send_ack_to,return_as)
                    
                    ref_packet_payload=result_list #t Word_List 
                    ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                    puts ppPacket(ref_packet) if @v #skip
#iv                        
                        puts "IF CORE: REDIR #{parent.current_subtask} TO #{getName(result)}" #C++ cout << "IF CORE: REDIR "<<parent.current_subtask<<" TO "<<(int)getName(result)<<endl; 
#ev                        
                    parent.core_return_type= P_reference
                    sba_tile.service_manager.subtask_list.to(parent.current_subtask,getName(result))                
                    if to!=S_IF
                        sba_tile.transceiver.tx_fifo.push(ref_packet)
                    else
                    #iv
                        puts "IF CORE: LOCAL CALL"
                    #ev 
                        if SEQVM==0                           
                        sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                        else # SEQVM==1
                        sba_tile.service_manager.activate_subtask(ref_packet)  
                        end # SEQVM
                        
                    end
=begin
    So far, so good: RETURN send a ref packet, result will be redirected. Now, the final dest will send and ACK and this will
    arrive as P_data, containing a reference (quoted?). So how will RETURN know that this is an ACK?
    We need some state information: say the state reg contains a 1 if we're waiting for an ACK? Not good. This should be
    information in the subtask item. So we add a flag "waiting_for_ack" to the subtask list item.
    Then we simply say
    if sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask)==1
        # OK, we received an ACK, time to clean up
        # The problem is that the core must return something. But no other service is expecting a packet. So that's useless.
        # So I'll use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
        parent.core_status=CS_managed
    else
        # The usual
    end
=end
                else
                    raise "IF CORE: TROUBLE: don't know <#{getKind(result)}>"
                end          
            else
                # It's unquoted, so it's a value. Just return it.
                puts "Unquoted word, must be value"  if @v #skip
                parent.core_return_type=P_data
            end        
#iv
        if @v #skip
            puts "IF CORE: <#{result}> #{ppSymbol(result)} " #skip
            puts "IF CORE: )" #skip
        else #skip
            puts "IF CORE: (#{ppSymbol(result_list[0])})"
        end #skip
        puts "IF CORE: SUBTASK: #{parent.current_subtask}"
        puts "IF CORE: #{parent.core_return_type} TO: #{sba_tile.service_manager.subtask_list.to(parent.current_subtask)}"
#ev
    
        parent.result(result_list)
    else
        parent.result( [] ) #C++ Word_List empty_list; parent.result(empty_list);
    end
        
        end
    end # of ls_IF


    # -----------------------------------------------------------------------------    

=begin
The IF in the ServiceManager (S_IF) can only take quoted values. It can take K_R (not K_C), K_D and K_B
As far as I can see, this IF does not need ACK as it delivers locally; anyway ACK would be too complicated
So the assumption is that this IF always delivers locally, i.e. (S1 ... (S1-IF ...))
=end

    def ls_S_IF(sba_tile,parent)#t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        operation=parent.opcode #t Word
        addresses=parent.addresses() #t MemAddresses&
        valaddress=0 #t MemAddress 
        if operation==M_SBACore_S_IF_return
          puts "S_RETURN" #skip
            valaddress=addresses[0]
        else # must be IF
            puts "S_IF" #skip
            condaddr=addresses[0] #t MemAddress
            condval=sba_tile.data_store.mget(condaddr)[0]&1 #t Word
            if condval!=0
                valaddress=addresses[1]
            else
                valaddress=addresses[2]
            end           
        end # RETURN or IF
        
        result_list=sba_tile.data_store.mget(valaddress) #t Word_List
        result=result_list[0] #t Word
        #WV25112008: to make this really efficient, we could set K_D==P_request etc.
        if getKind(result) == K_D
            # If it's a variable or data => request & redirect result
            # i.e. create a request packet
            parent.core_return_type= P_request                               
        elsif getKind(result) == K_B                         
            # If it's a quoted expression =>  Just return it
            parent.core_return_type=P_data                                
        elsif getKind(result) == K_R   
          puts "S_IF/S_RETURN: Found reference #{result}, #{ppSymbol(result)}" #skip
          sba_tile.service_manager.subtask_list.to(parent.current_subtask,getName(result))
            parent.core_return_type= P_reference
        end          
        
        parent.result(result_list)
        
    end # of ls_S_IF        
    # -----------------------------------------------------------------------------    

    def ls_Math(sba_tile,parent)#t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        
        operation=parent.opcode #t Word
        addresses=parent.addresses() #t MemAddresses&
#C++    Word_List result_list;
        if operation==M_SBACore_Math_rand        
        address0=addresses[0] #t MemAddress
        #FIXME: if min_Val or max_val fits in 16 bits, should be unextended!
        words=sba_tile.data_store.mget(address0) #t Word_List
        min_val=getInt(words) #t Int
        
        address1=addresses[1] #t MemAddress
        words=sba_tile.data_store.mget(address1)
        max_val=getInt(words) #t Int
        res_symbol=EXTSYM #C++ const Word res_symbol = EXTSYM;
        #C++ result_list.push_back(res_symbol);
        #C++ srandom(parent.state_register[0]);      
        rnd_val = rand(max_val) #C++ Word rnd_val = random();
        res_val=min_val+rnd_val #C++ Word res_val=min_val + max_val + (rnd_val % max_val);  
        result_list=[res_symbol,res_val]  #C++ result_list.push_back(res_val);
#iv        
        puts "RAND CORE RESULT: #{res_val}"
#ev         
        else
            raise "Math: Operation #{operation} not supported"
        end
        parent.result(result_list)
        
    end # of ls_Math     
    # -----------------------------------------------------------------------------      


# ---------------------------------------------------------------------------------------------------------

    # With LET aliased to ASSIGN, in principle we could register all calls to ASSIGN'ed variables.
    # Then LET could multicast to those services only the list of variables to clean up.
    # Direct addressing is very risky, if not impossible, for this: the variable will be stored at different addresses
    # for every service involved. But ASSIGN _knows_ those addresses!     
 
    def ls_LET(sba_tile,parent) #t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        addresses=parent.addresses() #t MemAddresses&
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
        
#		puts parent.current_subtask if @v #skip
		service=getName(service_word) #t Name_t
        parent.core_return_type=P_data
#iv
        ppservice=service #C++ int ppservice=(int)service;
		if @v #skip
        print "LET (#{parent.service}) CORE: #{parent.current_subtask}: (#{ppservice}<>#{SC_SBACore_LET}\n" 
		end #skip
#ev        
        method=parent.method() #t uint
#        puts "METHOD: #{method}",M_SBACore_LET_assign
        if method==M_SBACore_LET_let or method==M_SBACore_LET_lettc
#iv
			if @v #skip
	        print "LET (#{parent.service}) CORE: ",parent.current_subtask,"\n"

            print "LET (#{parent.service}) CORE: ", "TO: ", sba_tile.service_manager.subtask_list.to(parent.current_subtask),"\n"            
            print "LET (#{parent.service}) CORE: ", "RETURN TO: ", sba_tile.service_manager.subtask_list.return_to(parent.current_subtask),"\n"
            print "LET (#{parent.service}) CORE: ", "RETURN AS: ", sba_tile.service_manager.subtask_list.return_as(parent.current_subtask),"\n"
            print "LET (#{parent.service}) CORE: ", "CALLED AS: ", sba_tile.service_manager.subtask_list.called_as(parent.current_subtask),"\n"
			end #skip
#ev            
            last=false #t bool
            nargs=parent.n_args #t uint        
            argct=nargs #t uint
            for address in addresses #t MemAddresses       
                argct-=1
                last=(argct==0)
                label=sba_tile.service_manager.symbol_table[address] #t Word     
            #iv
				if @v #skip
				    yn= ( last ? "Y" : "N" ) #t string
                print "LET (#{parent.service}) CORE: ", "LABEL:", label," last? ",yn,"\n"             
				end #skip
            #ev
                
                if getQuoted(label)==1
                    label=setQuoted(label,0) # Next time it's a proper value, not a quoted symbol
                    numval=sba_tile.data_store.mget(address)[0] #t Word
#                    print "Found Q: ",numval," at ",label,"\n" if @v #skip
                    # -reset the datastatus:
                    label=setStatus(label,DS_requested)
                    # for the new status calculation
                    sba_tile.service_manager.subtask_list.incr_nargs_absent(parent.current_subtask)
                    sba_tile.service_manager.symbol_table[address]=label
                    if getKind(numval)!=K_R                  
                        raise "Only R's allowed inside LET, no bare L's. Use RETURN." if @v #skip
                    end
                    # - reset the task status to STS_blocked
                    #iv
						if @v #skip
                        puts "LET (#{parent.service}) CORE: BLOCKED #{parent.current_subtask}"             
						end #skip
                    #ev                    
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_blocked)
                    # -create a ref packet and send it off
#                    to=getName(numval) #t To_t
                    to=getSNId(numval) #t To_t
                    return_to=S_SBACore_LET #t Return_to_t
#                    puts "TO:#{to},RETURN_TO:#{return_to}"
                    var_label = setSubtask(label,address) #t Word
					var_label = setName(var_label,S_SBACore_LET)
                    return_as=var_label #t Word
                    ack_to=0 #t Word
                    packet_type=P_reference #t Packet_Type
                    prio=0 #t Ctrl_t
                    redir=0 #t Redir_t
                    reslist=[] #t Word_List
                    reslist.push(numval)
                    payload_length=1 #t Length_t
=begin
So what happens if it's a tail call?
- redirect
- but exit, i.e. should jump past the return
=end
#C++                bool use_redir=false;


                    use_redir=true

                    if (last and use_redir)      
                        puts "LET CORE: last arg quoted, REDIR/wait for ACK" if @v #skip
                        # if the last argument is quoted, redirect instead of trying to sequence
                        # so the return packet will be an ACK
                        # CORE decides to redirect
                        parent.core_status=CS_managed
                        send_ack_to=setName(label,S_SBACore_LET) #t Word
                        send_ack_to=setSubtask(send_ack_to,address)
                        parent.ack_ok=0 # If a core redirects, it doesn't send an ACK
                        # set "waiting for ACK" flag
                        if method!=M_SBACore_LET_lettc                       
                            sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,1)
                        end
                        return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask)                       
                        return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask)
                        ref_packet_header= mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as)
                        ref_packet_payload=reslist #t Word_List 
                        ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                        puts ppPacket(ref_packet) if @v #skip
                        # FIXME: to is FQN, S_LET is only SNId!
                        if to!=S_SBACore_LET
                          #sysc           OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": 3 LET CORE sends packet To:"<<to<<"\n"; 
                          # To:"<<to<<"; Return-to:"<<return_to<<";Ctrl:"<<ctrl<<endl;

                            sba_tile.transceiver.tx_fifo.push(ref_packet)
                        else
                            if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                            else # SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet)  
                            end # SEQVM
                        end
                        puts "Packet will go to: #{sba_tile.service_manager.subtask_list.to(parent.current_subtask)} as type #{parent.core_return_type}" if @v #skip
                        puts "Code address: #{getSubtask(label)}" if @v #skip
                    else
                    if VERBOSE==1                        
                        puts "NOT LAST arg, sequencing"
                    end # VERBOSE 
                        # -set the core status to CS_managed
                        parent.core_status=CS_managed                    
                        # if redirection is not supported, the value of the last arg should return in the end
                        ref_packet_header= mkHeader(packet_type,prio,redir,payload_length,to,return_to,ack_to,return_as)
                        ref_packet_payload=reslist #t Word_List 
                        ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                        if VERBOSE==1
                        puts ppPacket(ref_packet)
                        end # VERBOSE 
                        if to!=S_SBACore_LET
                            sba_tile.transceiver.tx_fifo.push(ref_packet)
                        else
                            if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                            else # SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet)  
                            end # SEQVM                            
                        end
                    end # of if last
                    if  service!=M_SBACore_LET_lettc               
                        parent.result( reslist ) # will be ignored anyway
                    else 
                        break
                    end    
                end # of if quoted
            end # of for
                    
            # If none of the arguments is quoted, process the last argument
            # So here we restore the subtask record from the internal state
            # and we return the value for the last argument
            # This means there is no redirection!
            labeladdr=addresses[nargs-1] #t MemAddress
            result=sba_tile.data_store.mget(labeladdr) #t Word_List
            parent.core_return_type=P_data
            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processing)
            if sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask)==1
                # OK, we received an ACK, time to clean up
                puts "LET CORE: got ACK" if @v #skip
#iv
				if @v #skip
                puts "LET CORE: ACK REDIR: #{sba_tile.service_manager.subtask_list.redir(parent.current_subtask)}"
				end #skip
#ev                
                # The problem is that the core must return something. But no other service is expecting a packet. So that's useless.
                # So I'll use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
                parent.ack_ok=1 # we're not redirecting so we can ack
                parent.core_status=CS_managed
                sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processed)
                sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,0)        
            end

            # Cleaning up all ASSIGN'ed variables inside current LET
            # Assumptions:
            # - ASSIGN call returns a Symbol K_L:D_x:x:x:x:VarAddress:Name
            # - The lookup table contains a Symbol K_x:D_x:x:x:Data_Status:VarAddress:Name
            # - The symbol_table contains a Symbol K_R:D_x:x:Quoted:DS_present:Subtask:S_ASSIGN
            puts addresses.inspect if @v #skip
            puts sba_tile.service_manager.symbol_table.inspect if @v #skip
            
            for labeladdr in addresses #t MemAddresses
#iv
				if @v #skip
                puts "LET-ASSIGN\tFound #{labeladdr}"
				end #skip
#ev                
                var_label_l=sba_tile.data_store.mget(labeladdr) #t Word_List
                    if var_label_l.length>0        
                    var_label=var_label_l[0] #t Word    
                    if getKind(var_label)==K_L
                        var_address=getSubtask(var_label) #t MemAddress 
                        var_name=getName(var_label) #t Name_t 
                      # Now clear this variable
                        # remove from the data address list of ASSIGN
                        # push the address back on the stack
                    
                        print "LET-ASSIGN\tFound #{ppSymbol(var_label)} @ #{labeladdr}\n" if @v #skip
                        # The next bit only works if we can store arbitrary-length lists in the store
                        # Remove the entry from the lookup table
                        has_label=false #t bool
#                        lookup=sba_tile.data_store.mget(sba_tile.service_manager.service_core_ram+1) #t Word_List
#                        nlookup=[] #t Word_List
#                        for word in lookup #t Word_List
#                            if getName(word) == var_name and getSubtask(word)==var_address
#                                has_label=true    
#                            else
#                                nlookup.push(word)
#                            end
#                        end                
                        
                        if sba_tile.lookup_table.count(var_name)==1
                            word=sba_tile.lookup_table.read(var_name) #t Word
                            if getSubtask(word)==var_address
                                has_label=true 
                                sba_tile.lookup_table.erase(var_name)
                            end
                        end
                        
                        if has_label==true
#                            sba_tile.data_store.mput(sba_tile.service_manager.service_core_ram+1,nlookup)
                            print "LET-ASSIGN\tPushed #{var_address} on data_address_stack in ASSIGN\n" if @v #skip
                            sba_tile.service_manager.data_address_stack.push(var_address)
                            # FIXME: maybe we do need some way to remove data!
                            sba_tile.data_store.remove(var_address) #skip 
                        end                                        
                    end
                    end                        
                    
            end
            puts "LET (#{parent.service}) CORE: Passing on #{result}" if @v  #skip
            parent.result( result )
        ### end of LET                                
        else # NOT M_SBACore_LET_let
            result=0 #t Word
            #C++ #ifndef STATIC_ALLOC
            value_list=[] #t List<Word_List>            
            #C++ #else
            #C++ List<Word_List,MAX_NARGS> value_list;
            #C++ #endif
            for address in addresses #t MemAddresses
#iv            
			if @v #skip
                    try_array=sba_tile.data_store.mget(address) #t Word_List
					puts try_array.inspect #skip
                    print "LET (#{parent.service}) CORE: #{address}\t#{ppPayload(try_array)}\n"
			end #skip 
#ev
                    value_list.push(sba_tile.data_store.mget(address)) 
            end  
            var=value_list[0] #t Word_List # first symbol is the variable declaration
            # 1. Create an address_label from the first word of function             
            var_name=var[0] #t Word
            puts "var_name: #{ppSymbol(var_name)}" if @v #skip
            result=var_name
            # 2. Use this address_label to store definition 
            print "<#{var_name}>" if @v #skip
            ###### ASSIGN ###### 
            case method
            when M_SBACore_LET_assign 
                # Take the addresses of Symbol and Data
                puts "LET called as ASSIGN" if @v #skip
#WV20110612: this could be pass-by-value
                sym_address=addresses[0] #t MemAddress
                data_address=addresses[1] #t MemAddress
                #iv
				if @v #skip
                puts "ASSIGN addresses: #{sym_address} => #{data_address}"
				end #skip
                #ev
                # Get the varname from the symbol
#WV20110612: for pass-by-value, we pass the symbol, so we would not need the code below				
                var_label=sba_tile.service_manager.symbol_table[sym_address] #t Word
                puts "ASSIGN SYMBOL: #{ppSymbol(var_label)}" if @v #skip
                var_name=getName(var_label) #t Name_t
                # Check if this varname is in the lookup table
                has_label=false #t bool
                var_address=0 #t MemAddress
                
                if sba_tile.lookup_table.count(var_name)==1
                    word = sba_tile.lookup_table.read(var_name) #t Word
                    has_label=true    
                    var_address= getSubtask(word)
                end                    
                if has_label==false
                    # Nothing stored under this address_label. That's as it should be
                    var_address=sba_tile.service_manager.data_address_stack.pop
                    puts "ADDRESS FROM STACK: #{var_address}" if @v #skip
                    word=setSubtask(var_label,var_address) #t Word
                    word=setStatus(word,DS_present)
                    puts "ASSIGN: NAME: #{var_name}=>ADDRESS:#{var_address}" if @v #skip
#iv
					if @v #skip
                    puts "ASSIGN: STS=#{sba_tile.service_manager.subtask_list.status(parent.current_subtask)}"
					end #skip
#ev                    
                    sba_tile.lookup_table.write(getName(word),word)
                # Store variable definition
                    var_value=sba_tile.data_store.mget(data_address) #t Word_List
#iv
					if @v #skip
                    puts "ASSIGN: storing",ppPayload(var_value), "@ #{var_address}"
					end #skip
#ev                    
                    sba_tile.data_store.mput(var_address,var_value)
                    result=word
#iv                                
                else
					if @v #skip

                    # Some trouble: we're overwriting a presumably immutable variable!!                    
                    puts " [WARNING: overwriting <#{var_label}> (",ppPayload(sba_tile.data_store.mget(var_address)),")] "                                
					end #skip
#ev                        
                end
                parent.result([result]) #C++ Word_List result_list; result_list.push_back(result); parent.result(result_list);
                ###### UPDATE #######
            when M_SBACore_LET_update
               # Receives a quoted L, looks up the value and updates it.
                sym_address=addresses[0] #t MemAddress
                # Get the varname from the symbol
                var_label=sba_tile.service_manager.symbol_table[sym_address] #t Word
                puts "LET called as UPDATE #{ppSymbol(var_name)}" if @v #skip
                var_name=getName(var_label)
                # Check if this varname is in the lookup table                
                has_label=false #t bool
                var_address=0 #t MemAddress
                if sba_tile.lookup_table.count(var_name)==1
                    word=sba_tile.lookup_table.read(var_name) #t Word
                    has_label=true
                    var_address= getSubtask(word)
                end                
                result_list=[] #t Word_List
                if has_label==true
                    # Value stored under this address_label. That's as it should be
                    # Now update
                    newval_address=addresses[1] #t MemAddress
                    newval= sba_tile.data_store.mget(newval_address) #t Word_List
                    sba_tile.data_store.mput(var_address,newval)
                    result_list=newval
                else
                    # ASSIGN has not yet returned. 
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_pending)
                    parent.core_status=CS_managed                    
                end
                parent.result(result_list)            
                ###### READ #######
                # It turns out that if we use UPDATE, we need READ. That's because L-variables could be cached          
            when M_SBACore_LET_read
                # What does a call do? Very simple: it receives a quoted L, looks up the value and returns it.
                sym_address=addresses[0] #t MemAddress
                # Get the varname from the symbol
                var_label=sba_tile.service_manager.symbol_table[sym_address] #t Word
                puts "LET called as READ #{ppSymbol(var_label)}" if @v #skip
                var_name=getName(var_label)
                # Check if this varname is in the lookup table
                has_label=false #t bool
                var_address=0 #t MemAddress
                
                if sba_tile.lookup_table.count(var_name)==1
                    word=sba_tile.lookup_table.read(var_name) #t Word
                    has_label=true
                    var_address= getSubtask(word)
                end                              
                result_list=[] #t Word_List  
                if has_label==true
                    # Value stored under this address_label. That's as it should be
                    var_value=sba_tile.data_store.mget(var_address) #t Word_List
                    result_list=var_value
#iv                    
                    if @v #skip
					puts "READ: returning",ppPayload(result_list)
					end #skip
#ev                    
                else
                    # TROUBLE: ASSIGN has not yet returned. In the brave new world, where we do 
                    # (READ 'v (UPDATE 'v Ru (ASSIGN 'v Ra)))
                    # this is impossible of course
                    puts "READ: not yet there","Setting subtask <#{parent.current_subtask}> to #{STS_pending}" if @v #skip
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_pending)
                    parent.core_status=CS_managed
                end                
                parent.result(result_list)
#C++        break;}
#C++        default: std::cerr<< "Service "<<service<< " has no implementation\n";exit(0);
            end # of non-LET services case block
           
#iv           
			if @v #skip
            print  ") => "		
            print ppSymbol(result),"\n"
			end #skip
#ev            
            parent.core_return_type=P_data	            
            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_cleanup)
            parent.result([result])  #C++  Word_List result_list; result_list.push_back(result); parent.result(result_list);
        end # of not M_SBACore_LET_let
#C++ } // of not M_SBACore_LET_let
    end # of ls_LET        
# -----------------------------------------------------------------------------    
    # SEQ is like a LET but it does not provide scope, all it does is sequence the arguments. It's like BEGIN with seq support.
def ls_SEQ(sba_tile,parent) #t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
    #core 
    addresses=parent.addresses() #t MemAddresses&
    service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word    
    puts parent.current_subtask if @v #skip
    service=getName(service_word) #t Name_t
    parent.core_return_type=P_data
#iv
    ppservice=service #C++ int ppservice=(int)service;
    print "SEQ (#{parent.service}) CORE: #{parent.current_subtask}: (#{ppservice}<>#{M_SBACore_SEQ_seq}\n" 
    print "SEQ (#{parent.service}) CORE: ", "TO: ", sba_tile.service_manager.subtask_list.to(parent.current_subtask),"\n"            
    print "SEQ (#{parent.service}) CORE: ", "RETURN TO: ", sba_tile.service_manager.subtask_list.return_to(parent.current_subtask),"\n"
    print "SEQ (#{parent.service}) CORE: ", "RETURN AS: ", sba_tile.service_manager.subtask_list.return_as(parent.current_subtask),"\n"
    print "SEQ (#{parent.service}) CORE: ", "CALLED AS: ", sba_tile.service_manager.subtask_list.called_as(parent.current_subtask),"\n"
#ev            
        last=false #t bool
        nargs=parent.n_args #t uint        
        argct=nargs #t uint
        for address in addresses #t MemAddresses       
            argct-=1
            last=(argct==0)
            label=sba_tile.service_manager.symbol_table[address] #t Word     
        #iv
            print "SEQ (#{parent.service}) CORE: ", "LABEL:", label," last?",last,"\n"             
        #ev            
            if getQuoted(label)==1
                label=setQuoted(label,0) # Next time it's a proper value, not a quoted symbol
                numval=sba_tile.data_store.mget(address)[0] #t Word
                print "Found Q: ",numval," at ",label,"\n" if @v #skip
                # -reset the datastatus:
                label=setStatus(label,DS_requested)
                # for the new status calculation
                sba_tile.service_manager.subtask_list.incr_nargs_absent(parent.current_subtask)
                sba_tile.service_manager.symbol_table[address]=label
                if getKind(numval)!=K_R                  
                    raise "Only R's allowed inside SEQ, no bare L's. Use RETURN." if @v #skip
                end
                # - reset the task status to STS_blocked
                #iv
                    puts "SEQ (#{parent.service}) CORE: BLOCKED #{parent.current_subtask}"             
                #ev                    
                sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_blocked)
                # -create a ref packet and send it off
                to=getName(numval) #t To_t
                return_to=S_SBACore_SEQ #t Return_to_t
                var_label = setSubtask(label,address) #t Word
                var_label = setName(var_label,S_SBACore_SEQ)
                return_as=var_label #t Word
                ack_to=0 #t Word
                packet_type=P_reference #t Packet_Type
                prio=0 #t Ctrl_t
                redir=0 #t Redir_t
                reslist=[] #t Word_List
                reslist.push(numval)
                payload_length=1 #t Length_t
                use_redir=true #t bool
                if (last and use_redir)      
                    puts "SEQ CORE: last arg quoted, REDIR/wait for ACK" if @v #skip
                    # if the last argument is quoted, redirect instead of trying to sequence
                    # so the return packet will be an ACK
                    # CORE decides to redirect
                    parent.core_status=CS_managed
                    send_ack_to=setName(label,S_SBACore_SEQ) #t Word
                    send_ack_to=setSubtask(send_ack_to,address)
                    parent.ack_ok=0 # If a core redirects, it doesn't send an ACK
                    # set "waiting for ACK" flag
                    if service!=M_SBACore_SEQ_seqtc                        
                        sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,1)
                    end
                    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask)                       
                    return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask)
                    ref_packet_header= mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as)
                    ref_packet_payload=reslist #t Word_List 
                    ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                    puts ppPacket(ref_packet) if @v #skip
                    if to!=S_SBACore_SEQ
                      #sysc           OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": 3 SEQ CORE sends packet To:"<<to<<"\n"; 
                        sba_tile.transceiver.tx_fifo.push(ref_packet)
                    else
                        if SEQVM==0
                        sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                        else # SEQVM==1
                        sba_tile.service_manager.activate_subtask(ref_packet)  
                        end # SEQVM
                    end
                    puts "Packet will go to: #{sba_tile.service_manager.subtask_list.to(parent.current_subtask)} as type #{parent.core_return_type}" if @v #skip
                    puts "Code address: #{getSubtask(label)}" if @v #skip
                else
                    puts "NOT LAST arg, sequencing" if @v #skip
                    # -set the core status to CS_managed
                    parent.core_status=CS_managed                    
                    # if redirection is not supported, the value of the last arg should return in the end
                    ref_packet_header= mkHeader(packet_type,prio,redir,payload_length,to,return_to,ack_to,return_as)
                    ref_packet_payload=reslist #t Word_List 
                    ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                    puts ppPacket(ref_packet) if @v #skip
                    if to!=S_SBACore_SEQ
                        sba_tile.transceiver.tx_fifo.push(ref_packet)
                    else
                        if SEQVM==0
                        sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                        else # SEQVM==1
                        sba_tile.service_manager.activate_subtask(ref_packet)  
                        end # SEQVM                            
                    end
                end # of if last
                if  service!=M_SBACore_SEQ_seqtc                    
                    parent.result(reslist) # will be ignored anyway
                else 
                    break
                end    
            end # of if quoted
        end # of for
                
        # If none of the arguments is quoted, process the last argument
        # So here we restore the subtask record from the internal state
        # and we parent.result(the value for the last argument
        # This means there is no redirection!
        labeladdr=addresses[nargs-1] #t MemAddress
        result=sba_tile.data_store.mget(labeladdr) #t Word_List
        parent.core_return_type=P_data
        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processing)
        if sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask)==1
            # OK, we received an ACK, time to clean up
            puts "SEQ CORE: got ACK" if @v #skip
#iv
            puts "SEQ CORE: ACK REDIR: #{sba_tile.service_manager.subtask_list.redir(parent.current_subtask)}"
#ev                
            # The core must return something. But no other service is expecting a packet. 
            # So I'll use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
            parent.ack_ok=1 # we're not redirecting so we can ack
            parent.core_status=CS_managed
            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processed)
            sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,0)        
        end
        puts "SEQ (#{parent.service}) CORE: Passing on #{result}" if @v  #skip
        
#iv            
        print  ") => "      
        print ppSymbol(result[0]),"\n"
#ev            
        parent.core_return_type=P_data              
        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_cleanup)
        parent.result(result)
#        return [result] #C++  Word_List result_list; result_list.push_back(result); parent.result(result_list);
end # of ls_SEQ       
    # -----------------------------------------------------------------------------
    def ls_LAMBDA(sba_tile,parent) #t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core
        addresses=parent.addresses() #t MemAddresses& 
#iv
        print "LAMBDA CORE (#{parent.current_subtask}): \n"
#ev
        result=[] #t Word_List
        value_list=[] #t Word_List
        for address in addresses #t MemAddresses
                tval=sba_tile.data_store.mget(address)[0] #t Word
                value_list.push(tval)
        end
        for value in value_list #t Word_List
            result.push(value)
        end
#iv
        puts "\nLAMBDA CORE (#{parent.current_subtask}): result:",ppPayload(result)
#ev
        parent.result(result)
    end # of LAMBDA
    # -----------------------------------------------------------------------------

# skip


=begin

24/11/2007

APPLY can only be called once per subtask, so we need a mechanism to
-keep track of the current calls
-defer the task
A call is "current" as long as it has not returned. APPLY redirects, so 
this means it needs to wait for ACKs.
So first we need a proper ACK mechanism for APPLY
Then we need a list of "current" subtasks. Store them in the state register.
Unfortunately, there is a serious catch:
If we call APPLY with a recursive task, the first recursive call is not unique
because the "root" task is waiting for the ACK to return,
so the whole computation blocks. So somehow I should distinguish between
recursive calls and multiple calls...

=end

    def ls_APPLY(sba_tile,parent) #t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
            #core 
#        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word  
#    	service=getName(service_word) #t Name_t
        addresses=parent.addresses() #t MemAddresses&
    	method=parent.opcode #t Uint  
    	# FIXME: why is this not "opcode"?
    	#C++ bool use_redir; bool use_unique;
        if method==M_SBACore_APPLY_apply
#iv
            print "APPLY CORE (#{parent.current_subtask}): \n"
#ev        
            use_redir=true
            if SEQVM==0            
            use_unique=true
            else # SEQVM==1
              use_unique=false  
            end # SEQVM
        elsif method==M_SBACore_APPLY_applytc
        #iv
            puts "APPLY TAILCALL" 
        #ev            
            use_redir=false
            use_unique=false
        else
            raise "CORE APPLY: no such method #{method}"
        end
     # First check if this subtask was just waiting for an ACK   
         if sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask)==1
            # OK, we received an ACK, time to clean up
            # Use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
            puts "APPLY CORE: got ACK" if @v #skip 
            if parent.lookup_table.count(parent.current_subtask) 
                lref=parent.lookup_table.read(parent.current_subtask) #t Word
#iv
                puts "APPLY: remove #{lref} for #{parent.current_subtask}"
#ev
                parent.lookup_table.erase(parent.current_subtask) #s/delete/erase/
                parent.lookup_table.erase(lref) #s/delete/erase/
            end
            parent.ack_ok=1 # we're not redirecting so we can ack
            parent.core_status=CS_managed
            #iv
            puts "WARNING: core_status=CS_managed; returning empty list"
#ev
            parent.result([]) #C++ Word_List empty_list; parent.result(empty_list);
        else
            # A new call to APPLY.
            unique=true #t bool
            result=0 #t Word
            lambda_function_args=[] #t Word_List 
            # The first argument is the lambda definition, i.e. a list of symbols
            # all other arguments are symbols referencing values for the lambda variables
            # Loop over all argument value symbols in the subtask	   	
            lambda_def_address=addresses[0] #t MemAddress
            lambda_label=sba_tile.service_manager.symbol_table[lambda_def_address] #t Word
#iv                            
            data_kind=getKind(lambda_label) #t Kind_t
                # What Kind can the arguments to APPLY have?
                # The LAMBDA will be L or R. Could it be U? Yes, e.g. recursive function
                # All others will be Q ????
            data_status=getTask(lambda_label) #t Task_t 
#ev                
            # If we could check here to see if it is really a LAMBDA and not a returned result (which might be a LAMBDA!)
            # then we would be OK. Maybe if I set the kind to K_D, that could work, as it can never be D if it's a LAMBDA
            lambda_function=sba_tile.data_store.mget(lambda_def_address) #t Word_List   
#iv
            puts "LAMBDA FUNCTION:",ppPayload(lambda_function)
            puts lambda_label,data_kind,lambda_def_address,data_status if @v #skip
                puts "LEN: #{lambda_function.length}"
#ev
            if lambda_function.length>1   # (non-trivial only!)
                ext=0 #t uint
                for itemw in lambda_function #t Word_List                   
                    next if ext>0 #C++ if (ext==0) {
                    ext=getExt(itemw)
                    if getKind(itemw) == K_S 
                        raise "APPLY CORE: ERROR: LAMBDA definition list args can only be U or R"
                    end
                    if getKind(itemw) == K_A
                    #iv
                        puts "ARG: #{ppSymbol(itemw)}"
                    #ev
                        lambda_function_args.push(itemw)
                    elsif getKind(itemw)==K_R
                    #iv
                            puts "REF: #{ppSymbol(itemw)}" 
                    #ev
                        ext=0
                        # Here we can check
                        if parent.lookup_table.count(itemw)==0

#iv
                            puts "APPLY: activating code #{ppSymbol(itemw)} for #{parent.current_subtask}" 
#ev                                
                            parent.lookup_table.write(parent.current_subtask,itemw)
                            parent.lookup_table.write(itemw,parent.current_subtask)
                        else
                            unique=false 
                            puts "APPLY: NOT UNIQUE: code #{ppSymbol(itemw)} is already active: #{parent.lookup_table.inspect}" if @v #skip
                        end
                        break
                    end
                    #C++ } // do "next"
                end
            end                      
            
            if use_unique and not unique 
                # defer the task
                result_list=[] #t Word_List  
#iv
                puts "APPLY CORE: DEFER: task is not unique, setting subtask <#{parent.current_subtask}> to #{STS_pending}"
#ev                        
                sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_pending)
                parent.core_status=CS_managed
                parent.result(result_list)
            end 
#iv                    
        pp_data_kind=data_kind #C++ int pp_data_kind=(int)data_kind;
        pp_data_status=data_status #C++ int pp_data_status=(int)data_status;
        print  "APPLY CORE: #{parent.current_subtask}: CALLER ARG: (#{pp_data_kind}) #{ppSymbol(lambda_label)}=>#{lambda_def_address}=> <#{pp_data_status}>\n" 
        print  "APPLY CORE: #{parent.current_subtask}: CALLER VAL: ",ppPayload(sba_tile.data_store.mget(lambda_def_address)),"\n" 
#ev                
        # end of lambda definition handling    
    # In the brave new world, LAMBDA returns a list with first the arguments (if any) and then the references. 
    #iv 
    print "APPLY CORE: ","\nREWRITING R-code\n"
    #ev
    root_ref=1 #t uint
=begin
    The sequence is:
    * Loop through all reference symbols Ri for nested references (if any)
    * look up the code based on getSubtask(Ri)
    * loop through the code and substitute the K_A symbols with the corresponding symbol from called_function_args
    * Dispatch the new code as Code packets
    * Finally, loop through the root ref code, substitute and dispatch as Subtask packet
    FIXME: Currently, we send a Ref packet after the last Code
=end
    for ref_symbol_word in lambda_function #t Word_List       
        # LAMBDA never changes and is either bound to an L or it's an unquoted R, i.e. resolved at marshalling time. 
        # So it should not be in the R-list. Only R's of Datatype u should be in the list.
        next unless getKind(ref_symbol_word)==K_R #skip # i.e. skip the arguments. 
        #C++ if(getKind(ref_symbol_word)==K_R) {                
        #TODO: Ideally, there would be an indication of the number of arguments.
        # This is simple: use the  Service symbol's subtask field to indicate the number of args            
        if root_ref==1 #  Simple way of choosing the first arg as the root reference of the task
            result=ref_symbol_word
            root_ref=0
        end
        # Get the code corresponding to the reference
        #iv
        puts "APPLY CORE: code for #{ppSymbol(ref_symbol_word)}" 
        #ev                
        lambda_function_definition_address=getCodeAddress(ref_symbol_word) #t CodeAddress
        #iv
        puts "APPLY CORE: CODE ADDRESS: #{lambda_function_definition_address}"
        #ev                
        lambda_function_definition=sba_tile.code_store.mget(lambda_function_definition_address) #t Word_List
        #iv 
        puts "LAMBDA FUNCTION PACKET PAYLOAD:",ppPayload(lambda_function_definition) 
        #ev
        appl_function=[] #t Word_List #s/=..//                        
        ext=0 #t uint
        ii=0 #t uint
        #iv
            puts "LAMBDA FUNCTION LEN: #{lambda_function_definition.length}"
        #ev
        for i in 0..lambda_function_definition.length-1 #t uint
            symbol_word=lambda_function_definition[i] #t Word
            if ext==0
                if  getKind(symbol_word)!=K_A
                    appl_function.push(symbol_word) 
                    #iv
                    puts "X:appl_function[#{ii}]: #{ppSymbol(symbol_word)}"
                    #ev
                else      
                    # Lookup by loop, as there are very few arguments
                    for j in 0..lambda_function_args.length-1 #t uint                        
                        if setQuoted(symbol_word,0) == setQuoted(lambda_function_args[j],0)
                            first=1 #t uint
                            data_address=addresses[j+1] #t MemAddress
                            symbol_word_list=sba_tile.data_store.mget(data_address) #t Word_List                                    
                            for val_word in symbol_word_list #t Word_List
                                if first==1
                                    newsym=setQuoted(val_word,getQuoted(symbol_word)|getQuoted(val_word)) #t Word
                                    appl_function.push(newsym) 
                                    first=0
                                else
                                    appl_function.push(val_word) 
                                end                             
                                #iv           
                                puts "U:appl_function[#{ii}]: #{appl_function[appl_function.length()-1]}"
                                #ev
                                ii=ii+1
                            end
                            ii=ii-1
                        end 
                    end
                end                                   
                if getExt(symbol_word)==1 and (getKind(symbol_word)==K_B ) # or getKind(symbol_word)==K_Q) # Only K_B symbols should extended!
                    ext=getSubtask(symbol_word)
                    #iv
                    puts "EXT: #{ppSymbol(symbol_word)} => ext=#{ext}"
                    #ev
                    next #C++ ii=ii-1;
                end      
            else # ext>0 
                ext-=1
                ii=ii+1                  
                appl_function.push(lambda_function_definition[i])
                #iv
                puts "E(#{ext}):appl_function[#{ii}]: #{ppSymbol(appl_function[appl_function.length()-1])}"
                #ev
            end                      
            ii=ii+1
        end 
=begin
To make APPLY work with redir, we need to have the ability to take action on return of the ACK
Currently, APPLY redirects but in a different way:
By changing the To field, but keeping the Return-to and Return-as, the result of the computation
will off course go to APPLY's caller.
So setting Redir=1 and Ack-to to the first arg of APPLY could work just fine - unless the call to APPLY was already a redirection.
No, we must do it like in IF or LET
=end
                # Now create a packet with this code
# So we have to create the header:
#           Packet_Type => P_code
#           Length      => appl_function_ext.length
#           Ctrl        => 0
#           To          => set
#           Return_to   => 0 (DC)
#           Ack_to     => 0 (leave redirection for later)
#           Return_as   => set
#           Redir       => 1
        plength=appl_function.length #t uint
        code_packet_header=mkHeader(P_code,0,0,plength,0,0,0,0) 
        code_packet_header=setTo(code_packet_header,getName(ref_symbol_word))
        code_packet_header=setReturn_as(code_packet_header,ref_symbol_word)
        code_packet_payload=appl_function #t Word_List
        puts "CODE PACKET PAYLOAD:",appl_function.inspect if @v #skip
        code_packet=mkPacket(code_packet_header,code_packet_payload)
        puts "CODE PACKET:",code_packet.inspect if @v #skip
#iv
        puts "APPLY CORE: CODE PACKET",ppPacket(code_packet) 
#ev                
        sba_tile.transceiver.tx_fifo.push(code_packet)
        #C++ } // to emulate next
    end  # of for               
    result_symbol=mkSymbol(K_B,0,0,1,0,0,0) # WV17082008: is this meaningful?
            
            if use_redir
            #iv
                        puts "APPLY CORE REDIR/wait for ACK"
            #ev
                        # CORE decides to redirect
                        send_ack_to=setName(lambda_label,S_APPLY) #t Word
                        send_ack_to=setSubtask(send_ack_to,lambda_def_address)
                        parent.ack_ok=0 # If a core redirects, it doesn't send an ACK
                        # Now fool the subtask:
                        # set status of argument to "requested"
                        sba_tile.service_manager.symbol_table[lambda_def_address]=setStatus(lambda_label,DS_requested)                        
                        # set status of subtask to "blocked"                       
                        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_blocked)
                        # for new status calculation
                        sba_tile.service_manager.subtask_list.incr_nargs_absent(parent.current_subtask)
                        # set "waiting for ACK" flag
                        sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,1)
                        parent.core_status=CS_managed
                        packet_type=P_reference #t Packet_Type
                        prio=0 #t Ctrl_t
                        payload_length=1 #t Length_t
                        result_list=[result] #C++ Word_List result_list;result_list.push_back(result);
                        to=getName(result) #t Name_t
                        return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask) #t Return_to_t
                        return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
                        send_ack_to=setName(lambda_label,S_APPLY)
                        send_ack_to=setSubtask(send_ack_to,lambda_def_address)
                        ref_packet_header= mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as)
                        ref_packet_payload=result_list #t Word_List 
                        ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                        #iv
                        puts ppPacket(ref_packet) 
                        #ev
                        if to!=S_APPLY
                            sba_tile.transceiver.tx_fifo.push(ref_packet)
                        else
                          if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                          else # SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet)
                          end # SEQVM
                        end
                        result_symbol=setSubtask(result_symbol,1) # WV17082008: is this meaningful? Should I use Name?
            end            
            if not use_redir                                                 
                puts "APPLY CORE: Result:",pretty(result)  if @v #skip        
                parent.core_return_type= P_reference
                result_symbol=result 
                sba_tile.service_manager.subtask_list.to(parent.current_subtask,getName(result_symbol))

            end   
            parent.result([result_symbol]) #C++ Word_List result_symbol_l;result_symbol_l.push_back(result_symbol);parent.result(result_symbol_l);
        end # if M_SBACore_APPLY_apply
    end # of ls_APPLY

# ----------------------------------------------------------------------------------------------------------------------
#     
    def ls_IO(sba_tile,parent) #t void (na;Base::ServiceCore*)  #s/parent/parent_ptr/
        #core
        addresses=parent.addresses() #t MemAddresses&
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word        
	
		# puts parent.current_subtask #skip
    	service=getName(service_word) #t Name_t
        parent.core_return_type=P_data
    # we use the lookup_table to map ports to file descriptors
    # in fact, in Ruby we don't care at all what we store in the table.
        port_address=addresses[0] #t MemAddress
        port_symbol=sba_tile.data_store.mget(port_address) #t Word_List
        builtin_symbol=EXTSYM # port_symbol[0] #t Word
        # "255 filehandles is enough for everyone!" 
        port = port_symbol[0] & 255  #t Word
        eof=[builtin_symbol,0] #C++ Word_List eof; eof.push_back(builtin_symbol); eof.push_back((Word)(0));      
        pass=[builtin_symbol,1] #C++ Word_List pass; pass.push_back(builtin_symbol); pass.push_back((Word)(1));
if WORDSZ==64
        fail=[builtin_symbol,Integer(2**64-1)] #C++ Word min1=(Uint64)(-1);        
else # WORDSZ==32
        fail=[builtin_symbol,4294967295] #C++ Word min1=(Uint32)(-1);
end # WORDSZ 
#C++    Word_List fail; fail.push_back(builtin_symbol); fail.push_back(min1);
#C++    FILE* fd;
        method=parent.method() #t uint
        if method==M_SBACore_IO_open
            filename_address=addresses[1] #t MemAddress
            filename_bytes=sba_tile.data_store.mget(filename_address) #t Word_List
            filename=sym2str(filename_bytes) #t string
#iv
            puts "CORE FOPEN: HANDLE: #{port} FILENAME: #{filename}"
#ev                        
            if addresses.length==3
                flag_address=addresses[2] #t MemAddress
                flag=sym2str(sba_tile.data_store.mget(flag_address)) #t string
                fd=File.open(filename,flag) #C++ fd=fopen(filename.c_str(),flag.c_str());
            else
                fd=File.open(filename) #C++ fd=fopen(filename.c_str(),"r");
            end
            parent.lookup_table.write(port,fd) #C++ parent.lookup_table.write(port,(Uint64)fd);
            parent.result(port_symbol)
        elsif method==M_SBACore_IO_close
            if parent.lookup_table.count(port)
            	fd=parent.lookup_table.read(port) #C++ fd=(FILE*)(parent.lookup_table.read(port));
            	fd.close() #C++ fclose(fd);
            	parent.result(pass)
            else
            	parent.result(fail)
            end        
        elsif method==M_SBACore_IO_readline 
            if addresses.length == 2
                nbytes_address=addresses[1] #t MemAddress
                nbytes=getValue(sba_tile.data_store.mget(nbytes_address)[0]) #t Word
                if nbytes!=0 
                    raise "CORE IOREAD: reading chunks of #{nbytes} bytes is not yet implemented" #s/raise/std::cerr << /
                end 
            end
            #C++ char* nil=NULL;
            inp = nil #C++ char inp[255]; // the 255 is very ad-hoc
            if (not parent.lookup_table.count(port))
            	warn "#{port}: " #C++ std::cerr << port << ": ";
                inp = readline #C++ fgets(inp,255,stdin);
            else
                fd = parent.lookup_table.read(port) #C++ fd=(FILE*)(parent.lookup_table.read(port));
                if not fd.eof #s/fd.eof/feof(fd)/             
                    inp = fd.readline #C++ fgets(inp,255,fd);
                end
            end
            #C++ 
            puts "CORE READLINE: <#{inp}>" #skip
            if inp!=nil
                inp_sym = string2symbol(inp) #t Word_List # must return as a built-in symbol, so need a function. Should return an empty symbol if inp is nil
                puts inp_sym.inspect #skip
                parent.result(inp_sym)
            else
                emptysymbol = [NIHIL]  #C++ Word_List emptysymbol;emptysymbol.push_back(NIHIL);
                parent.result(emptysymbol)
            end
                # or maybe we need a conditional outside the function
        elsif method==M_SBACore_IO_write
            data_address=addresses[1]  #t MemAddress       
            data_symbol = sba_tile.data_store.mget(data_address) #t Word_List
            #C++ string data;            
            kind=getKind(data_symbol[0]) #t Kind_t
            datatype=getDatatype(data_symbol[0]) #t Datatype_t
			nwords =  getSubtask(data_symbol[0]) #t uint
			ext =  getExt(data_symbol[0]) #t uint
            if (kind==K_B and ext==1 and nwords==0) # means it's an "empty" symbol
                parent.result(eof)
            else
                 if (not parent.lookup_table.count(port))
                     puts "#{port}: #{data_symbol}" #skip 
                     parent.result(fail)
                 else            
                     fd = parent.lookup_table.read(port) #C++ FILE* fd=(FILE*)(parent.lookup_table.read(port));
                     puts "CORE WRITE: #{port}: #{data_symbol}" #skip
					if kind==K_B
# because in 32-bit, type is only 1 bit, so any ext symbol of more than 4 bytes is considered as a string
                    if datatype==T_s or (WORDSZ==32 and nwords>1) 
                        data=sym2str(data_symbol) #t string
                        #C++ fprintf(fd,"%s",data.c_str());
                    elsif datatype==T_i
                        data=sym2int(data_symbol) #t Int           
                        #C++ fprintf(fd,"%d",(int)data);
                    elsif  datatype==T_f
                        data=sym2flt(data_symbol) #t Float            
                        #C++ fprintf(fd,"%f",data);
                    else
                        raise "CORE WRITE: datatype #{datatype}"
                    end          
					else
						raise "can't write out non-K_B values"	#skip
					end
                     fd.puts data #skip
                     parent.result(pass)
                 end
             end
        elsif method==M_SBACore_IO_eof
            #C++ Word_List res;
            if fd.eof #s/fd.eof/feof(fd)/
                res = [ONE] #C++ res.push_back(ONE); 
            else
                res = [ZERO] #C++ res.push_back(ZERO);
            end
            parent.result( res )
        elsif method==M_SBACore_IO_display

        # just because DISPLAY is logically an IO function
            result="" #t string
            for argn in 0..parent.nargs()-1 #t uint
				data=parent.arg(argn) #t Word_List
#                puts data.inspect 
#skip				
				case getDatatype(data[0])
					when T_i
#iv                    
						if @v #skip
							result+=">>>Int: "
						end #skip
#ev                        
						result+="#{getInt(data)}" #skip
					when T_f
#iv                    
						if @v #skip
							result+=">>>Float: " 
						end #skip
#ev                        
						result+="#{getFloat(data)}"  #skip
					when T_c
#iv                    
						if @v #skip
							result+=">>>Char: " 
						end #skip
#ev                        
						result+="#{getChar(data)}" #skip
					when T_s
#iv                    
						if @v #skip
							result+=">>>String: "
						end #skip
#ev                        
						result +="#{getString(data)}" #skip
					else
    		            result+=">>>#{data.inspect}\n" 
				end
#endskip				
=begin #C++
                if (data.size()==1 || data.size()==2) {
                switch (getDatatype(data[0])) {
    				case T_i:
                    #ifdef VERBOSE
                        std::cout << ">>>Int: ";
    				#endif
    				    std::cout << getInt(data) << std::endl;
    				    break;
				    case T_f:
#ifdef VERBOSE
                        std::cout << ">>>Float: ";
#endif
                        std::cout << getFloat(data) << std::endl;
                        break;
                    case T_c:
#ifdef VERBOSE
                        std::cout << ">>>Char: ";
#endif
                        std::cout << getChar(data) << std::endl;
                        break;
                    case T_s:
#ifdef VERBOSE
                        std::cout << ">>>String: ";
#endif
                        std::cout << getString(data) << std::endl;
                        break;				
                    default:
                        std::cout << data[0] << std::endl;
                        if (data.size()==2) {
                            std::cout << data[1] << std::endl;
                            }
                    }
                } else {

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
=end #C++

=begin
=end
            end    			
			# This is the actual display line! Don't comment!
			puts result #skip
            puts pass.inspect
            parent.result( pass ) # result
        else
            raise "CORE IO does not support method #{method}"
        end
    end # of IO
   

    # ----------------------------------------------------------------------------------------------------------------------
   

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------     
#         
# this is a dummy for unused services
def none(sba_tile,parent) #t void (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/    
    #core
    parent.result( [0] ) #C++ Result res; res.push_back((Word)0); parent.result(res);
end        
end #skip
#skipcc
#C++ }} // namespaces       
#endskipcc
#endif // NO_SERVICES


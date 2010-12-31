# ServiceCoreLibrary.rb
#   
# :title: Gannet Service-based SoC project - Service Core Library 
#    
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#// $Id: ServiceCoreLibrary.rb 2532 2009-04-22 16:15:08Z socgroup $
#++

# -----------------------------------------------------------------------------

=begin #inc

#ifndef SYSC
#include <dlfcn.h>
#include <fstream>
#include <sstream>
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/ServiceCore.h" //skipcc
#include "ServiceCoreLibrary.h" //skiph
#include "System.h" //skiph
//#if SEQVM==0
#include "Tile.h" //skiph
#include "ServiceCore.h" //skiph
//#else
//#include "TileVM.h" //skiph
//#include "ServiceCoreVM.h" //skiph
//#endif
// example static service 
//#include "cs_DCT.h" //skipcc
//#include "LoopService.h" //skipcc
#endif
=end #inc 

# This module contains the implementations of all SBA services. 
# Services starting with 'ls_' are language services
module SBA_SCLib
#skip    

@v=(VERBOSE==1)
def SBA_SCLib.mkSymbol(kind,datatype,ext,quoted,task,subtask,name) 
	wkind=(kind << FS_Kind) & F_Kind 
	wdatatype=(datatype << FS_Datatype) & F_Datatype 
	wext=(ext << FS_Ext) & F_Ext 
	wquoted=(quoted << FS_Quoted) & F_Quoted 
	wtask=(task << FS_Task) & F_Task 
	wsubtask=(subtask << FS_Subtask) & F_Subtask 
	wname=(name << FS_Name) & F_Name 
	word=wkind+wdatatype+wext+wquoted+wtask+wsubtask+wname 
	return word
end
def SBA_SCLib.getKind(word) 
	return (word & F_Kind) >> FS_Kind
end
def SBA_SCLib.setKind(word,field) 
	return (word & FN_Kind) + (field << FS_Kind)
end
def SBA_SCLib.getDatatype(word) 
	return (word & F_Datatype) >> FS_Datatype
end
def SBA_SCLib.setDatatype(word,field) 
	return (word & FN_Datatype) + (field << FS_Datatype)
end
def SBA_SCLib.getExt(word) 
	return (word & F_Ext) >> FS_Ext
end
def SBA_SCLib.setExt(word,field) 
	return (word & FN_Ext) + (field << FS_Ext)
end
def SBA_SCLib.getQuoted(word) 
	return (word & F_Quoted) >> FS_Quoted
end
def SBA_SCLib.setQuoted(word,field) 
	return (word & FN_Quoted) + (field << FS_Quoted)
end
def SBA_SCLib.getTask(word) 
	return (word & F_Task) >> FS_Task
end
def SBA_SCLib.setTask(word,field) 
	return (word & FN_Task) + (field << FS_Task)
end
def SBA_SCLib.getStatus(word) #t DS_t (Word)
    return SBA_SCLib.getTask(word)
end
def SBA_SCLib.setStatus(word,status) #t Word (Word;DS_t)
    return SBA_SCLib.setTask(word,status)
end
def SBA_SCLib.getSubtask(word) 
	return (word & F_Subtask) >> FS_Subtask
end
def SBA_SCLib.setSubtask(word,field) 
	return (word & FN_Subtask) + (field << FS_Subtask)
end
def SBA_SCLib.getName(word) 
	return (word & F_Name) >> FS_Name
end
def SBA_SCLib.setName(word,field) 
	return (word & FN_Name) + (field << FS_Name)
end

def SBA_SCLib.getValue(word) #t Value_t (Word)
    return (word & F_Value) >> FS_Value
end
def SBA_SCLib.setValue(word,field) #t Word (Word;Value_t)
    return (word & FN_Value) + (field << FS_Value)
end
def SBA_SCLib.getNSymbols(word) #t NSymbols_t (Word)
    return (word & F_NSymbols) >> FS_NSymbols
end
def SBA_SCLib.setNSymbols(word,field) #t Word (Word;NSymbols_t)
    return (word & FN_NSymbols) + (field << FS_NSymbols)
end

def SBA_SCLib.getInt(words) #t Int (Word_List)   
    #C++ Int int_result;    
    if getExt(res_symbol)==1
        result=words[1]
        int_result=(result>2**(WORDSZ-1))?(result-2**WORDSZ):result #C++ int_result=(Int)result;        
    else
        result=getValue(words[0]) 
        one=1 #t Word
        int_result= (result>(one<< (FB_Value-1)))?(result-(one<< FB_Value)):result              
    end                    
    result=Integer(int_result) #skip
    return result
end    
    
def SBA_SCLib.getUInt(words) #t Word (Word_List)   
    #C++ Word result;    
    if getExt(words[0])==1
        result=words[1]
    else
        result=getValue(words[0]) 
    end                    
    return result
end 

def SBA_SCLib.getCodeAddress(word) #t CodeAddress
    # extract page form word
    page=(word & F_CodePage)>> FS_Task #t Word
    # extract address from Work
    address=(word & F_CodeAddress) >> FS_Subtask #t Word    
if VM==1
    service=(word & F_Service)>>FS_Name #t Word
    code_address=(service << FS_Service)+(page << FS_CodePage)+address #t Word    
else # VM==0    
    code_address=(page << FS_CodePage)+address #t Word
end # VM    
#iv    
    puts "Service: #{service} | Page: #{page} | Address: #{address} => #{code_address}"
#ev    
	return code_address
end

def SBA_SCLib.mkHeader(packet_type,prio,redir,length,to,return_to,ack_to,return_as) 
	wpacket_type=(packet_type << FS_Packet_type) & F_Packet_type 
	wprio=(prio << FS_Ctrl) & F_Ctrl 
	wredir=(redir << FS_Redir) & F_Redir 
	wlength=(length << FS_Length) & F_Length 
	wto=(to << FS_To) & F_To 
	wreturn_to=(return_to << FS_Return_to) & F_Return_to 
	w1=wpacket_type+wprio+wredir+wlength+wto+wreturn_to 
	return [w1,ack_to,return_as] 
end
def SBA_SCLib.getPacket_type(header) 
	w1=header[0] 
	return (w1 & F_Packet_type) >> FS_Packet_type
end
def SBA_SCLib.setPacket_type(header,field) 
	modheader=[]  
	w0=(header[0] & FN_Packet_type) + (field << FS_Packet_type) 
	modheader.push(w0) 
	modheader.push(header[1]) 
	modheader.push(header[2]) 
	return modheader
end
def SBA_SCLib.getCtrl(header) 
	w1=header[0] 
	return (w1 & F_Ctrl) >> FS_Ctrl
end
def SBA_SCLib.setCtrl(header,field) 
	modheader=[]  
	w0=(header[0] & FN_Ctrl) + (field << FS_Ctrl) 
	modheader.push(w0) 
	modheader.push(header[1]) 
	modheader.push(header[2]) 
	return modheader
end
def SBA_SCLib.getRedir(header) 
	w1=header[0] 
	return (w1 & F_Redir) >> FS_Redir
end
def SBA_SCLib.setRedir(header,field) 
	modheader=[]  
	w0=(header[0] & FN_Redir) + (field << FS_Redir) 
	modheader.push(w0) 
	modheader.push(header[1]) 
	modheader.push(header[2]) 
	return modheader
end
def SBA_SCLib.getLength(header) 
	w1=header[0] 
	return (w1 & F_Length) >> FS_Length
end
def SBA_SCLib.setLength(header,field) 
	modheader=[]  
	w0=(header[0] & FN_Length) + (field << FS_Length) 
	modheader.push(w0) 
	modheader.push(header[1]) 
	modheader.push(header[2]) 
	return modheader
end
def SBA_SCLib.getTo(header) 
	w1=header[0] 
	return (w1 & F_To) >> FS_To
end
def SBA_SCLib.setTo(header,field) 
	modheader=[]  
	w0=(header[0] & FN_To) + (field << FS_To) 
	modheader.push(w0) 
	modheader.push(header[1]) 
	modheader.push(header[2]) 
	return modheader
end
def SBA_SCLib.getReturn_to(header) 
	w1=header[0] 
	return (w1 & F_Return_to) >> FS_Return_to
end
def SBA_SCLib.setReturn_to(header,field) 
	modheader=[]  
	w0=(header[0] & FN_Return_to) + (field << FS_Return_to) 
	modheader.push(w0) 
	modheader.push(header[1]) 
	modheader.push(header[2]) 
	return modheader
end

	    def SBA_SCLib.getAck_to(header) 
        return header[1]
    end

	    def SBA_SCLib.getReturn_as(header) 
        return header[2]
    end

    def SBA_SCLib.setReturn_as(header,field) 
         modheader=[]  
         modheader.push(header[0]) 
         modheader.push(header[1]) 
         modheader.push(field) 
         return modheader
    end

    def SBA_SCLib.setAck_to(header,field) 
         modheader=[]  
         modheader.push(header[0]) 
         modheader.push(field) 
         modheader.push(header[2]) 
         return modheader
    end

	def SBA_SCLib.getType(header) 
		return SBA_SCLib.getPacket_type(header)
	end

	def SBA_SCLib.setType(header,field) 
		SBA_SCLib.setPacket_type(header,field)
	end

    def SBA_SCLib.mkPacket(header,payload) 
        packet=[] 
        for w in header 
            packet.push(w) 
        end
        for w in payload 
            packet.push(w) 
        end
        return packet
    end
    
    def SBA_SCLib.getHeader(packet) 
    	header=[] 
    	header.push(packet[0])  
    	header.push(packet[1])  
    	header.push(packet[2])  
        return header 
    end    

    def SBA_SCLib.setHeader(packet,header) 
        npacket=[] 
        for w in header 
            npacket.push(w) 
        end
        payload=SBA_SCLib.getPayload(packet) 
        for w in payload 
            npacket.push(w) 
        end
        return npacket
    end
        
    def SBA_SCLib.getPayload(packet) 
        pl=[] 
        i=0 
        for w in packet 
            pl.push(w) unless i<3 
            i+=1
        end
        return pl
    end   
     
    def SBA_SCLib.getValues(addresses,sba_tile) 
        vl=[] 
        for a in addresses
            wl= sba_tile.data_store.mget(a)
            vl.push(wl)            
        end
        return vl
    end       
    
    def to_signed_int(num)
        if num>2**63
            return num-2**64
        else 
            return num
        end
    end

    def to_float(w)
        if WORDSZ==64
            float=[w].pack("Q").unpack("G")[0]
        else # WORDSZ==32
            float=[w].pack("N").unpack("g")[0]
        end # WORDSZ
        return float
    end
    
    def to_float_list(wl)
        numlist=[]
        for num in wl
        if WORDSZ==64
            fnum=[num].pack("Q").unpack("G")[0]
        else # WORDSZ==32
            fnum=[num].pack("N").unpack("g")[0]
        end # WORDSZ
            numlist.push(fnum)
        end 
        return numlist
    end

    def to_signed_int_list(wl) 
        numlist=[] 
        for w in wl 
            if w>2**63 
                sw=w-2**64
            else 
                sw=w
            end
            numlist.push(sw) 
        end
        return numlist
    end     

            
    def SBA_SCLib.ppSymbol(w) 
        os="" 
        if @v 
        os="#{kind_l(SBA_SCLib.getKind(w))}:#{SBA_SCLib.getDatatype(w)}:#{SBA_SCLib.getExt(w)}:#{SBA_SCLib.getQuoted(w)}:#{SBA_SCLib.getTask(w)}:#{SBA_SCLib.getSubtask(w)}:#{SBA_SCLib.getName(w)} "
        else
        os="#{SBA_SCLib.getKind(w)}:#{SBA_SCLib.getDatatype(w)}:#{SBA_SCLib.getExt(w)}:#{SBA_SCLib.getQuoted(w)}:#{SBA_SCLib.getTask(w)}:#{SBA_SCLib.getSubtask(w)}:#{SBA_SCLib.getName(w)}"
        end
		return os
    end
    
#    def SBA_SCLib.ppHeader(wl) 
#        w1=wl[0] 
#        w2=wl[1] 
#        w3=wl[2] 
#        if @v
#        os="#{packettype_l(SBA_SCLib.getPacket_type(wl))}:#{SBA_SCLib.getCtrl(wl)}:#{SBA_SCLib.getRedir(wl)}:#{SBA_SCLib.getLength(wl)}:#{num2name(SBA_SCLib.getTo(wl))}:#{num2name(SBA_SCLib.getReturn_to(wl))}\n#{w2}\n#{w3} (#{ppSymbol(w3)})" 
#        else
#        os="#{SBA_SCLib.getPacket_type(wl)}:#{SBA_SCLib.getCtrl(wl)}:#{SBA_SCLib.getRedir(wl)}:#{SBA_SCLib.getLength(wl)}:#{num2name(SBA_SCLib.getTo(wl))}:#{SBA_SCLib.getReturn_to(wl)}\n#{w2}\n#{w3} (#{ppSymbol(w3)})" 
#        end
#		return os
#    end
    def SBA_SCLib.ppHeader(wl) #t string (Header_t)
        w2=wl[1] #t Word
        w3=wl[2] #t Word

#C++	ostringstream outs;
#C++	outs << (uint)getPacket_type(wl) << ":" << (uint)getCtrl(wl) <<":"<< (uint)getRedir(wl) <<":"<<(uint)getLength(wl)<<":"<< (uint)getTo(wl) <<":"<<(uint)getReturn_to(wl) <<"\n"<< (uint)w2<<"\n"<<(uint)w3;
        if @v
        os="#{packettype_l(getPacket_type(wl))}:#{getCtrl(wl)}:#{getRedir(wl)}:#{getLength(wl)}:#{num2name(getTo(wl)).downcase}:#{num2name(getReturn_to(wl)).downcase}\n#{w2}\n#{ppSymbol(w3)} (#{w3})" #skip
        else 
        os="#{getPacket_type(wl)}:#{getCtrl(wl)}:#{getRedir(wl)}:#{getLength(wl)}:#{getTo(wl)}:#{getReturn_to(wl)}\n#{w2}\n#{w3}" #skip
        end 
        #C++	string os=outs.str();
		return os
    end    
    
    def SBA_SCLib.ppPayload(wl) #t string (Word_List)
            os="" #C++ ostringstream outs;
            for i in 0..wl.length-1 #t uint
#C++	        outs << ppSymbol(wl[i]) <<"\n";	
                os+="#{ppSymbol(wl[i])}\n" #skip
            end
#C++	        string os=outs.str();       
		return os
    end
        
    def SBA_SCLib.ppPacket(wl) 
        
        os="#{SBA_SCLib.ppHeader(SBA_SCLib.getHeader(wl))}\n------------\n#{SBA_SCLib.ppPayload(SBA_SCLib.getPayload(wl))}\n" 
		return os    
    end

    def SBA_SCLib.pretty(arg)
        return "#{arg}"
    end

#endskip


if WORDSZ==64    
    # Helper functions for FP ALU
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

    
    
#ifndef NO_SERVICES
    # --------------------------------------------------------------------------
    # Helper functions for IO
    # --------------------------------------------------------------------------
    def SBA_SCLib.string2symbol(str) #t Word_List (string)
    # pad str with null bytes
        null=[0].pack("c") #skip
        npad=NBYTES - (str.length % NBYTES) #t uint 
        nwords=(str.length+npad)/NBYTES #t uint
        str=str+null * npad #C++ str.resize(NBYTES*nwords,0);
        sheader=mkSymbol(K_Q,T_s&1,1,1,0,nwords,npad)
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
#skip
if WORDSZ==64
        # 8 bytes, we don't do Unicode
        wordstr=str.unpack("Q"*nwords)      
else # WORDSZ==32
        # 4 bytes, idem
        wordstr=str.unpack("N"*nwords) 
end # WORDSZ  
        puts wordstr.inspect 
        sym=[sheader]+wordstr 
#endskip
        return sym
    end

    def SBA_SCLib.sym2str(sym) #t string (Word_List)
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

    def SBA_SCLib.sym2int(sym) #t Int (Word_List)
        return getInt(sym)
    end
    
    def SBA_SCLib.sym2uint(sym) #t Word (Word_List)
        return getUInt(sym)
    end
    
    def SBA_SCLib.sym2bool(sym) #t bool (Word_List)
        return getUInt(sym)==1
    end
                    
    def SBA_SCLib.sym2flt(sym) #t float (Word_List)
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

    # this is a dummy to make sure the GATEWAY is registered
    def SBA_SCLib.sba_GATEWAY(sba_tile,parent,addresses) #t Result (na;Base::ServiceCore*;MemAddresses&)
        return 1 #C++ Result res; res.push_back((Word)1); return res;
    end



#ifndef NO_SERVICES    
    # --------------------------------------------------------------------------
    #skip
    def SBA_SCLib.display(sba_tile,parent,addresses)
        print "#{parent.service} CORE (#{parent.current_subtask}): \n"
        result=''
        for address in addresses
#            if SYM==0
#                result+=">>>#{sba_tile.data_store.get(address)}\n"             
#            else # SYM==1
                result+=">>>#{sba_tile.data_store.mget(address).inspect}\n"          
#            end # SYM   
        end    
        puts result
        return [1] # result
    end # of display
    #endskip
    # --------------------------------------------------------------------------
    # Helper for ALU
    def SBA_SCLib.div(m,n) #t Int (Int;Int)
        q=0 #t Int
        if FP==0
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
        else # FP==1
            q=m/n
        end  # FP 
        return q*sn*sm
    end
    
    # --------------------------------------------------------------------------
    # To make the ALU type-aware (int or float), we need to get the types of the arguments.
    # So we need the labels of the arguments
    # But this is a silly approach: we should simply have an FP ALU separately!
    
    def SBA_SCLib.ls_ALU(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
#iv        
        puts "ALU CORE: processing subtask #{parent.current_subtask}"
#ev
        #C++ Word_List result_list;
        operation=parent.opcode #t Uint

# now if result is an extended Symbol, and assuming we us a single word for numbers,
# we could just take the next element:

        # if @v #skip
#iv        
    puts "ALU (#{parent.service}) CORE: #{addresses.length} addresses"
#ev     
#    if addresses.length==0 #skip
#      exit(0) #skip
#    end  #skip
#    puts addresses.inspect #skip
	address=addresses[0] #t MemAddress
 	res_symbol=sba_tile.data_store.mget(address)[0] #t Word
#C++ Word result; 	
#C++ Int int_result; 	
 	if getExt(res_symbol)==1
        result=sba_tile.data_store.mget(address)[1]
        int_result=(result>2**(WORDSZ-1))?(result-2**WORDSZ):result #C++ int_result=(Int)result;        
    else
        result=getValue(res_symbol) # FIXME: this assumes the ALU is WORDSZ only, i.e. number of words in ext symbol==1
        one=1 #t Word
        int_result= (result>(one<< (FB_Value-1)))?(result-(one<< FB_Value)):result              
    end                    
                result=Integer(int_result) #skip
#iv
                puts "ALU CORE: arg 1: Found int #{result} (#{T_i}) @ #{address}"   
#ev                
        if operation==A_not
            result=1-result
        else
            ii=0; #t int
            for address in addresses #t MemAddresses
                ii+=1
                if ii>1
                tres_symbol=sba_tile.data_store.mget(address)[0] #t Word
                #C++ Word tres;
                #C++ Int int_tres;   
                if getExt(tres_symbol)==1
                    tres=sba_tile.data_store.mget(address)[1]
                    int_tres=(tres>2**(WORDSZ-1))?(tres-2**WORDSZ):tres #C++ int_tres=(Int)tres;                    
                else
                    tres=getValue(tres_symbol)
                    one=1 #t Word
                    int_tres= (tres>(one<<(FB_Value-1)))?(tres-(one<< FB_Value)):tres  
                end    
                
                tres=Integer(int_tres) #skip            
#iv
                        puts "ALU CORE: arg #{ii}: Found int #{tres} (#{T_i}) @ #{address}"   
#ev                        
                case operation
                when A_plus
                    puts "ALU CORE operation: +" if @v #skip
                    result=result+tres #C++ int_result+=int_tres;                    
                when A_minus
                    puts "ALU CORE operation: -" if @v #skip
                    result=result-tres #C++ int_result-=int_tres; 
                when A_times  
                    puts "ALU CORE operation: *" if @v #skip 
                    result=result*tres #C++ int_result*=int_tres;
                when A_over
                    puts "ALU CORE operation: /" if @v #skip
                    result=SBA_SCLib.div(result,tres) #C++ int_result=div(int_result,int_tres);
                    # result=result/tres #/
                when A_lt
                    puts "ALU CORE operation: <" if @v #skip
                    result=(result<tres)?1:0 #C++ int_result=(int_result<int_tres)?1:0;
                when A_gt
                    puts "ALU CORE operation: >" if @v #skip
                    result=(result>tres)?1:0 #C++ int_result=(int_result>int_tres)?1:0;
                when A_eq
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
        return result_list
    end # of ALU
# ----------------------------------------------------------------------------------------------------
# Five years to see the light :-( 
# Here's the FP ALU -- Ruby only
            
def SBA_SCLib.ls_FP_ALU(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core

#iv        
    puts "FP ALU CORE: processing subtask #{parent.current_subtask}"
#ev
                    
    #C++ Word_List result_list;

    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
    operation=parent.opcode #t Uint

# now if result is an extended Symbol, and assuming we us a single word for numbers,
# we could just take the next element:

    # if @v #skip
#iv        
    puts "FP ALU (#{parent.service}) CORE: #{addresses.length} addresses"
#ev     
address=addresses[0] #t MemAddress
result=sba_tile.data_store.mget(address)[0] #t Word
if getExt(result)==1
    result=sba_tile.data_store.mget(address)[1]
end    
res_symbol=sba_tile.data_store.mget(address)[0] #t Word    

#C++ result_list.push_back(res_symbol);

if WORDSZ==64                      
    #C++ double flt_result=word2dbl(result); //0;
    flt_result=[result].pack("Q").unpack("G")[0] #skip
else # WORDSZ==32
    #C++ float flt_result=word2flt(result);//0;
    flt_result=[result].pack("N").unpack("g")[0] #skip 
end # WORDSZ
    result=flt_result #skip
    puts "FP ALU CORE: Found double #{result} (#{getDatatype(res_symbol)}<>#{T_i})" if @v  #skip

    if operation==A_not
        result=1-result
    else
        ii=0; #t int
        for address in addresses #t MemAddresses
            ii+=1
            if ii>1
                tres_symbol=sba_tile.data_store.mget(address)[0] #t Word
                tres=sba_tile.data_store.mget(address)[1] #t Word

                if WORDSZ==64
                    flt_tres=[tres].pack("Q").unpack("G")[0] #C++ double flt_tres=word2dbl(tres);
                else # WORDSZ==32
                    flt_tres=[tres].pack("N").unpack("g")[0] #C++ float flt_tres=word2flt(tres);
                end # WORDSZ
                    tres=flt_tres #skip
                    puts "FP ALU CORE: Found double #{tres}" if @v #skip
                    
            case operation
            when A_plus
                puts "FP ALU CORE operation: +" if @v #skip
                result=result+tres #C++ flt_result+=flt_tres;                    
            when A_minus
                puts "FP ALU CORE operation: -" if @v #skip
                result=result-tres #C++ flt_result-=flt_tres; 
            when A_times  
                puts "FP ALU CORE operation: *" if @v #skip 
                result=result*tres #C++ flt_result*=flt_tres;
            when A_over
                puts "FP ALU CORE operation: /" if @v #skip
                result=result/tres #C++ flt_result=flt_result/flt_tres;
                # result=result/tres #/
            when A_lt
                puts "FP ALU CORE operation: <" if @v #skip
                result=(result<tres)?1:0 #C++ flt_result=(flt_result<flt_tres)?1:0;
            when A_gt
                puts "FP ALU CORE operation: >" if @v #skip
                result=(result>tres)?1:0 #C++ flt_result=(flt_result>flt_tres)?1:0;
            when A_eq
                puts "FP ALU CORE operation: ==" if @v #skip
                result=(result==tres)?1:0 #C++ flt_result=(flt_result==flt_tres)?1:0;
                #C++ break;}
            else #C++ default:
                raise "Unknown FP ALU CORE service: #{operation}" 
                #C++   exit(0);
            end #;
        end
        end
    end
            #iv
            puts "FP ALU CORE RESULT (double): #{result}"  if @v #skip      
            #ev
#skip            
if WORDSZ==64               
             uint64_result=[result].pack("G").unpack("Q")[0]
             result=uint64_result #C++ result=dbl2word(flt_result);
             
else # WORDSZ==32
            uint32_result=[result].pack("g").unpack("N")[0] #WV: untested!
            result=uint32_result #C++ result=flt2word(flt_result);
end # WORDSZ
#endskip                
    #iv    
    puts "FP ALU CORE RESULT: (uint#{WORDSZ}) #{result}" 
    puts "FP ALU (#{parent.service}) CORE (#{parent.current_subtask}):  result: #{result}"
    #ev

    result_list=[res_symbol,result]  #C++ result_list.push_back(result);
    
    return result_list    
end # of FP_ALU    
        
    # --------------------------------------------------------------------------    
    # This is a simple proof-of-concept example of a service that does not return in "zero time". The service takes count_upto
    # "clock cycles" and returns the sum of all counts
    # "clock cycles" are really parse cycles, i.e. the status will toggle between CS_ready and CS_busy
    # With concurrent tasks this might behave weirdly...    
    # This is a way to emulate concurrency: the core is called at every parse cycle and returns immediately, holding its status to CS_busy
    # WV29112010: it does consume time all right but it is not multitasking. For that I guess I'll meed "CS_managed" and "STS_blocked"
    def SBA_SCLib.ls_COUNTER(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
        address=addresses[0] #t MemAddress
        address1=addresses[1] #t MemAddress
        count_upto=getUInt( sba_tile.data_store.mget(address) ) #t Word
        inst=getUInt( sba_tile.data_store.mget(address1) ) #t Word
        puts "INST:#{inst}"
        res_symbol=EXTSYM #C++ const Word res_symbol = EXTSYM;
        #C++ result_list.push_back(res_symbol);
        if parent.state_register[0]<count_upto
            parent.state_register[0]+=1            
            parent.state_register[1]+=parent.state_register[0]
            puts "ITER #{parent.state_register[0]}: #{parent.state_register[1]}" #skip
            parent.core_status=CS_busy
        end
        result_list=[res_symbol,parent.state_register[1]]  #C++ result_list.push_back(parent.state_register[1]);
        return result_list
    end # of COUNTER
    
    # --------------------------------------------------------------------------
    # COUNTDOWN is an example of the use of the state register
    # state register 0 containts the actual state: 0 before initialisation, 1 during countdown
    # state register 1 stores the actual counter when in state 0 and decrements it when in state 1
    # Although this looks like a persistent task, the task does not "spend time" like a thread would.   
    # All it does is store some state which is used on the next invocation.
    def SBA_SCLib.ls_COUNTDOWN(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
        puts "COUNTDOWN called: #{parent.state_register[1]}" #skip
        address=addresses[0] #t MemAddress
        count_downfrom=sba_tile.data_store.mget(address)[1] #t Word
        res_symbol=EXTSYM #C++ const Word res_symbol = EXTSYM;
        #C++ result_list.push_back(res_symbol);
        if parent.state_register[0]==0
            parent.state_register[1]=count_downfrom            
            parent.state_register[0]=1
        else
            parent.state_register[1]-=1
            if parent.state_register[1]==0
            parent.state_register[0]=0
            end
        end

        result_list=[res_symbol,parent.state_register[1]]  #C++ result_list.push_back(parent.state_register[1]);
        puts "COUNTDOWN returns #{result_list.inspect}" #skip
        return result_list
    end # of COUNTDOWN
    # --------------------------------------------------------------------------
    # FIB is a helper to compute the Fibonacci series, similar to COUNTDOWN
    def SBA_SCLib.ls_FIB(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        # The symbol for a 32-bit signed int is a constant
        res_symbol=EXTSYM  #C++ const Word res_symbol = EXTSYM;        
        #C++ Word_List result_list;
        #C++ result_list.push_back(res_symbol);
        puts "FIB called: #{parent.state_register.inspect}" #skip
        opaddr=addresses[0] #t MemAddress
        op=sba_tile.data_store.mget(opaddr)[1] #t Word

        numval=0 #t Word
        if op==0
                num1addr=addresses[1] #t MemAddress
        num1=sba_tile.data_store.mget(num1addr) #t Word_List
        num2addr=addresses[2]  #t MemAddress
        num2=sba_tile.data_store.mget(num2addr) #t Word_List       
            parent.state_register[0]=num1[1]
            parent.state_register[1]=num2[1]
        elsif op==1
            numval=parent.state_register[0]
        elsif op==2
            numval=parent.state_register[1]
        else
            raise "first arg value must be 0 (store 2 numbers), 1 (read 1st number) or 2 (read 2nd number)" #skip
        end
        result_list=[res_symbol,numval]  #C++ result_list.push_back(numval);
        puts "FIB returns #{result_list.inspect}" #skip
        return result_list
    end # of FIB
            
    # This is one step further: the service core launches a thread and monitors a state register. The thread writes 2 to the state reg
    # when it's done. As long as the reg is 1, core status stays CS_busy
    # This is not portable as such to C++, neither is there much point in doing it for the VM;
    # but this shows that a number of HW service cores could run in parallel. The VM Is effectively
    # polling the service cores on every iteration of the main loop
    # NOTE 10/01/2009: The C++ code doesn't work as the main program exits because the tread is detached
    def SBA_SCLib.ls_THREAD(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
        address=addresses[0] #t MemAddress
        count_upto=sba_tile.data_store.mget(address)[1] #t Word
        res_symbol=sba_tile.data_store.mget(address)[0] #t Word

#C++    parent.state_register[1]=count_upto; 
        
        if parent.state_register[0]==0
            parent.core_status=CS_busy
            parent.state_register[0]==1
            # launch thread
#iv
            puts "THREAD: Starting core thread..."
#ev                        
#skip            
            Thread.new do
                sleep 1 # i.e. do work
                parent.state_register[1]=7
                parent.state_register[0]=2
            end
#endskip
                       
=begin #C++
#ifdef THREADED_CORE
                pthread_t tid;
                pthread_attr_t attr;
                void* tstatus;
                
                pthread_attr_init(&attr);
                pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
                pthread_create(&tid, &attr, SCLib::run_dct, (void*)parent_ptr);        
#endif
=end #C++
                                                 
        elsif parent.state_register[0]==1
            #iv
                        puts "THREAD: Core is busy ..."
            #ev 
            parent.core_status=CS_busy
        else
        #iv
                    puts "THREAD: Core has finished ..."
        #ev 
        end                                            
        result_list=[res_symbol,parent.state_register[1]]  #C++ result_list.push_back(count_upto);
        return result_list
    end # of THREAD

       
        
    #WV02092008: for testing of multi-threaded core. Not backward-compatible
    # This service returns its 2nd argument after a delay given by the first argument (in seconds)
    # (delay '5 '7) returns 7 after 5 seconds
    # It's a kind of "proof of concept" for exploring threads, and Ruby-only
    def SBA_SCLib.ls_DELAY(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
#        puts "DELAY CORE #{parent.tid} called: #{parent.state_register[0]}"
        # The symbol for a 32-bit signed int is a constant
        res_symbol=EXTSYM  #C++ const Word res_symbol = EXTSYM;        
        #C++ Word_List result_list;
        #C++ result_list.push_back(res_symbol);        
        delay_address=addresses[0] #t MemAddress
        delay_val=sba_tile.data_store.mget(delay_address)[1] #t Word
        res_address=addresses[1] #t MemAddress
        res_val=sba_tile.data_store.mget(res_address)[1] #t Word

        if parent.state_register[0]==0
        #iv    
        puts "DELAY (#{parent.service}) CORE (#{parent.tid},#{parent.current_subtask}) VAL: #{delay_val}" #skip
        #ev
            parent.core_status=CS_busy
            parent.state_register[0]=1
            parent.state_register[1]=0
            # launch thread
#skip
            Thread.new do
                sleep delay_val
                parent.state_register[1]=res_val
                parent.state_register[0]=2
            end
#endskip

#C++        // Using POSIX threads, purely for experimenting. We really want HW threads of course

        elsif parent.state_register[0]==1
            parent.core_status=CS_busy
        else 
            parent.core_status=CS_done            
            parent.state_register[0]=0
        #iv    
        puts "DELAY (#{parent.service}) CORE (#{parent.tid},#{parent.current_subtask}) RESULT: #{parent.state_register[1]}" #skip
        #ev
        end
        result_list=[res_symbol,parent.state_register[1]]  #C++ result_list.push_back(parent.state_register[1]);
        return result_list
    end # of DELAY     

    # This is a simple model for a HW "thread".
    # When state_register[0]==0, we write the input data for the HW core to a fifo
    # and we set state_register[0]=1
    # As long as state_register[0]==1, the core is CS_busy
    # When state_register[0]==2, we read the content of state_register[1] 
    # and we set state_register[0]=0
    def SBA_SCLib.ls_HWTHREAD(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
#        sym_address=addresses[0] #t MemAddress
        res_data=0 #t Word
#        res_symbol=sba_tile.data_store.mget(sym_address)[0] #t Word

        if parent.state_register[0]==0
            parent.core_status=CS_busy
            parent.state_register[0]==1
            input_data=[] #C++ Word_List input_data;
            for address in addresses #t MemAddresses
                input_data.push(sba_tile.data_store.mget(address)[1])
            end

### Here we need to insert the code to communicate with the HW core via IPIF FIFO and status register ###            
            
#            Thread.new do
#                sleep 1
                parent.state_register[1]=7188
                parent.state_register[0]=2
#            end
        elsif parent.state_register[0]==1
            parent.core_status=CS_busy
        else # must be 2, reset it        
            parent.state_register[0]=0
            # and get the data
            res_data=parent.state_register[1]
        end        
        result_list=[res_symbol,res_data]  #C++ result_list.push_back(res_data);
        return result_list
    end # of HWTHREAD 
    # --------------------------------------------------------------------------
    # This is an object for use inside ls_SELF_INTERRUPTING_TASK
    # This is the "new way" for multicore Gannet
    # Simply use an object with methods, and generate a wrapper "service function" at compile time
    # See also the "API" in ServiceCore.rb
    # My main issue is that "suspend()" is not so clear a name, as it does not imply a new iteration
    # The "proper" name is "repost"; maybe "restart" is good? Or "relaunch" or "respawn"?
#skip    
    class LoopService
        attr_accessor :lcur,:lval
        def initialize (_p,_lstart,_lstop,_lval)
            @lstart=_lstart
            @lstop=_lstop
            @lcur=_lstart
            @lval=_lval
            @sys=_p
        end
        def loop()           
                @lval+=@lcur
                @lcur+=1
            if !done()
                @sys.iterate()
            end
        end         
        def current()            
            puts "LOOP VALUE: #{@lval}"
            if !done()
                return @sys.iterate(@lval)
            else
                return @lval   
            end
            
        end
        def done()
            return (@lcur>=@lstop)
        end
    end

    def SBA_SCLib.ls_SELF_INTERRUPTING_TASK(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
         
        operation=parent.opcode #t uint    
        result_list=[] #t Word_List 
        res_symbol=EXTSYM  #C++ const Word res_symbol = EXTSYM;
        res_data=0 #t Word
        lstart=0 #t Word
        lstop=9 #t Word
        
        # Instead of parent.init we could also have an init method for the service
        # in the Gannet-C code this might be the service constructor, so INITIALIZE or NEW is good too
        # so A_INIT  would be called once, resulting in creation of the object
        # In fact, a Gannet-C service is an interface, so we could use the SC_ name as the initialize method
        # So what we would have is 
        # when SC_LOOPSERVICE 
        #     service=LoopService.new(parent,lstart,lstop,res_data)
        #C++ LoopService* service_ptr;
       
        if parent.init()
            # create service object           
            service=LoopService.new(parent,lstart,lstop,res_data) #C++ service_ptr = new LoopService(parent_ptr,lstart,lstop,res_data);
            parent.store(service) #s/service/service_ptr/
        else
            # load service object
            service=parent.load() #C++ service_ptr = (LoopService*)parent.load();
        end
        #C++ LoopService& service= *service_ptr;
        case operation 
        when SC_ITERATOR
            puts "ITERATOR" #skip
        when A_ITERATOR_LOOP
            puts "LOOP COUNTER: #{service.lcur}" #skip
            service.loop()
        when A_ITERATOR_VALUE
            res_data=service.current()
        else #C++ break; } default:
            raise "Unknown service: #{operation}"  #skip
            #C++   exit(0);
        end #;
        
        if !service.done()     
            # store service object. I wonder if I need to do this only once?
            # Also, maybe we can merge suspend and store
            
#            parent.suspend()
            return result_list 
        else
            puts "RESULT: #{res_data}"
            result_list=[res_symbol,res_data]  #C++ result_list.push_back(res_data);
            #C++ delete(service_ptr);
            return result_list
        end
    end # of SELF_INTERRUPTING_TASK

    # --------------------------------------------------------------------------
    # ImgBlock is a 4x128-Word Word_List masquerading as a 128x128 matrix of bits
    class ImgBlock
        def initialize(_imgblock,_bsz)
            @bsz=_bsz
            @block=_imgblock
        end
        def write(x,y,b)
            # in which word is this?
            word = ( ( x - x % WORDSZ ) / WORDSZ ) + y * ( @bsz / WORDSZ )
            bitpos = x % WORDSZ
            @block[word] = (@block[word] & (F_Symbol-(1<<bitpos)))+(b<<bitpos)        
        end
        
        def read(x,y)
            word = ( ( x - x % WORDSZ ) / WORDSZ ) + y * ( @bsz / WORDSZ )
            bitpos = x % WORDSZ
            return (@block[word]>>bitpos)&0x1         
        end
        # these methods return a list of @bsz/WORDSZ words
        def row(r)
            rw=[]
            for w in r*(@bsz/WORDSZ) .. (r+1)*(@bsz/WORDSZ)-1
                rw.push(@block[w])
            end
            return rw
        end
        
        def col(c)
            cl=[]
            w=0
            for r in 0..@bsz-1
                bit=read(r,c)
                rs=r%WORDSZ
                w+=bit<<rs
                if rs==WORDSZ-1
                    cl.push(w)
                    w=0
                end
            end
            return cl
        end
    end    
    
class ConwayLife
#    attr_accessor 
    def initialize ()
        @new_imgblock = ImgBlock.new([],_bsz)
        @crow=1
        @bsz=_bsz
    end
    def init(_imgblock,_bsz)
        @imgblock = ImgBlock.new(_imgblock,_bsz)
    end
#    def calc_core()           
#        for ccol in 1..@bsz-2
#            # calculate new values for crow and ccol
#        end
#        @crow+=1
#        if @crow<@bsz-2
#            @sys.iterate()
#        end
#    end
    # To calculate the core values we don't need neighbouring edges
    def calc_core()
        for crow in 1..@bsz-2
            for ccol in 1..@bsz-2
                life_rules_core(crow,ccol)                 
            end
        end
    end    
    # FIXME: for the 
    def calc_edges(tlc,trow,trc,rcol,brc,brow,blc,lcol)        
       for crow in 1..@bsz-2
           # left edge
           life_rules(crow,0)  
           # right edge
           life_rules(crow,@bsz-1)
       end       
       for ccol in 0..@bsz-1
           # top row
           life_rules(0,col)
           # bottom row
           life_rules(@bsz-1,col)
       end 
    end
    
    # Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    # Any live cell with two or three live neighbours lives on to the next generation.
    # Any live cell with more than three live neighbours dies, as if by overcrowding.
    # Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction
    def life_rules_core(x,y)
        if n_neighbours_core(x,y)<2 or n_neighbours_core(x,y) >3
            @new_imgblock.write(x,y,0)
        elsif n_neighbours_core(x,y)==3
            @new_imgblock.write(x,y,1)
        end
    end 
    
    def n_neighbours_core(x,y)
        n=0;
        for i in x-1..x+1
            for j in y-1 .. y+1
                if i!=x and j!=y and @imgblock.read(i,j)==1
                    n+=1
                end
            end
        end
        return n
    end   
             
    def brc()
        return [@imgblock.read(@bsz-1,@bsz-1)]
    end
    
    def brow()
        return @imgblock.row(@bsz-1)
    end
    
    def blc()
        return [@imgblock.read(@bsz-1,0)]
    end
        
    def lcol()
        return @imgblock.col(0)
#        lc=[]
#        for r in 0..@bsz-1
#            lc[r]=  @imgblock[r][0]
#        end
#        return lc
    end
    
    def tlc()
        return [@imgblock.read(0,0)]
    end
    
    def trow()
        return @imgblock.row(0)
    end
        
    def trc()
        return [@imgblock.read(0,@bsz-1)]
    end
        
    def rcol()
        return @imgblock.col(@bsz-1)
#        rc=[]
#        for r in 0..@bsz-1
#            rc[r]=  @imgblock[r][@bsz-1]
#        end
#        return rc        
    end
    def block()
        return @new_imgblock
    end
    def done()
        return (@crow>=@bsz-1)
    end
end

def SBA_SCLib.cs_CONWAY_LIFE(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
     
    operation=parent.opcode #t uint    
    #C++ Word_List result_list;
    res_symbol=EXTSYM  #C++ const Word res_symbol = EXTSYM;
    res_data=0 #t Word

    bloksize=128
    # Instead of parent.init we could also have an init method for the service
    # in the Gannet-C code this might be the service constructor, so INITIALIZE or NEW is good too
    # so A_INIT  would be called once, resulting in creation of the object
    # In fact, a Gannet-C service is an interface, so we could use the SC_ name as the initialize method
    # So what we would have is 
    # when SC_LOOPSERVICE 
    #     service=LoopService.new(parent,lstart,lstop,res_data)
    if parent.init()
        # create service object           
        service=ConwayLife.new(parent,bloksize) #C++ LoopService* service = new LoopService(parent,lstart,lstop,res_data); 
        parent.store(service)            
    else
        # load service object
        service=parent.load()
    end
    
    case operation 
    when A_ITERATOR_LOOP
        puts "LOOP COUNTER: #{service.lcur}"
        service.loop()
    when A_ITERATOR_VALUE
        res_data=service.current()
    else #C++ default:
        raise "Unknown service: #{operation}" 
        #C++   exit(0);
    end #;
    
    if !service.done()     
        # store service object. I wonder if I need to do this only once?
        # Also, maybe we can merge suspend and store
        
#            parent.suspend()
        return []
    else
        puts "RESULT: #{res_data}"
        result_list=[res_symbol,res_data]  #C++ result_list.push_back(res_data);
        #C++ delete(service);
        return result_list
    end
end # of cs_CONWAY_LIFE
#endskip

# --------------------------------------------------------------------------    
    
    #--
    # We need at least following "abstract" services:
    # --------------------------------------------------------------------------
    #++
    
    # The BEGIN core takes in all arguments returns the last result 
# I want to support sequencing in BEGIN. Maybe I could create a SEQ service for this very purpose
# A non-scoping seq block would be SEQ, an non-scoping par block would be BEGIN
    def SBA_SCLib.ls_BEGIN(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/ 
        #core
        result=[] #C++ Word_List result;
        for address in addresses #t MemAddresses
            result=sba_tile.data_store.mget(address)
        end
        #iv
        puts "#{parent.service} CORE: Passing on #{result}" #C++ cout << parent.service<< " CORE: Passing on result\n";
        #ev
        return result
    end # of BEGIN
    
# OLD: The BEGIN core takes in all arguments, checks for Error and returns the last result or Error
# WV:Do we need an Error Kind?
def SBA_SCLib.ls_BEGIN_OLD(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/ 
    #core
    result='Error' #C++ Word_List result;
    for address in addresses #t MemAddresses
            result=sba_tile.data_store.mget(address)
        #skip
        if result=='Error'
            parent.core_return_type=P_error
            return result
        end
        #endskip
    end
    #iv
    puts "#{parent.service} CORE: Passing on #{result}" #C++ cout << parent.service<< " CORE: Passing on result\n";
    #ev
    return result
end # of BEGIN_OLD
    
    # --------------------------------------------------------------------------
    # the actual data store service core
    # WV: For now, we use a global hash to store the DATA packets, and put them in the DATA store every time the service receives a (DATA ...) subtask. 
    # What we really should do is wait for the DATA packets to arrive and store them; if a (DATA ...) subtask would arrive before the packet is there, this subtask would have a 0 type ('empty') until the packet arrives. On the other hand, any DATA packet arriving at the gateway will automtically be sent to the DATA store. So do we really need (DATA ...)?
    # Well, maybe like this: the Originator of the task should not just send DATA. the DATA service will allocate space and request data packets for every subtask it receives. Unrequested data packets will be dropped. Security!!
    
    def SBA_SCLib.ls_DATA(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
        #core
        status=1 #t Word # for OK
        #C++ Word_List status_list;
        #C++ status_list.push_back(status);
    raise "ls_DATA broken!"     
    end # of DATA
    
    # --------------------------------------------------------------------------
    def SBA_SCLib.ls_IF(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
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
        return [] #C++ Word_List empty_list; return empty_list;
    else
        service=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
        operation=getName(service) #t uint    
        valaddress=0 #t MemAddress 
        if operation==A_RETURN or operation==A_RETURNTC
            #iv
            print "IF CORE: #{parent.current_subtask}: (RETURN \n"
            #ev
            valaddress=addresses[0]

        else # must be IF
            #iv
            print "IF CORE: #{parent.current_subtask}: (IF \n"
            #ev
            condval='Error' #t Word #s/=.Error.//
            condaddr=addresses[0] #t MemAddress
                condval=sba_tile.data_store.mget(condaddr)[0]&1 # quoted numbers!                
            if condval>0
                condval=1
            else
                condval=0
            end
            #iv
            puts "IF CORE: CONDVAL:#{condval}"
            puts "IF CORE: LABEL1:<#{addresses[1]}>"
            puts "IF CORE: LABEL2:<#{addresses[2]}>"
            #ev
            label='Error' #skip
            if condval==1
                valaddress=addresses[1]
            elsif condval==0
                valaddress=addresses[2]
            end
           
        end # RETURN or IF
        result_list=sba_tile.data_store.mget(valaddress) #t Word_List
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
                    if getKind(result) == K_S or getKind(result) == K_U
                        # If it's an expression => Error
                        raise "IF CORE: ERROR: IF arg can't be #{getKind(result)}"
                    elsif getKind(result) == K_L or getKind(result) == K_D
                        # If it's a variable or data => request & redirect result
                        # i.e. create a request packet
                        parent.core_return_type= P_request                               
                        sba_tile.service_manager.subtask_list.to(parent.current_subtask,getKind(result))                
                    elsif getKind(result) == K_B or getKind(result) == K_Q                         
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
                        if operation==A_RETURN or operation==SC_IF
                            puts "IF CORE wait for ACK"  if @v #skip
                            send_ack_to=setName(label,S_IF) 
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
                        if operation==A_RETURN or operation==SC_IF 
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
#                end # result.Quoted
            # C++ //result_list.push_back(result);
        else
            # It's unquoted, so it's a value. Just return it.
            puts "Unquoted word, must be value"  if @v #skip
            parent.core_return_type=P_data
#                result_list=sba_tile.data_store.mget(valaddress) #t Word_List
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
        return result_list
        end
    end # of ls_IF


    # -----------------------------------------------------------------------------    

=begin
The IF in the ServiceManager (S_IF) can only take quoted values. It can take K_R (not K_C), K_D and K_B
As far as I can see, this IF does not need ACK as it delivers locally; anyway ACK would be too complicated
So the assumption is that this IF always delivers locally, i.e. (S1 ... (S1-IF ...))
=end

    def SBA_SCLib.ls_S_IF(sba_tile,parent,addresses)#t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        operation=parent.opcode #t Word
        valaddress=0 #t MemAddress 
        if operation==A_S_RETURN 
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
        
        return result_list
        
    end # of ls_S_IF        
    # -----------------------------------------------------------------------------    

    def SBA_SCLib.ls_RAND(sba_tile,parent,addresses)#t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        
#        operation=parent.opcode #t Word
#C++    Word_List result_list;        
        address0=addresses[0] #t MemAddress
        #FIXME: if min_Val or max_val fits in 16 bits, should be unextended!
        min_val=0 #t Word
        
        if (getExt(sba_tile.data_store.mget(address0)[0])==1)            
            min_val=sba_tile.data_store.mget(address0)[1]
        else
            min_val=getValue(sba_tile.data_store.mget(address0)[0])
        end
        address1=addresses[1] #t MemAddress
        max_val=0 #t Word
        if (getExt(sba_tile.data_store.mget(address1)[0])==1)            
            max_val=sba_tile.data_store.mget(address1)[1]
        else
            max_val=getValue(sba_tile.data_store.mget(address1)[0])
        end
        res_symbol=EXTSYM #C++ const Word res_symbol = EXTSYM;
        #C++ result_list.push_back(res_symbol);
        #C++ srandom(parent.state_register[0]);      
        rnd_val = rand(max_val) #C++ Word rnd_val = random();
        res_val=min_val+rnd_val #C++ Word res_val=min_val + max_val + (rnd_val % max_val);  
        result_list=[res_symbol,res_val]  #C++ result_list.push_back(res_val);
        parent.state_register[0]=res_val;
#iv        
        puts "RAND CORE RESULT: #{res_val}"
#ev         
        return result_list
        
    end # of ls_RAND     
    # -----------------------------------------------------------------------------      

    # -----------------------------------------------------------------------------    

    def SBA_SCLib.ls_RND_MATRIX(sba_tile,parent,addresses)#t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
#        operation=parent.opcode #t Word
        result_list=[] #C++ Word_List result_list;        
        min_val=0 #t Word
        max_val=255 #t Word
        for i in 0..63 #t int
            res_val=min_val+rand(max_val) #C++ Word res_val=min_val+(random() % max_val);
            result_list.push(res_val)  #C++ result_list.push_back(res_val);           
        end
      #iv            
                  puts "RAND MATRIX CORE RESULT"
                  puts "#{ppPayload(result_list)}" #skip
      #ev          
        return result_list
        
    end # of ls_RND_MATRIX     
    # -----------------------------------------------------------------------------   

    # -----------------------------------------------------------------------------    

    def SBA_SCLib.ls_PROC_MATRIX(sba_tile,parent,addresses)#t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        #iv
      puts "CORE PROC_MATRIX"
      #ev 
        result_list=[] #C++ Word_List result_list;
#        operation=parent.opcode #t Uint
        first=true #t bool
        for address in addresses #t MemAddresses
            matrix=sba_tile.data_store.mget(address) #t Word_List
          puts "ADDRESS: #{address}", matrix.inspect #skip          
            if first            
                for elt in matrix #t Word_List      
                    result_list.push(elt)  #C++ result_list.push_back(elt);
                                        
                    end
                first=false
            end
        end    
        
        if @v #skip
#        puts "CORE PROC_MATRIX RESULT #{result_list.inspect}" #skip
        #iv
            puts "CORE PROC_MATRIX RETURN RESULT size #{result_list.size}" #skip
        #ev        
        end #skip
        return result_list
    end # of ls_PROC_MATRIX     
    # -----------------------------------------------------------------------------          
    
# What we want is a service that will serve NxN blocks for every call.
# So we must have the image stored in a memory location that doesn't get cleared;
# The state registers can contain the X and Y values of start of the block to be served
# the block size can actually be an argument to the call; as could be the image name?
# Or will we map image names to numbers in a Data section in the YAML file? That is easy.
#
# So, on a call to the IMGBLOCK service, we first check the state register[0]: if it is 0, means no
# image; 1 means an image has been loaded; 2 means done.

  def SBA_SCLib.cs_IMG_IN(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
  #core
      result_list=[] #C++ Word_List result_list;
      opcode = parent.opcode #t Service
      #C++ Word_List raw_img_data;
if opcode==SC_IMG or opcode==A_IMG_IN or opcode==M_IMG_IN
       # (img.in blockdim)
  # the range DATA_OF .. DATA_OF+NREGS is for registers, no clean-up  
      img_address=DATA_OF #t MemAddress
      
      nrows=0 #t Word 
      ncols=0 #t Word 
      blockdim=0 #t Word
    if parent.state_register[0]==2
        parent.core_status=CS_done_eos
#ifdef SYSC            
#C++          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": CORE IMG IN set core status to EOS"<<endl;            
#endif // SYSC
        parent.state_register[0]=0
        
    else
      if parent.state_register[0]==0
          # get the blocksize from the argument
          address = addresses[0] #t MemAddress
          blockdim = getValue(sba_tile.data_store.mget(address)[0]) 
          # store the blockdim
          parent.state_register[5]=blockdim    
          # open the file
          
#skip
          raw_img_file=sba_tile.sba_system.task_data
          raw_img_data=[]
          data_file=File.open(raw_img_file,"r")
          
          nrows=data_file.gets.hex
          ncols=data_file.gets.hex
          data_file.each_line { |pixval|
              raw_img_data.push(pixval.hex);
          }          
#endskip 
                 
=begin #C++    
#ifndef SYSC          
    string raw_img_file=sba_system.task_data;
#else           
    string raw_img_file=SC_SBA::data_file;
#endif                    
    std::ifstream data_file(raw_img_file.c_str()); 

    if (data_file.bad()) {
        std::cerr << "Error: Could not open "<< raw_img_file <<"\n";
        exit (8);
    }
    data_file >> hex;
    data_file >> nrows;
    data_file >> ncols;
=end #C++    
        parent.state_register[1]=nrows;
        parent.state_register[2]=ncols;
=begin #C++    
    Word pixval;
        Word_List raw_img_data;
    while(  data_file >> pixval ) {
        raw_img_data.push_back(pixval);
    }
    data_file.close(); # I guess
=end #C++    
      # store the image outside the function. I guess the easiest way is to load the image into a Word_List,
      # basically move the image from its current address to an address that will never be cleared,
      # i.e. one of the registers
          sba_tile.data_store.mput(img_address,raw_img_data)
          
          parent.state_register[0]=1;
          parent.state_register[3]=0 
          parent.state_register[4]=0         
      end    
      
      if (parent.state_register[0]==1) 
          nrows=parent.state_register[1]
          ncols=parent.state_register[2]
          blockdim=parent.state_register[5]
          
          raw_img_data=sba_tile.data_store.mget(img_address)
          # load the complete image from memory into a matrix
          # based on row and col we could actually do this selectively; but why bother?
#skip          
          img=[] 
          cc=ncols 
          row=[] 
          for pixval in raw_img_data       
              cc-=1
              row.push(pixval) 
              if cc==0
                  img.push(row)
                  row=[] 
                  cc=ncols
              end
          end
#endskip
=begin #C++          
          vector< vector< Word > > img(nrows,vector< Word >(ncols,0));
          Word_List::iterator iter=raw_img_data.begin();
          
          for (uint row=0;row<nrows;row++) {
            for (uint col=0;col<ncols;col++) {
            if(iter!=raw_img_data.end()) {          
                Word pixval=*iter;
                img[row][col]=pixval;
                iter++;
              } else {
                img[row][col]=0;
          }
          }
          } 
=end #C++                              
          
          
          # Then extract the NxN matrix and put it into a Word_List, and return
          crow=parent.state_register[3] #t Word 
          ccol=parent.state_register[4] #t Word 
          # for debugging
#ifdef SYSC          
#C++ OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": CORE IMG IN" << " row:"<<crow<<";col:"<<ccol<<endl;            
#endif //SYSC          
          coords=(crow << 16 ) + ccol #t Word
          result_list.push(coords)
          for rr in crow..crow+blockdim-1 #t uint
              for cc in ccol..ccol+blockdim-1 #t uint
              # get the col elts, put into results_list
                  result_list.push(img[rr][cc])
              end
          end
          ccol+=blockdim
          if ccol>ncols-blockdim # just because I'm too lazy to do the padding
              ccol=0
              crow+=blockdim
              if crow>nrows-blockdim
                  crow=0
                  parent.state_register[0]=2                  
                  puts "PROCESSED IMAGE" if @v #skip
              end
          end
          parent.state_register[3]=crow
          parent.state_register[4]=ccol
      end
    end
elsif opcode==A_IMG_SIZE or opcode==M_IMG_SIZE
  # (img.size)
  # there is no reason to return this as a Symbol;
  # OTOH nrows and ncols will never be more than 16 bits anyway
  nrows=parent.state_register[1] #t uint
  ncols=parent.state_register[2] #t uint
  blockdim=parent.state_register[5] #t uint
# TODO: ncolssym=mkSymbol(K_B,T_i,)
# TODO: nrowssym
  result_list.push(blockdim)
  result_list.push(nrows)
  result_list.push(ncols)
  puts "IMG-SIZE CORE: RETURN RESULT #{result_list.inspect}" if @v #skip
else
  raise "cs_IMG_IN CORE: unsupported opcode #{opcode} (SC_IMG: #{SC_IMG}; A_IMG_SIZE: #{A_IMG_SIZE} "
end
#iv
      puts "IMG-IN CORE: RETURN RESULT size #{result_list.size}"
#ev       
      return result_list
  end # of cs_IMG_IN    
# ---------------------------------------------------------------------------------------------------------
# A service to reconstruct the image from the blocks it receives. It takes as argument the BLOCKSZ. 
# How does this service now the number of rows and columns in the original image? 
# The IMG_IN service must provide this information on request.
# So it is best to first collect all the blocks and then get the nrows and ncols and reconstruct the image 
# (img.out (img.size) )  
    def SBA_SCLib.cs_IMG_OUT(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core
        result_list=[] #C++ Word_List result_list;
        opcode = parent.opcode #t Service
        # the range DATA_OF .. DATA_OF+NREGS is for registers, no clean-up
        img_address=DATA_OF #t MemAddress
        if opcode==SC_PROCIMG or opcode==M_PROCIMG_IN
            # get the block
#iv
            if @v #skip           
            puts "#{parent.service} CORE PROCIMG: ADD BLOCK"
            end #skip
#ev            
            address = addresses[0] #t MemAddress
            block = sba_tile.data_store.mget(address) #t Word_List
            # The lines below are ony for SYSC debugging
            if block.size()>1
            coords=block.shift() #C++ Word coords=block.front();block.pop_front();
            crow=(coords>>16)&0xFF #t Word
            ccol=coords&0xFF #t Word
#ifdef SYSC            
#C++          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": CORE IMG OUT" << " row:"<<crow<<";col:"<<ccol<<endl;            
#endif // SYSC
            else
#ifdef SYSC            
#C++          OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": CORE IMG OUT empty packet:"<<block.size()<<endl;            
#endif // SYSC
            end
            sba_tile.data_store.mpush(img_address, block)
#            img=sba_tile.data_store.mget(img_address) #t Word_List
#            for pixel in block #t Word_List
#                img.push(pixel)                
#            end
#            puts "IMG ADDRESS: #{img_address} #{img.size()}"
#            sba_tile.data_store.mput(img_address, img) # malloc error here in OS X 10.6
            # purely for debugging, thereis no good reason for this!
            result_list=[7188] #skip        
        elsif opcode==A_PROCIMG_DUMP or opcode==M_PROCIMG_DUMP 
#iv
            if @v #skip           
            puts "#{parent.service} CORE PROCIMG: DUMP"
            end #skip
#ev            
            # (img.size) returns (blockdim, nrows,ncols) 
            img_sz_address = addresses[0] #t MemAddress
            # note these are raw numbers, not Symbols!
            blockdim = sba_tile.data_store.mget(img_sz_address)[0] #t Word
            nrows = sba_tile.data_store.mget(img_sz_address)[1] #t Word 
            ncols = sba_tile.data_store.mget(img_sz_address)[2] #t Word                        
#            nrows = nrows & 0xFFFF
#            ncols = ncols & 0xFFFF
            nblocks_row=(nrows - (nrows % blockdim))/blockdim #t Word
            nblocks_col=(ncols - (ncols % blockdim))/blockdim #t Word
            # for now. Of course we really must reconstruct the image
            result_list.push(nblocks_row)
            result_list.push(nblocks_col)
            # Reconstructing means transform the matrix into a raw image file
            # the raw image file is a trivial row-by-row format
            # the "img" list contains all the blocks in row-by-row order
            img=sba_tile.data_store.mget(img_address) #t Word_List
            npixblock=blockdim*blockdim #t uint
            npiximg=npixblock*img.length #t uint
            imgsz=ncols*nrows #t uint
#iv
if @v #skip
            puts ncols, nrows, blockdim #C++ cout <<ncols<<"\n"<< nrows<<"\n"<< blockdim<<"\n";
            puts "NBLOCKS: #{npiximg/npixblock}" 
end #skip
#ev

#skip            
            rgbimage=[]
            # Fill the image with black     
            for row in 0 .. nrows-1
                rgbimage[row]=[] 
                    for col in 0..ncols-1
                        rgbimage[row][col]="#{col} #{nrows-1-row} 0 0 0"
                    end
            end
#endskip
                        
=begin #C++                       
           vector< vector<string> > rgbimage;
           for (unsigned int row=0;row<nrows;row++) {
                vector<string> tmpv;
                for (unsigned int col=0;col<ncols;col++) {
                    std::ostringstream outs; 
                    outs<<" "<<col<<" "<<(nrows-1-row)<<" 0 0 0\n";
                    tmpv.push_back(outs.str());
                }
                rgbimage.push_back(tmpv);
            }
=end #C++                        
#ifndef SYSC            
            raw_img_file=sba_tile.sba_system.task_data #C++ string raw_img_file = sba_system.task_data;raw_img_file+=".rgb";
#else             
            #C++ string raw_img_file=SC_SBA::data_file;raw_img_file+=".rgb";
#endif            
            rgbimage_file=File.open(raw_img_file+".rgb","w") #C++ std::ofstream rgbimage_file (raw_img_file.c_str());
            npix =(npiximg<imgsz)?npiximg:imgsz #t uint
            for i in 0..npix-1 #t uint               
               blockn=(i- (i%npixblock))/npixblock #t uint
               # row and col inside a block
               bcol=(i%npixblock)%blockdim #t uint
               brow=((i%npixblock)-bcol)/blockdim #t uint
               
               # x and y coord of the nth block
               xb=blockn % (ncols/blockdim) #t uint
               yb=(blockn-xb)/(ncols/blockdim) #t uint
#               puts "#{bcol} #{brow} #{xb} #{yb}"
               x=xb*blockdim+bcol #t uint
               y=yb*blockdim+brow #t uint
               
               w=img[i] #t uint
                b=w&255 #t uint
                g=(w>>8)&255 #t uint
                r=(w>>16)&255 #t uint
                puts "#{x} #{y} #{r} #{g} #{b}" if @v  #skip
                rgbimage[y][x] = "#{x} #{nrows-1-y} #{r} #{g} #{b}" #skip
                #iv
                #C++ cout<< " "<<x<<" "<<(nrows-1-y)<<" "<<r<<" "<<g<<" "<<b<<"\n";
                #ev
                #C++ std::ostringstream outs; outs<< " "<<x<<" "<<(nrows-1-y)<<" "<<r<<" "<<g<<" "<<b<<"\n";
                #C++ rgbimage[y][x]=outs.str();                            
            end    
#skip            
            for row in rgbimage
                for pix in row
                    rgbimage_file.puts(pix)              
                end
            end                    
#endskip
=begin #C++
            
            for (vector< vector<string> >::iterator iter=rgbimage.begin();iter!=rgbimage.end();iter++) {
                vector<string> tmpv=*iter;
                for ( vector<string>::iterator iter2=tmpv.begin();iter2!=tmpv.end();iter2++) {
                    string pix=*iter2;
                    rgbimage_file << pix;
                }
            }
            
=end #C++
            rgbimage_file.close()
        elsif opcode==M_PROCIMG_DRAW
            # code for drawing with SDL goes here
        end
        return result_list
    end # of cs_IMG_OUT    
# ---------------------------------------------------------------------------------------------------------

    # With LET aliased to ASSIGN, in principle we could register all calls to ASSIGN'ed variables.
    # Then LET could multicast to those services only the list of variables to clean up.
    # Direct addressing is very risky, if not impossible, for this: the variable will be stored at different addresses
    # for every service involved. But ASSIGN _knows_ those addresses!     
 
    def SBA_SCLib.ls_LET(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
#        raise "BROKEN! Adapt to multi-threaded core!"
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
        
        puts parent.current_subtask if @v #skip
    	  service=getName(service_word) #t Name_t
        parent.core_return_type=P_data
#iv
        ppservice=service #C++ int ppservice=(int)service;
        print "LET (#{parent.service}) CORE: #{parent.current_subtask}: (#{ppservice}<>#{SC_LET}\n" 
#ev        
  
        if service==SC_LET or service==A_LETTC
#iv
        print "LET (#{parent.service}) CORE: ",parent.current_subtask,"\n"

            print "LET (#{parent.service}) CORE: ", "TO: ", sba_tile.service_manager.subtask_list.to(parent.current_subtask),"\n"            
            print "LET (#{parent.service}) CORE: ", "RETURN TO: ", sba_tile.service_manager.subtask_list.return_to(parent.current_subtask),"\n"
            print "LET (#{parent.service}) CORE: ", "RETURN AS: ", sba_tile.service_manager.subtask_list.return_as(parent.current_subtask),"\n"
            print "LET (#{parent.service}) CORE: ", "CALLED AS: ", sba_tile.service_manager.subtask_list.called_as(parent.current_subtask),"\n"
#ev            
            last=false #t bool
            nargs=parent.n_args #t uint        
            argct=nargs #t uint
            for address in addresses #t MemAddresses       
                argct-=1
                last=(argct==0)
                label=sba_tile.service_manager.symbol_table[address] #t Word     
            #iv
                print "LET (#{parent.service}) CORE: ", "LABEL:", label," last?",last,"\n"             
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
                        raise "Only R's allowed inside LET, no bare L's. Use RETURN." if @v #skip
                    end
                    # - reset the task status to STS_blocked
                    #iv
                        puts "LET (#{parent.service}) CORE: BLOCKED #{parent.current_subtask}"             
                    #ev                    
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_blocked)
                    # -create a ref packet and send it off
                    to=getName(numval) #t To_t
                    return_to=S_LET #t Return_to_t
                    var_label = setSubtask(label,address) #t Word
                    var_label = setName(var_label,S_LET)
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
                        send_ack_to=setName(label,S_LET) #t Word
                        send_ack_to=setSubtask(send_ack_to,address)
                        parent.ack_ok=0 # If a core redirects, it doesn't send an ACK
                        # set "waiting for ACK" flag
                        if service!=A_LETTC                        
                            sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,1)
                        end
                        return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask)                       
                        return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask)
                        ref_packet_header= mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as)
                        ref_packet_payload=reslist #t Word_List 
                        ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                        puts ppPacket(ref_packet) if @v #skip
                        if to!=S_LET
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
                        puts "NOT LAST arg, sequencing" if @v #skip
                        # -set the core status to CS_managed
                        parent.core_status=CS_managed                    
                        # if redirection is not supported, the value of the last arg should return in the end
                        ref_packet_header= mkHeader(packet_type,prio,redir,payload_length,to,return_to,ack_to,return_as)
                        ref_packet_payload=reslist #t Word_List 
                        ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                        puts ppPacket(ref_packet) if @v #skip
                        if to!=S_LET
                            sba_tile.transceiver.tx_fifo.push(ref_packet)
                        else
                            if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                            else # SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet)  
                            end # SEQVM                            
                        end
                    end # of if last
                    if  service!=A_LETTC                    
                        return reslist # will be ignored anyway
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
                puts "LET CORE: ACK REDIR: #{sba_tile.service_manager.subtask_list.redir(parent.current_subtask)}"
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
                puts "LET-ASSIGN\tFound #{labeladdr}"
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
            return result
        ### end of LET                                
        else # NOT SC_LET
            result=0 #t Word
            #C++ #ifndef STATIC_ALLOC
            value_list=[] #t List<Word_List>            
            #C++ #else
            #C++ List<Word_List,MAX_NARGS> value_list;
            #C++ #endif
            for address in addresses #t MemAddresses
#iv            
                    try_array=sba_tile.data_store.mget(address) #t Word_List
                    print "LET (#{parent.service}) CORE: #{address}\t#{ppPayload(try_array)}\n"
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
            case service
            when A_ASSIGN 
                # Take the addresses of Symbol and Data
                puts "LET called as ASSIGN" if @v #skip
                sym_address=addresses[0] #t MemAddress
                data_address=addresses[1] #t MemAddress
                #iv
                puts "ASSIGN addresses: #{sym_address} => #{data_address}"
                #ev
                # Get the varname from the symbol
                var_label=sba_tile.service_manager.symbol_table[sym_address] #t Word
                puts "ASSIGN SYMBOL: #{ppSymbol(var_label)}" if @v #skip
                var_name=getName(var_label) #t Name_t
                # Check if this varname is in the lookup table
                has_label=false #t bool
                var_address=0 #t MemAddress
#                lookup=sba_tile.data_store.mget(sba_tile.service_manager.service_core_ram+1) #t Word_List
#                for word in lookup #t Word_List
#                    if getName(word) == var_name
#                        has_label=true    
#                        var_address= getSubtask(word)
#                        break
#                    end
#                end               
                
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
                    puts "ASSIGN: STS=#{sba_tile.service_manager.subtask_list.status(parent.current_subtask)}"
#ev                    
#                    lookup.push(word)
#                    sba_tile.data_store.mput(sba_tile.service_manager.service_core_ram+1,lookup)
#                    puts "#{getName(word)}: #{word}"
                    sba_tile.lookup_table.write(getName(word),word)
                # Store variable definition
                    var_value=sba_tile.data_store.mget(data_address) #t Word_List
#iv
                    puts "ASSIGN: storing",ppPayload(var_value), "@ #{var_address}"
#ev                                        
                    sba_tile.data_store.mput(var_address,var_value)
                    if var_value[0]==NIL
                        result=ZERO
                    else
                        result=ONE
                    end    
#iv                                
                else
                    # Some trouble: we're overwriting a presumably immutable variable!!                    
                    puts " [WARNING: overwriting <#{var_label}> (",ppPayload(sba_tile.data_store.mget(var_address)),")] "                                
#ev                        
                end
                return [result] #C++ Word_List result_list; result_list.push_back(result); return result_list;
                ###### UPDATE #######
            when A_UPDATE 
               # Receives a quoted L, looks up the value and updates it.
                sym_address=addresses[0] #t MemAddress
                # Get the varname from the symbol
                var_label=sba_tile.service_manager.symbol_table[sym_address] #t Word
                puts "LET called as UPDATE #{ppSymbol(var_name)}" if @v #skip
                var_name=getName(var_label)
                # Check if this varname is in the lookup table
                
                has_label=false #t bool
                var_address=0 #t MemAddress
#                lookup=sba_tile.data_store.mget(sba_tile.service_manager.service_core_ram+1) #t Word_List
#                for word in lookup #t Word_List
#                    if getName(word) == var_name
#                        has_label=true
#                        var_address= getSubtask(word)
#                        break
#                    end
#                end              
                
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
                    if var_value[0]==NIL
                        result=ZERO
                    else
                        result=ONE
                    end                        
                    result_list=[result] #C++ result_list.push_back(result); 
                else
                    # ASSIGN has not yet returned. 
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_pending)
                    parent.core_status=CS_managed                    
                end
                return result_list            
                ###### READ #######
                # It turns out that if we use UPDATE, we need READ. That's because L-variables could be cached          
            when A_READ 
                # What does a call do? Very simple: it receives a quoted L, looks up the value and returns it.
                sym_address=addresses[0] #t MemAddress
                # Get the varname from the symbol
                var_label=sba_tile.service_manager.symbol_table[sym_address] #t Word
                puts "LET called as READ #{ppSymbol(var_label)}" if @v #skip
                var_name=getName(var_label)
                # Check if this varname is in the lookup table
                has_label=false #t bool
                var_address=0 #t MemAddress
#WV14112008                
#                lookup=sba_tile.data_store.mget(sba_tile.service_manager.service_core_ram+1) #t Word_List
#                for word in lookup #t Word_List
#                    if getName(word) == var_name
#                        has_label=true   
#                        var_address= getSubtask(word)
#                        break
#                    end
#                end              
                
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
                    puts "READ: returning",ppPayload(result_list)
#ev                    
                else
                    # TROUBLE: ASSIGN has not yet returned. In the brave new world, where we do 
                    # (READ 'v (UPDATE 'v Ru (ASSIGN 'v Ra)))
                    # this is impossible of course
                    puts "READ: not yet there","Setting subtask <#{parent.current_subtask}> to #{STS_pending}" if @v #skip
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_pending)
                    parent.core_status=CS_managed
                end
                
                return result_list

#            when A_APPLY 
#                ###### APPLY ######                
#                # A shiny new APPLY which does binding if the arguments are not quoted and substitution if they are quoted
#                # I'm still undecided about this. On the one hand, it is consistent with Scheme and other languages
#                # LET is sugar for LAMBDA. But on the other hand, I do need the substitution strategy, so we would need 2 kinds of apply

#C++        break;}
#C++        default: std::cerr<< "Service "<<service<< " has no implementation\n";exit(0);
#skip                
            else ########## LIST SERVICES ###############
=begin
    The new LIST service also provides HEAD (car), TAIL (cdr), LENGTH, CONS and APPEND

        * How do we de-allocate this? We can really only do so if LIST is an alias of LET/ASSIGN
        * Then any list allocated inside a LET is deallocated on leaving the LET
        * So even if we don't ASSIGN it, it should be de-allocated. 
        * So the address should be added to the subtask argument list
    LIST, CONS, TAIL and APPEND must return the reference to the subtask (Quoted? Unquoted? I guess Quoted=1)
    The core must contain a lookup table between this ref and the address/offset/status
    * Clean-up is achieved by adding the list_code_ref symbols for all list-creating tasks to the LET argument list at compile time
    Then it is simply a matter of looping through all these arguments, looking up the address and deallocate it
I think we should read symbols from the LET code from the back until we reach the ref of the tail call:
    based on code_address  we get the code (a Word_List)
    we iterate through it back to front:
    wl=sba_tile.data_store.mget(code_address)
    i=wl.length-1
    while wl[i]!=tailref
       # deallocate
        list_ref=parent.lookup_table[wl[i]]
        list_addr=getSubtask(list_ref) #t MemAddress
        sba_tile.service_manager.data_address_stack.push(list_addr)
        i-=1
    end
=end        

#WV17082008 Issue with List store
# - if Lists are storea as Word_List, they're very short
# - if we create a longer List_Store object, it doesn't "fit" in Store
# - so either we need a separate store for Lists or we can only have short Lists 
# - or we reinvent linked lists, with the first Word of a Word_List containing a pointer to the next field
            service_id=sba_tile.service_manager.service #t Service
    # TODO: to get the longest possible list, we need a different structure for the listref:
    # K_0:offset:address so we can't really use setSubtask etc
            listop_code_address=sba_tile.service_manager.subtask_list.code_address(current_subtask) #t CodeAddress         
             puts "\nCODE ADDRESS: #{listop_code_address}" if @v #skip
            listop_code_ref_taskfield=(listop_code_address >> FS_CodeAddress) & FW_CodePage #t Task_t
            listop_code_ref_subtaskfield=listop_code_address & FW_CodeAddress #t Subtask_t
            listop_code_ref=mkSymbol(K_R,T_d,0,0,listop_code_ref_taskfield,listop_code_ref_subtaskfield,service_id) #t Word
            puts "CODE REF: #{listop_code_ref} (#{listop_code_ref_taskfield}:#{listop_code_ref_subtaskfield})" if @v #skip
            if service==A_LIST 
    # The LIST core:
                # pops and address off the stack
                list_address=sba_tile.service_manager.data_address_stack.pop() #t MemAddress
                # create an empty list
                list=[] #C++ List_Store list;
                # add the arguments if any
                list_length=0 #t Word
                for address in addresses #t MemAddresses
                    value_l=sba_tile.data_store.mget(address)
                    list_length+=1
                    list+=value_l #C++ foreach(List_Store,value_l) {Word w=*iter;list.push_back(w);}
                end
    #FIXME            list.push(list_length)
                # write the list to the list_address. 
                sba_tile.data_store.mput(list_address,list) 
                # create the list reference: Kind(3):offset(13):address(16)
                list_ref=0 #t Word
                list_ref=setSubtask(list_ref,list_address)
                list_ref=setKind(list_ref,0)
                puts "LIST detected!" if @v #skip
                parent.lookup_table.write(listop_code_ref,list_ref)
                return [listop_code_ref] #C++  Word_List result_list; result_list.push_back(list_code_ref); return result_list;
            else 
                list_code_ref_addr=addresses[0] #t MemAddress
                list_code_ref_l=sba_tile.data_store.mget(list_code_ref_addr) #t Word_List
                list_code_ref=list_code_ref_l[0] #t Word
                list_ref=parent.lookup_table.read(list_code_ref)
                list_addr=getSubtask(list_ref) #t MemAddress
                puts "LIST ADDR: #{list_code_ref} => #{list_addr}" if @v #skip
                list_offset=list_ref & F_Name #t int
                case service 
                    when A_LENGTH
                        # The LENGTH core FIXME: This returns the length in WORDS not SYMBOLS!
                        list=sba_tile.data_store.mget(list_addr) #t Word_List
    #FIXME                list_length=list[list.length-1] #C++ Word list_length=list.back();
                        list_length=list.length #t Word
                        # but then we need to reserve space for a length field. 
                        # must return a K_B Symbol
                        symbol_header=mkSymbol(K_B,T_i,1,1,0,1,0) # Datatype,Ext,Quoted,Task,Subtask,Name
                        return [symbol_header,list_length] #C++  Word_List result_list; result_list.push_back(symbol_header); result_list.push_back(list_length); return result_list;
                    when A_EMPTY                
                        list=sba_tile.data_store.mget(list_addr) #t Word_List
    #FIXME                list_length=list[list.length-1] #C++ Word list_length=list.back();
                        empty=(list.length==0)?1:0 #t Word
                        symbol_header=mkSymbol(K_B,T_i,1,1,0,1,0) # Datatype,Ext,Quoted,Task,Subtask,Name
                        return [symbol_header,empty] #C++  Word_List result_list; result_list.push_back(symbol_header); result_list.push_back(empty); return result_list;                    
                    when A_HEAD 
    # The HEAD (car) core
                        list=sba_tile.data_store.mget(list_addr)
                        puts "HEAD: #{list.inspect}" if @v #skip 
                        puts "HEAD OFFSET: #{list_offset}" if @v #skip
                        elt_value= list[list_offset] #t Word
                        elt_value_as_list=[elt_value] #C++ Word_List elt_value_list; elt_value_list.push_back(elt_value);
                        if getExt(elt_value)>0
                            for i in 1..getSubtask(elt_value)
                                list_offset+=1
                                elt_value_as_list.push(list[list_offset])
                            end
                        end
                        return elt_value_as_list
                    when A_TAIL 
        # The TAIL (cdr) core
        #                list_offset+=1 # well, no: at least 1, but it might be an extended symbol, so we must check!
                        # So check the symbol at original list_offset
                        list=sba_tile.data_store.mget(list_addr) #t List_Store
                        puts "TAIL: LIST ADDR: #{list_code_ref} => #{list_addr}" if @v #skip
                        puts "TAIL: list is #{list.inspect}" if @v #skip
                        list_elt=list[list_offset] #t Word
                        list_offset+=1 
                        if getExt(list_elt)>0
                            list_offset+=getSubtask(list_elt)
                        end
                        puts "TAIL OFFSET: #{list_offset}" if @v #skip
                        puts "TAIL returns #{list[list_offset..list.length-1].inspect}" if @v #skip
                        tail_list_ref=0 #t Word
                        tail_list_ref=setSubtask(tail_list_ref,list_addr)
                        tail_list_ref=setKind(tail_list_ref,0)
                        tail_list_ref=tail_list_ref+(list_offset & F_Name)
                        # Now this is a new code ref so bind to the list ref & return
                        # PROBLEM: what about length? It seems that we to know offset both in Word and in Symbols
                        
                        # Now bind this to the code address
                        parent.lookup_table.write(listop_code_ref,tail_list_ref)
                        return [listop_code_ref] #C++  Word_List result_list; result_list.push_back(list_code_ref); return result_list;
                    when A_CONS         
        # The CONS core    
        # for simplicity, my CONS should be (CONS list elt) not (CONS elt list)    
        # so we should really call it PUSH
                        elt_addr=addresses[1] #t MemAddress
                        elt=sba_tile.data_store.mget(elt_addr) #t Word_List
                        # get the old list
                        list=sba_tile.data_store.mget(list_addr)  #t List_Store 
                        # add the element
                        cons_list=list[list_offset,list.length-1]+elt #t List_Store    
                        puts "CONS: list is now #{cons_list.inspect}" if @v #skip       
                        # get a new list address
                        cons_list_addr=sba_tile.service_manager.data_address_stack.pop() #t MemAddress
                        puts "CONS LIST ADDR: #{listop_code_ref} => #{cons_list_addr}" if @v #skip
                        # store the lot
                        sba_tile.data_store.mput(cons_list_addr,cons_list)
                        # create the list reference: Kind(3):offset(13):address(16)
                        cons_list_ref=0 #t Word
                        cons_list_ref=setSubtask(cons_list_ref,cons_list_addr)
                        cons_list_ref=setKind(cons_list_ref,0)
                        parent.lookup_table.write(listop_code_ref,cons_list_ref)
                        return [listop_code_ref] #C++  Word_List result_list; result_list.push_back(list_code_ref); return result_list;
                    when A_APPEND        
                        # The APPEND core        
                        list2_code_ref_addr=addresses[1] #t MemAddress
                        list2_code_ref_l=sba_tile.data_store.mget(list2_code_ref_addr) #t Word_List
                        list2_code_ref=list2_code_ref_l[0] #t Word
                        list2_ref=parent.lookup_table[list2_code_ref]
                        list2_addr=getSubtask(list_ref) #t MemAddress
                        list2_offset=list_ref & F_Name #t int
                        # get the old list
                        list=sba_tile.data_store.mget(list_addr) #t Word_List
                        list2=sba_tile.data_store.mget(list2_addr) #t Word_List
                        # join the lists. FIXME: must take offset into account!
                        join_list=list[list_offset..list.length-1]+list2[list2_offset..list2.length-1] #t Word_List     
                        # get a new list address
                        join_list_addr=sba_tile.service_manager.data_address_stack.pop() #t MemAddress
                        # store the lot
                        sba_tile.data_store.mput(join_list_addr,join_list)
                        # build the new reference
                        join_list_ref=0 #t Word
                        join_list_ref=setSubtask(join_list_ref,join_list_addr)
                        join_list_ref=setKind(join_list_ref,0)
                        parent.lookup_table[listop_code_ref]=join_list_ref
                        return [listop_code_ref] #C++  Word_List result_list; result_list.push_back(list_code_ref); return result_list;
                    else
                        raise "LET (#{parent.service}) CORE: WARNING: service=#{service} #{sba_tile.service_manager.subtask_list.called_as(parent.current_subtask)}" if @v #skip
                        # who knows?
                    end  # of case for LIST services except LIST
                end # of if A_LIST or not                                
#endskip 
            end # of non-LET services case block
           
#            print  ") in #{var_address} => "		
#iv            
            print  ") => "		
            print ppSymbol(result),"\n"
#ev            
            parent.core_return_type=P_data	            
            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_cleanup)
            return [result] #C++  Word_List result_list; result_list.push_back(result); return result_list;
        end # of not SC_LET
    end # of ls_LET        
# -----------------------------------------------------------------------------    
    # SEQ is like a LET but it does not provide scope, all it does is sequence the arguments. It's like BEGIN with seq support.
def SBA_SCLib.ls_SEQ(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
    #core 
    service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
    
    puts parent.current_subtask if @v #skip
    service=getName(service_word) #t Name_t
    parent.core_return_type=P_data
#iv
    ppservice=service #C++ int ppservice=(int)service;
    print "SEQ (#{parent.service}) CORE: #{parent.current_subtask}: (#{ppservice}<>#{SC_SEQ}\n" 

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
                return_to=S_SEQ #t Return_to_t
                var_label = setSubtask(label,address) #t Word
                var_label = setName(var_label,S_SEQ)
                return_as=var_label #t Word
                ack_to=0 #t Word
                packet_type=P_reference #t Packet_Type
                prio=0 #t Ctrl_t
                redir=0 #t Redir_t
                reslist=[] #t Word_List
                reslist.push(numval)
                payload_length=1 #t Length_t
# #C++                bool use_redir=false;
                use_redir=true #t bool

                if (last and use_redir)      
                    puts "SEQ CORE: last arg quoted, REDIR/wait for ACK" if @v #skip
                    # if the last argument is quoted, redirect instead of trying to sequence
                    # so the return packet will be an ACK
                    # CORE decides to redirect
                    parent.core_status=CS_managed
                    send_ack_to=setName(label,S_LET) #t Word
                    send_ack_to=setSubtask(send_ack_to,address)
                    parent.ack_ok=0 # If a core redirects, it doesn't send an ACK
                    # set "waiting for ACK" flag
                    if service!=A_SEQTC                        
                        sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,1)
                    end
                    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask)                       
                    return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask)
                    ref_packet_header= mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as)
                    ref_packet_payload=reslist #t Word_List 
                    ref_packet=mkPacket(ref_packet_header,ref_packet_payload)
                    puts ppPacket(ref_packet) if @v #skip
                    if to!=S_SEQ
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
                    if to!=S_SEQ
                        sba_tile.transceiver.tx_fifo.push(ref_packet)
                    else
                        if SEQVM==0
                        sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
                        else # SEQVM==1
                        sba_tile.service_manager.activate_subtask(ref_packet)  
                        end # SEQVM                            
                    end
                end # of if last
                if  service!=A_SEQTC                    
                    return reslist # will be ignored anyway
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
            puts "SEQ CORE: got ACK" if @v #skip
#iv
            puts "SEQ CORE: ACK REDIR: #{sba_tile.service_manager.subtask_list.redir(parent.current_subtask)}"
#ev                
            # The problem is that the core must return something. But no other service is expecting a packet. So that's useless.
            # So I'll use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
            parent.ack_ok=1 # we're not redirecting so we can ack
            parent.core_status=CS_managed
            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processed)
            sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask,0)        
        end
        puts "SEQ (#{parent.service}) CORE: Passing on #{result}" if @v  #skip
        return result
#iv            
        print  ") => "      
        print ppSymbol(result),"\n"
#ev            
        parent.core_return_type=P_data              
        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_cleanup)
        return [result] #C++  Word_List result_list; result_list.push_back(result); return result_list;
end # of ls_SEQ       
    # -----------------------------------------------------------------------------
    def SBA_SCLib.ls_LAMBDA(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
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
        return result
    end # of LAMBDA
    # -----------------------------------------------------------------------------

# skip


=begin
A shiny new APPLY that works with a "restricted" Gannet:
-All variables in lambda function body must be arguments 
-All arguments must be quoted
-Only for compile-time decomposition
-Lambda in lambda is not allowed

BROKEN!

The whole thing is fundamentally broken.
What it should do is substitute the lambda arguments with the value from the data store,
as these should always be symbols anyway. 
So it should be a lot simpler!
1. get args: as before
2. get symbols via addresses: simply mget!
3. substitute

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

    def SBA_SCLib.ls_APPLY(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
            #core 
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word  
#        nargs=parent.n_args #t uint      
    	service=getName(service_word) #t Name_t  
    	#C++ bool use_redir; bool use_unique;
        if service==SC_APPLY
#iv
            print "APPLY CORE (#{parent.current_subtask}): \n"
#ev        
            use_redir=true
            if SEQVM==0            
            use_unique=true
            else # SEQVM==1
              use_unique=false  
            end # SEQVM
        elsif service==A_APPLYTC
        #iv
            puts "APPLY TAILCALL" 
        #ev            
            use_redir=false
            use_unique=false
        else
            raise "WRONG ALIAS FOR APPLY: #{service}"
        end
     # First check if this subtask was just waiting for an ACK   
         if sba_tile.service_manager.subtask_list.waiting_for_ack(parent.current_subtask)==1
            # OK, we received an ACK, time to clean up
            # The problem is that the core must return something. But no other service is expecting a packet. So that's useless.
            # So I'll use the waiting_for_ack flag to decide not to send any result packet, by setting the core status to "managed"
    #        raise "Got ACK"
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
            return [] #C++ Word_List empty_list; return empty_list;
        else
            # A new call to APPLY.
            unique=true #t bool
            result=0 #t Word
            lambda_function_args=[] #t Word_List 
#            called_function_args=[] #t Word_List
#            orig_kind=NONE #t Kind_t     
    
            # The first argument is the lambda definition, i.e. a list of symbols
            # all other arguments are symbols referencing values for the lambda variables
            # Loop over all argument value symbols in the subtask	   	
#            first=1 #t uint # skip the first arg, i.e. the LAMBDA part        
    
            lambda_def_address=addresses[0] #t MemAddress
#        .shift # C++ MemAddress lambda_def_address=addresses.front();addresses.pop_front();
                lambda_label=sba_tile.service_manager.symbol_table[lambda_def_address] #t Word
#iv                
                data_kind=getKind(lambda_label) #t Kind_t
                # What Kind can the arguments to APPLY have?
                # The LAMBDA will be L or R. Could it be U? Yes, e.g. recursive function
                # All others will be Q
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
                            if getKind(itemw) == K_U
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
##                        addresses.unshift(lambda_def_address) #skip 
                        result_list=[] #t Word_List  
#iv
                        puts "APPLY CORE: DEFER: task is not unique, setting subtask <#{parent.current_subtask}> to #{STS_pending}"
#ev                        
                        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_pending)
                        parent.core_status=CS_managed
                        return result_list
                    end 
#iv                    
                pp_data_kind=data_kind #C++ int pp_data_kind=(int)data_kind;
                pp_data_status=data_status #C++ int pp_data_status=(int)data_status;
                print  "APPLY CORE: #{parent.current_subtask}: CALLER ARG: (#{pp_data_kind}) #{ppSymbol(lambda_label)}=>#{lambda_def_address}=> <#{pp_data_status}>\n" 
                print  "APPLY CORE: #{parent.current_subtask}: CALLER VAL: ",ppPayload(sba_tile.data_store.mget(lambda_def_address)),"\n" 
#ev                
                # end of lambda definition handling    
##                addresses.shift #skip    
##            for data_address in addresses #t MemAddresses # after shift, only args are left
##                data_label=sba_tile.service_manager.symbol_table[data_address] #t Word
##                symbol_word_list=sba_tile.data_store.mget(data_address) #t Word_List
##                called_function_args.push(symbol_word_list)                
##            end # of loop over all data_labels
            
#            for i in 1..nargs #t uint
#                data_address=addresses[i] #t MemAddress
#                data_label=sba_tile.service_manager.symbol_table[data_address] #t Word
#                symbol_word_list=sba_tile.data_store.mget(data_address) #t Word_List
#                called_function_args.push(symbol_word_list)                            
#            end # of loop over all data_labels
            
##            addresses.unshift(lambda_def_address) #skip
            # In the brave new world, LAMBDA returns a list with first the arguments (if any) and then the references. 
            #iv 
            print "APPLY CORE: ","\nREWRITING R-code\n"
            #ev
            root_ref=1 #t uint
=begin
    The sequence is:
    * Loop through all reference symbols Ri for nested references (if any)
    * look up the code based on getSubtask(Ri)
    * loop through the code and substitute the K_U symbols with the corresponding symbol from called_function_args
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
#                lambda_function_packet=lambda_function_list #t Word_List

                #iv 
                puts "LAMBDA FUNCTION PACKET PAYLOAD:",ppPayload(lambda_function_definition) 
                #ev

                # .inspect if @v #skip
 
#                lambda_function_definition=lambda_function_packet #t Word_List
                # or just lambda_function_list minus the header! But to have a copy we would need to loop
                
                appl_function=[] #t Word_List #s/=..//        
                
                ext=0 #t uint
                ii=0 #t uint
                #iv
                    puts "LAMBDA FUNCTION LEN: #{lambda_function_definition.length}"
                #ev
                for i in 0..lambda_function_definition.length-1 #t uint
                    symbol_word=lambda_function_definition[i] #t Word
                    if ext==0
                        if  getKind(symbol_word)!=K_U
#                           appl_function[ii]=symbol_word    
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
#                                    data_label=sba_tile.service_manager.symbol_table[data_address] #t Word
                                    symbol_word_list=sba_tile.data_store.mget(data_address) #t Word_List                                    
                                    for val_word in symbol_word_list #t Word_List

#                                    for val_word in called_function_args[j] #t Word_List
                                        if first==1
#                                            appl_function[ii]=setQuoted(val_word,getQuoted(symbol_word)|getQuoted(val_word)) # this is the replacement strategy
                                            newsym=setQuoted(val_word,getQuoted(symbol_word)|getQuoted(val_word)) #t Word
                                            appl_function.push(newsym) 
                                            first=0
                                        else
#                                            appl_function[ii]=val_word # this is the replacement strategy for extended symbols
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
                        if getExt(symbol_word)==1 and (getKind(symbol_word)==K_B or getKind(symbol_word)==K_Q) # Only K_B symbols should extended!
                            ext=getSubtask(symbol_word)
                            #iv
                            puts "EXT: #{ppSymbol(symbol_word)} => ext=#{ext}"
                            #ev
                            next #C++ ii=ii-1;
                        end      
                    else # ext>0 
                        ext-=1
                        ii=ii+1                  
#                        appl_function[ii]=lambda_function_definition[i]
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
          
        
            return [result_symbol] #C++ Word_List result_symbol_l;result_symbol_l.push_back(result_symbol);return result_symbol_l;
        end # if APPLY
    end # of ls_APPLY



# kip

    # ----------------------------------------------------------------------------
    # WV10062008: CALL is essential! It allows to store quoted refs.
    # BUT: could we not do this simply by using LABEL? i.e.
    # (LET (ASSIGN 'f '(LAMBDA 'x '(* '6 x))) (APPLY (CALL (READ 'f)) '7))
    # (LET (LABEL f (LAMBDA 'x '(* '6 x))) (APPLY f '7))
    # Maybe we need both, what's the use of the LAMBDA call outside APPLY?
    # More importanly, could I use a LABEL as an argument? 

    def SBA_SCLib.ls_CALL(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
	#core
        print "#{parent.service} CORE: #{parent.current_subtask}: (CALL \n" if @v #skip
        argaddr=addresses[0] #t MemAddress
        result_list=[] #C++ Word_List result_list; 
        result_list=sba_tile.data_store.mget(argaddr)       
        result=result_list[0] #t Word
        parent.core_return_type=P_reference
        sba_tile.service_manager.subtask_list.to(parent.current_subtask,getName(result))
        if getKind(result)!=K_R
            raise "CALL only works on references (#{getKind(result)})!"
        end
#iv
        puts "CALL CORE: #{result}"
        puts "#{parent.service} CORE: )" if @v #skip
        puts "#{parent.service} CORE: #{parent.core_return_type} TO: #{sba_tile.service_manager.subtask_list.to(parent.current_subtask)}" if @v #skip
#ev
        return result_list
    end # of ls_CALL
    # ----------------------------------------------------------------------------
    # UNQUOTE does just that: you pass it quoted symbol, it returns the unquoted symbol.
    # I guess it only makes sense for non-extended symbols but maybe we should
    # be forward-compatible :-)
    # The main purpose is to have a way of passing variables without evaluating them
    # (let 
    #   (assign 'v (lambda 'n 'acc 'f '(if ... ... '(apply f ... ... 'f))
    #   (apply v ... ... (unquote 'v))
    # But in general, (S (unquote 'sym)) => S(sym) whereas (S sym) => S(val(sym))
    # for completeness: (S 'sym) => S('sym)
    def SBA_SCLib.ls_UNQUOTE(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
	#core
        print "#{parent.service} CORE: #{parent.current_subtask}: (UNQUOTE \n" if @v #skip
        argaddr=addresses[0] #t MemAddress
        result_list=[] #C++ Word_List result_list;
        result_list=sba_tile.data_store.mget(argaddr)       
        symbol=result_list[0] #t Word
        symbol=setQuoted(symbol,0)
        result_list[0]=symbol
#iv
        puts "UNQUOTE CORE: #{symbol}"
        puts "#{parent.service} CORE: )" if @v #skip
        puts "#{parent.service} CORE: #{parent.core_return_type} TO: #{sba_tile.service_manager.subtask_list.to(parent.current_subtask)}" if @v #skip
#ev
        return result_list
    end # of ls_UNQUOTE
    # ----------------------------------------------------------------------------
    
    # UNSYMBOL returns the Subtask field of unextended symbols or the extension words of extended symbols
    # I don't really like the name nor the idea that there is no Scheme equivalent, but it is an important service
    # Maybe RAW is a better name? or VAL? 
    def SBA_SCLib.ls_UNSYMBOL(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
	#core
        print "#{parent.service} CORE: #{parent.current_subtask}: (UNSYMBOL \n"
        argaddr=addresses[0] #t MemAddress
        datasymbol_list=sba_tile.data_store.mget(argaddr) #t Word_List
	result_list=[] #t Word_List
	for datasymbol in datasymbol_list #t Word_List
		if getExt(datasymbol)==0
#iv
		puts "Not EXT" 
#ev
			datavalue=getSubtask(datasymbol) #t Word 
			result_list.push(datavalue)
		else
#iv
		puts "EXT: #{getSubtask(datasymbol)}"
#ev
			for i in 1..getSubtask(datasymbol) #t int
				extsymbol=datasymbol_list[i] #t Word
				result_list.push(extsymbol)
			end
			break
		end
	end
#iv
        puts "#{parent.service} CORE: #{result_list[0]}"
        puts "#{parent.service} CORE: )"
#ev
        return result_list
    end # of ls_UNSYMBOL
    # ----------------------------------------------------------------------------
#skip
    
    # LOOP syntax: (loop 'task condition)
    # The task must be quoted as it's dispatched by the core
    # WV10062008: I don't think we need this, it can be done with LABEL
    def SBA_SCLib.ls_LOOP(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
        subtask=parent.current_subtask #t uint
        result=sba_tile.service_manager.subtask_list.return_as(subtask) #t Symbol
        # if the condition is met
        if cond!=0
            #first create a reference packet for the actual task and dispatch it
            task_return_as=sba_tile.service_manager.subtask_list.arguments(subtask).labels[0] #t Symbol # label of first argument
            # change Kind to R
            task_return_as_symbol=SBA_Symbol.new(task_return_as)
            task_return_as_symbol.Kind(K_R)
            task_return_as=task_return_as_symbol.to_num
            task_to=(task_return_as & F_Name)>>FS_Name #t Service
            task_return_to=sba_tile.service_manager.subtask_list.return_to(subtask) #t Service
            
            print "#{parent.service} CORE (#{subtask}): \n"
            print "#{parent.service} CORE : to : #{sba_tile.service_manager.subtask_list.to(subtask)}\n"
            print "#{parent.service} CORE : return_to : #{sba_tile.service_manager.subtask_list.to(subtask)}\n"
#            if REDIR==0
#            task_packet_header=SBA_Packet_Header.new(P_reference,1,1,task_to,task_return_to,result)
#            else
            task_packet_header=SBA_Packet_Header.new(P_reference,1,0,1,task_to,task_return_to,0,result)
#            end
            task_packet_payload=SBA_Packet_Payload.new(task_return_as)        
            task_packet=SBA_Packet.new(task_packet_header,task_packet_payload)
            sba_tile.transceiver.tx_fifo.push(task_packet)
            puts "#{parent.service} CORE: TASK: \n#{task_packet} "
            # now restart the loop task
            #WV: this should be conditional, so we need a conditional argument to loop
=begin
Now this is totally wrong! With this implementation, (loop ) will spew tasks as fast as it can, filling up
its child's reference packet queue _really fast_
What we need is a mechanism to tell loop to wait until the child tells it that it can send.
Maybe we could have the option to send an ACK to the parent, which would set a flag on the code referenced by the Return-as field
Seems a bit messy. But useful for things like LET as well: suppose LET redirects to its caller, then it could use an
ACK to know when to clean up. The ACK would be generated after evaluation of all args.
In case of (S1 (let ... '(S2 ...)))
let will send p(Ref,S2,S1,Rlet;R2)
So S2 can send an ACK for Rlet. But this means that let does an async action, i.e. the cleaning-up
It also means we need to define that action
However, maybe if the action is "clean up all data related to Rlet; set status of the task Rlet to 1",
then that makes a lot of sense. And it would stop loop from going crazy.
So we _must_ have this. It means another queue unless we use the management queue.
I guess this could be a good management scheme anyway: a flag on the stored task. 
There is some overhead but typically the ACK will be sent while the core is working.
=end
            parent.core_return_type=P_reference
            sba_tile.service_manager.subtask_list.to(subtask,parent.service) #t Service
            sba_tile.service_manager.subtask_list.return_to(subtask,task_return_to) #t Service
            puts "#{parent.service} CORE: LOOP: #{result}"
            puts "#{parent.service} CORE: )"
            puts "#{parent.service} CORE: #{parent.core_return_type} TO: #{sba_tile.service_manager.subtask_list.to(subtask)}"
            return result #C++ result_list.push_back(result); return result_list;   
        else
            # Condition is not met, so create an empty result packet that will return to the caller
            return nil #C++ return result_list;
        end
        
    end # of ls_LOOP
    # -----------------------------------------------------------------------------   
=begin
 How shall we handle the (fifo ...) service?
 Basically, the label for a fifo should not be cleared on return of the data
 So label would need a flag indicating that it should not be cleared
 (fifo 'f1 ...) => f1 should be special. Use a new Kind? e.g. same as Assign, but with an extra bit?
 Generalise this: the MSB of the Symbol can indicate Streaming
  
 Another question is how (fifo 'f1 (loop ...)) should work
 Should it return or not? It should, but only if the (loop ) call indicated that it was really done.
 It can do this only by sending a special type of packet to its parent. What about an empty packet, i.e. length==0?
 Then we can generalise that receipt of a 0-length packet with a given label results in freeing that label.
 Still no return though. I think I'm wrong somewhere: (fifo ) core will bind f1 to the return value of r_loop;
 Look at (assign ) code to see how this could work.
 I think it's like this:
 If the syntax is (fifo 'f1 (loop ...)) then the fifo core will be called when the first value from loop returns.
 So in that case the Service Manager should have marked r_loop as a stream.
 The consequence is that it is not possible to read from f1 until after it has at least a single value.
 What will the fifo core do? It should return a value. To which service? No point in sending anything to loop;
 but sending anything to let will result in cleaning-up -- UNLESS we only clean up when an empty packet arrives!
 Let's asumme we choose (fifo 'f1 '(loop ...)). Then the fifo core must do the dispatch, which it can do 
 instead of returning the label to let. From the moment of the dispatch of r_loop, read attempts on f1 are possible.
 The difference is mainly that in the unquoted case, a read attempt will result in a block because of f1 not defined;
 in the quoted case it will be because f1 is defined but empty.
 An important concern is that streams are not necessarily infinite. So say we end a stream by generating an empty result packet.
 The arrival of this packet should trigger a "proper" return of the fifo label to the let block.
 
 A workable solution might be like this:
 
 (fifo 'f1 (label L (loop ...)) (create 'f1 'L))
 
 So we introduce a new service CREATE which will create the fifo f1 to receive data from the task labeled L
 The CREATE service core
 - looks up the address allocated to L
 - binds f1 to that address
 So a request for f1 will return the head of L
 
 The FIFO service core
 - Returns L or f1 to the LET block (see ASSIGN)
 The FIFO service manager 
 -allocates an address to f1 and stores f1 there
 -allocates an address for L and waits for data
 -allocates an address for the return value of the create task
=end 
    ### MUST BE IMPLEMENTED AS ALIAS FOR ASSIGN!
    def SBA_SCLib.ls_FIFO_NOGOOD(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
        #core
        result=[]
        print "#{parent.service} CORE: #{parent.current_subtask}: (FIFO \n"   	
        value_list=[] #t Word_List
        # get the variable name ('f1') and the task reference ('r_task')
        for address in addresses
#            if SYM==0
#                print "#{parent.service} CORE: #{address}\t#{sba_tile.data_store.get(address)}\n"
#                value_list.push(sba_tile.data_store.get(address))                 
#            else # SYM==1
                try_array=sba_tile.data_store.mget(address)
                print "#{parent.service} CORE: #{address}\t#{pretty(try_array)}\n"
                value_list.push(sba_tile.data_store.mget(address)) 
#            end # SYM
        end  
        var=value_list[0] # first symbol is the variabe declaration
        task_ref=value_list[1] # second symbol is the function reference
        create_value=value_list[2] # return value of CREATE 
        
#        if SYM==0
#            if var.is_a?(Array)
#                var_name=var.shift # the function or variable name  
#                print "#{parent.service} CORE: WARNING: var_name is an Array! #{var.inspect}\n"   
#            else
#                var_name=var
#            end                      
#        else # SYM==1             
            var_name=var[0]
#        end # SYM
        result=var_name #t uint64            
        
#        if SYM==0
#            print result,"\n"
#        else # SYM==1
            print SBA_Symbol.new(result),"\n"
#        end # SYM
        parent.core_return_type=P_data	            
        # Skip clean-up
        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_cleanup)
        
        return result
    end # of FIFO_NOGOOD
    # -----------------------------------------------------------------------------    
 
=begin
    FIFO is the service that provides BUFFER, STREAM, GET, PEEK, EOS
    (BUFFER stream_id '(S ...)) 
    stores the value return by the expr in a fifo buffer. It actually stores the complete data packet for convenience.
    In HW, there will not be 1 FIFO service with multiple streams but as many HW FIFO services as there are 
    streams. In the VM a single FIFO service can deal with all of them. Won't be easy though. 
    So the datastructure we need is
    PacketFifo packet_fifo[NSTREAMS];
    But what we have is
    - a memory storing Packets
    - a state register storing Words 
    - a lookup table Word=>Word
    To implement an array of Fifo's we could do this:
    -Allocate NSTREAMS memory addresses as empty Fifo's
    -have a map streamid=>memaddress
    -we just push/shift the Word_List at the memory address
    
    -@service_core_ram is the starting address, we simply have NSTREAMS locations
    -we use state_register[0] for initialisations (really, there's no need, is there?)
    Although, what we do is simply: we store the data packet - or at least the data. This is done automatically.
    The address for the data is pushed onto the state register 
    
    WV10062008: In fact, I'll move the FIFO into the ServiceManager!! So no need for a service to provide it.
=end    
    def SBA_SCLib.ls_FIFO(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
        #core
        result=[]
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
        
        puts parent.current_subtask #skip
    	service=getName(service_word) #t Name_t
        parent.core_return_type=P_data
#iv
        print "FIFO (#{parent.service}) CORE: #{parent.current_subtask}:\n" 
#ev        
  
        if service==A_BUFFER        
        # a call to BUFFER results in the stream_id being stored in address 1 and a task ref in address 2
        # we then want to dispatch the task ref and reset the memory location's status so it waits for the data (the LET trick)
        # but we can of course already push the address onto the Word_List in @service_core_ram+stream_id
        stream_id_address=addresses[0]
     	stream_id_word=getValue(sba_tile.data_store.mget(stream_id_address)[0]) #t Word
if WORDSZ==64
                #C++ int64_t stream_id=(int64_t)stream_id_word;
else # WORDSZ==32
                #C++ int32_t stream_id=(int32_t)stream_id_word;
end # WORDSZ 
        stream_id=Integer(stream_id) #skip
#iv
                puts "ALU CORE: Found int #{stream_id} (#{T_i})"   
#ev                
            stream_address_fifo=sba_tile.service_manager.service_core_ram+stream_id
            data_address = addresses[1]
            sba_tile.data_store.mput(stream_address_fifo,data_address)
            # so far, so good, now dispatch the ref & do the magic
            # NOTE that we must store the reference! That's easy, just put it in the state_register.
        elsif service==A_STREAM
=begin
            STREAM is similar to BUFFER for the dispatch bitm but first we get the data
            from the FIFO and return it to the caller (in a data packet).
            Then we dispatch the reference
=end            
        elsif service==A_GET
        elsif service==A_PEEK
        elsif service==A_EOS
        else
            raise "FIFO CORE does not implement #{service}"
        end
        return result
    end # of ls_FIFO   
#endskip 

# ----------------------------------------------------------------------------------------------------------------------
#     
    def SBA_SCLib.ls_IO(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
        #core
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word        
        puts parent.current_subtask #skip
    	service=getName(service_word) #t Name_t
        parent.core_return_type=P_data
    # we use the lookup_table to map ports to file descriptors
    # in fact, in Ruby we don't care at all what we store in the table.
        port_address=addresses[0] #t MemAddress
        port_symbol=sba_tile.data_store.mget(port_address) #t Word_List
        builtin_symbol=port_symbol[0] #t Word
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
        if service==A_IO_OPEN
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
            return port_symbol
        elsif service==A_IO_CLOSE
            if parent.lookup_table.count(port)
            	fd=parent.lookup_table.read(port) #C++ fd=(FILE*)(parent.lookup_table.read(port));
            	fd.close() #C++ fclose(fd);
            	return pass
            else
            	return fail
            end        
        elsif service==A_IO_READ or service==A_IO_READLINE 
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
#                puts fd.inspect
                if not fd.eof #s/fd.eof/feof(fd)/             
                    inp = fd.readline #C++ fgets(inp,255,fd);
                end
            end
            #C++ 
            puts "CORE IOREAD: <#{inp}>" #skip
            if inp!=nil
                inp_sym = string2symbol(inp) #t Word_List # must return as a built-in symbol, so need a function. Should return an empty symbol if inp is nil
                puts inp_sym.inspect #skip
                return inp_sym
            else
                emptyK_B = NIL #t Word 
                emptysymbol = [emptyK_B]  #C++ Word_List emptysymbol;emptysymbol.push_back(emptyK_B);
                return emptysymbol
            end
                # or maybe we need a conditional outside the function
        elsif service==A_IO_WRITE
            data_address=addresses[1]  #t MemAddress       
            data_symbol = sba_tile.data_store.mget(data_address) #t Word_List
            #C++ string data;            
            kind=getKind(data_symbol[0]) #t Kind_t
            datatype=getDatatype(data_symbol[0]) #t Datatype_t
            if (kind==K_B and getExt(data_symbol[0])==1 and getSubtask(data_symbol[0])==0) # means it's an "empty" symbol
                return eof
            else
                 if (not parent.lookup_table.count(port))
                     puts "#{port}: #{data_symbol}" #skip 
                     return fail
                 else            
                     fd = parent.lookup_table.read(port) #C++ FILE* fd=(FILE*)(parent.lookup_table.read(port));
                     puts "CORE IOWRITE: #{port}: #{data_symbol}" #skip
                    if datatype==(T_s&1) and kind==K_Q
                        data=sym2str(data_symbol) #t string
                        #C++ fprintf(fd,"%s",data.c_str());
                    elsif  datatype==T_i
                        data=sym2int(data_symbol) #t Int           
                        #C++ fprintf(fd,"%d",(int)data);
                    elsif  datatype==T_f
                        data=sym2flt(data_symbol) #t Float            
                        #C++ fprintf(fd,"%f",data);
                    elsif  datatype==(T_i) and getUInt(data_symbol)<2 and kind==K_Q 
                        data=sym2bool(data_symbol) #t bool           
                        #C++ fprintf(fd,"%u",data);
                    else
                        raise "CORE IOWRITE: datatype #{datatype}"
                    end                
                     fd.puts data #skip
                     return pass
                 end
             end
        elsif service==A_IO_EOF
            #C++ Word_List res;
            if fd.eof #s/fd.eof/feof(fd)/
                res = [ONE] #C++ res.push_back(ONE); 
            else
                res = [ZERO] #C++ res.push_back(ZERO);
            end
            return res
        elsif service==A_IO_DISPLAY
        # just because DISPLAY is logically an IO function     
            result="" #skip
            for address in addresses #t MemAddresses
                data=sba_tile.data_store.mget(address) #t Word_List
                result+=">>>#{data.inspect}\n" #skip
=begin #C++
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
=end #C++
            end    
            puts result #skip
            return pass # result
        else
            raise "CORE StreamIO does not support #{service}"
        end
    end # of IO
   

    # ----------------------------------------------------------------------------------------------------------------------
    #     

        def SBA_SCLib.cs_DCT(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
            #core
            
            #C++ Word_List result_list;
        
    # now if result is an extended Symbol, and assuming we us a single word for numbers,
    # we could just take the next element:
    
        puts "DCT (#{parent.service}) CORE: #{addresses.length} addresses" if @v #skip
        address=addresses[0] #t MemAddress
        nruns=getValue(sba_tile.data_store.mget(address)[0]) #t Word
        res_symbol=sba_tile.data_store.mget(address)[0] #t Word
            
    #C++ result_list.push_back(res_symbol);
#iv
        puts "DCT: #{nruns} runs"
#ev 
    #C++ SCLib::calc_dct(nruns);
#iv
                    puts "DCT: done"
#ev      
            result_list=[res_symbol,nruns]  #C++ result_list.push_back(nruns);
            return result_list
            
        end
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# These are the services for dynamic reconfiguration
# It is currently a dummy in the sense that it returns a string in Ruby, a pointer in C++, not the actual binary data
# Obviously this is C/C++ specific, Ruby does this totally differently.
             
def SBA_SCLib.ds_CONFIG(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
    #core
    #iv
    service = parent.service #t  Service
    puts "#{service} DYNAMIC SERVICE CONFIG SERVER CORE" if @v #skip
 #ev
  configuration_address=addresses[0] #t MemAddress
  configuration_id=getValue(sba_tile.data_store.mget(configuration_address)[0]) #t Word

  # TODO: For different types of reconf substrates (uP, MORA, FPGA, ...); we ignore this for now
#  substrate_type_address=addresses[0] #t MemAddress
#  substrate_type=getValue(sba_tile.data_store.mget(substrate_type_address)[0]) #t Word

=begin #C++      
    Word_List result_list;

    // first check if there is a handle to close
    // get the service id of the caller
#ifndef SYSC  
    Return_to_t return_to=sba_tile.service_manager.subtask_list.return_to(parent.current_subtask);
#else
    Return_to_t return_to=parent.subtask_list.return_to(parent.current_subtask);
#endif     
    Word return_to_w=(Word)return_to;
    if (parent.lookup_table.count(return_to_w)!=0) {
        Word handle_w=parent.lookup_table.read(return_to_w);
        void* handle=(void*)handle_w;
#ifndef SYSC
//        int retval=dlclose(handle);
//        if (retval!=0) {
//          cerr << "Error on dlclose() for "<<handle_w<<" :"<<dlerror()<<"\n";
//        }
#endif  
    }
      // open the library
#ifndef SYSC
      string lib=sba_system.configurations[configuration_id].lib; 
      string configsym=sba_system.configurations[configuration_id].symbol;
#else    
    string lib=parent.cfg.configurations(configuration_id).lib; 
    string configsym=parent.cfg.configurations(configuration_id).symbol;
#endif // SYSC    
#ifndef SYSC                 
      void* handle = dlopen(lib.c_str(), RTLD_LAZY);
#else
void* handle = NULL;
#endif    
    const Word res_symbol = EXTSYM;  
    result_list.push_back(res_symbol);
#ifndef SYSC       
     if (!handle) {
         cerr << "Cannot open library: " << dlerror() << '\n';
         result_list.push_back((Word)1);
         return result_list;
     }
#endif  
    // for 32-bits host  
    //Word handle_uint_cast = (Word)handle;
    // HACK for 64-bits host
    Uint64 handle_uint_cast = (Uint64)handle;
#ifdef VERBOSE
  cout << "Handle for "<<lib<<"::"<<configsym<<": "<<handle_uint_cast<<endl;
#endif // VERBOSE

  parent.lookup_table.write(return_to_w,handle_uint_cast); // OK? is Sint64 
    // now do dlsym
  #ifdef VERBOSE
    cout << "Symbol:"<<configsym<< endl;
  #endif // VERBOSE
#ifndef SYSC   
    void* fp=dlsym(handle,configsym.c_str());
#else
    void* fp=NULL;
#endif    
    //uint fp_uint_cast = (uint)fp;
    // HACK! we need host detection 
    Uint64 fp_uint64_cast = (Uint64)fp;
#ifdef VERBOSE
    cout << "Pointer cast: " << fp_uint64_cast << endl;
#endif
    Uint64 fp_uint64_cast_l = fp_uint64_cast & 0xFFFFFFFF;
    Uint64 fp_uint64_cast_h = (fp_uint64_cast>>32) & 0xFFFFFFFF;
    Word fp_uint_cast_l = (Word)fp_uint64_cast_l;
    Word fp_uint_cast_h = (Word)fp_uint64_cast_h;
//    result_list.push_back(fp_uint_cast);
//    result_list.push_back(handle_uint_cast);
  
    result_list.push_back(fp_uint_cast_l);
    result_list.push_back(fp_uint_cast_h);
  
#ifdef SYSC  
  result_list.push_back((Word)configuration_id);
  uint config_sz=parent.cfg.configurations(configuration_id).configsz;
  for (Word w=0;w<config_sz;w++) {
    result_list.push_back(w);
  }

#endif // SYSC   
=end #C++

# In Ruby, open a file, read contents into a string;
# return that string
# We assume a configuration table in the YAML file maps unique numbers to the filenames

#skip
    lib=sba_tile.sba_system.configurations['ruby'][configuration_id][0] #t string
    configsym=sba_tile.sba_system.configurations['ruby'][configuration_id][1] #t string
     
    libh=File.open(lib,"r") 
    configsrc = ""
#    libh.each_byte {|byte|
#        configsrc += "#{byte}"
    libh.each_line { |line|
        configsrc += line        
    }     
  puts "#{service} DYNAMIC SERVICE CONFIG SERVER CORE: configuration #{configsym} from library #{lib}"
    res_symbol = 0xDD_0002_02
    # DD means 13|13 i.e. 1101 |11|01 i.e. a Quoted Extended Built-in with 2 ; Name field is 2 but should be 0
    result_list=[res_symbol,configsym,configsrc]      
#TODO: combine the bytes into Words and create a Word_List of them, more realistic
#endskip
    
# for SystemC, we want to create a packet with a realistic load
# based on MORA cores for the dynamic services.
# We can simply add the size of the packet in the YAML file for now.
# Once we have the actual MORA SystemC model we can do proper configurations.
# For testing, we assume that 
#   - the config time at ds_CONFIG is 20 cycles
#   - the config size is 15x96bits or 45 Words
#   - the config time at ds_DYNAMIC is 45 cycles, i.e. the packet size; maybe add 5 cycles offset
#   - the run time at ds_DYNAMIC is 15 cycles per byte, with maybe 5 cycles offset                
                    

    puts "#{service} DYNAMIC SERVICE CONFIG SERVER CORE: RESULT #{configsrc}" if @v #skip
    return result_list
end # of CONFIG
# --------------------------------------------------------------------------
# For full flexibility with streaming services that can reconfigure themselves
# recursively, we need a mechanism that will defer the actual reconfiguration
# until a streaming call returns.
# The idea is that the .run of the streaming call will take the caller's 
# config reference off the stack, get the config data and configure the core
# In fact, every .run call will do this, but only recursive calls will fill the stack,
# so an empty stack will not result in configuration
# FIXME! the defered calls to configure need a stack! otherwise RETURN_AS gets overwritten!
# encode an 8-position stack in a Word: 3 bits per position
# xxx|xxx|xxx|xxx|xxx|xxx|xxx|xxx||xxx
# sp=w&7
# pop() = (w>>(3+sp*3))&7; w=w&0xfffffffc+(sp+1)
# push(v)= ((v&7)<<(3+sp*3)) + (w>>(3+(sp+1)*3))<<(3+(sp+1)*3) + (sp-1)
# Neat enough. However, the stack won't do, we'll have to simply use the lookup table
# and store the word based on the current_subtask.
def SBA_SCLib.ds_DYNAMIC(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
    #core
    opcode = parent.opcode #t  Service
    
    #iv
    service = parent.service #t  Service
    puts "#{service} DYNAMIC SERVICE #{service} CORE: OPCODE #{opcode}" 
    #ev
    #C++ Word_List result_list;MemAddress configuration_id_address; MemAddress configuration_address;
    if opcode==A_S_RECONF 
      print "#{service} DYNAMIC SERIVCE #{service} CORE: CONFIGURING #{opcode} as " #skip
        configuration_id_address=addresses[0] 
        configuration_id = getValue(sba_tile.data_store.mget(configuration_id_address)[0]) #t Word
        current_configuration_id=parent.state_register[2] #t Word
        if configuration_id!=current_configuration_id
        configuration_address=addresses[1] 
        # So what's stored in the second Symbol?
        # if the call is deferred, it's a quoted K_R, else it's a quoted ext K_B
            configsym_w=sba_tile.data_store.mget(configuration_address)[1] #t Word
            configsrc_w=sba_tile.data_store.mget(configuration_address)[2] #t Word
            puts configsym_w #skip
#ifdef SYSC
        #C++ Word configuration_id=sba_tile.data_store.mget(configuration_address)[3];
        #C++ parent.state_register[4]=configuration_id;
#endif                
            # for Ruby, we store the strings in the lookup table
            #FIXME: this is rather broken, esp. for SystemC. 
            # what we need is to emulate the configuration time. The configuration data
            # will disappear from the data store as it is used to configure the core
            parent.lookup_table.write(configsym_w, configsrc_w) #skip     
            # Store the function pointer in the state register 0
            parent.state_register[0]=configsym_w
            parent.state_register[1]=configsrc_w 
            parent.state_register[2]=configuration_id
        end
            #FIXME: add 64-bit code
#C++      const Word res_symbol = 0xD5000001UL;         
            result_list=[0xD5000001] #C++  result_list.push_back(res_symbol);
    # ------------------------------
    # Run the configured core    
    elsif opcode==A_S_RUN or opcode==A_S_CONFRUN
        if opcode==A_S_CONFRUN
            # (s1.confrun '(config '2 '1) ...)
            # A minor problem is that we don't want to reconfigure unless the
            # configuration has actually changed; but confrun does not have that information,
            # not unless we use the 1st arg to store the configuration code.
            # we must configure the core first
            # to do so, we must first check if the config task was deferred
            puts addresses.inspect #skip
            #WV30112009: There is an issue with the addresses: they don't get cleaned up if we use shift;
            # however, if they get cleaned up then on EOS we get an error as the core tries to access them
            # It's clearly a result of the 2-stage function of the core. Some race condition
            # I think the problem is that the subtask is already scheduled so it's not skipped
            
            configuration_id_address=addresses[0]  
            configuration_address=addresses[1]  
            configuration_id = getValue(sba_tile.data_store.mget(configuration_id_address)[0]) #t Word
            current_configuration_id=parent.state_register[2] #t Word
            puts "#{service} DYNAMIC SERVICE #{service} CORE: #{current_configuration_id}<>#{configuration_id}" #skip
            if configuration_id!=current_configuration_id                                       
                # So what's stored in the second Symbol?
                # if the call is deferred, it's a quoted K_R, else it's a quoted ext K_B
                ref_symbol=sba_tile.data_store.mget(configuration_address)[0] #t Word            
                label=sba_tile.service_manager.symbol_table[configuration_address] #t Word              
                if getQuoted(label)==1 and getKind(ref_symbol)==K_R
                    puts "#{service} DYNAMIC SERVICE #{service} CORE: GET CONFIG" #skip
                    # Now fool the subtask:
                    # set status of argument to "requested"
                    sba_tile.service_manager.symbol_table[configuration_address]=setStatus(label,DS_requested)                        
                    # set status of subtask to "blocked"                       
                    sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_blocked)
                    puts "13 subtask #{parent.current_subtask} status is now STS_blocked" #skip
                    # for new status calculation
                    sba_tile.service_manager.subtask_list.incr_nargs_absent(parent.current_subtask)
                    # now we must send of a reference packet
                    # ideally the Service Manager should do this
                    # (s1.run (config '1 '1) (stream 'b1))
                    # so to becomes config; return_to is the service itself; type is reference; payload is the symbol
                    to=sba_tile.service_manager.subtask_list.to(parent.current_subtask) #t To_t
                    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
                    puts "TO: #{to}" #skip
                    puts "RETURN_AS: #{ppSymbol(return_as)}" #skip
                    # we can put to in the Name field of return_as, as the actual service is the current one
                    return_as_to=setName(return_as,to) #t Word
                    puts "RETURN_AS_TO: #{ppSymbol(return_as_to)}" #skip
                    # we must set the mode to M_normal, so we must save the mode somewhere
                    mode=sba_tile.service_manager.subtask_list.mode(parent.current_subtask) #t Mode_t
                    return_as_to_mode=setKind(return_as_to,mode) #t Word
                    # now we put this in a state register
                    parent.lookup_table.write(parent.current_subtask,return_as_to_mode)
                    sba_tile.service_manager.subtask_list.to(parent.current_subtask,S_CONFIG)
                    sba_tile.service_manager.subtask_list.return_to(parent.current_subtask,parent.service)
                    sba_tile.service_manager.subtask_list.mode(parent.current_subtask,M_normal)
                    var_label = setSubtask(label,configuration_address) #t Word
                    var_label = setName(var_label,parent.service)                
                    sba_tile.service_manager.subtask_list.return_as(parent.current_subtask,var_label)
                    parent.core_return_type= P_reference
                    ref_symbol=setQuoted(ref_symbol,0)
                    return [ref_symbol] #C++ result_list.push_back(ref_symbol); return result_list;
                else
                    puts "#{service} DYNAMIC SERVICE #{service} CORE: CONFIGURE" #skip
                # this means the config call has returned, we can simply configure the core
                    configsym_w=sba_tile.data_store.mget(configuration_address)[1] #t Word
                    configsrc_w=sba_tile.data_store.mget(configuration_address)[2] #t Word
                    puts configsym_w #skip
                    #ifdef SYSC
                    #C++ Word configuration_id=sba_tile.data_store.mget(configuration_address)[3];
                    #C++ parent.state_register[4]=configuration_id;
                    #endif                
                    # for Ruby, we store the strings in the lookup table
                    #FIXME: this is rather broken, esp. for SystemC. 
                    # what we need is to emulate the configuration time. The configuration data
                    # will disappear from the data store as it is used to configure the core
                    parent.lookup_table.write(configsym_w, configsrc_w) #skip     
                    # Store the function pointer in the state register 0
                    parent.state_register[0]=configsym_w
                    parent.state_register[1]=configsrc_w
    #                return_as_to_mode=parent.state_register[3] #t Word
                    return_as_to_mode=parent.lookup_table.read(parent.current_subtask) #t Word
                    mode=getKind(return_as_to_mode) #t Mode_t
                    to=getName(return_as_to_mode) #t To_t
                    return_as_to=setName(return_as_to_mode,parent.service) #t Word
                    return_as=setKind(return_as_to,K_R) #t Word
                    sba_tile.service_manager.subtask_list.to(parent.current_subtask,to)
                    sba_tile.service_manager.subtask_list.return_as(parent.current_subtask,return_as)
                    sba_tile.service_manager.subtask_list.mode(parent.current_subtask,mode)  
                    parent.state_register[2]=configuration_id   
                    puts "RETURN_AS: #{ppSymbol(return_as)}" #skip                             
                end    
            end            
        end    
      
        configsym_w=parent.state_register[0] #t Word
puts "#{service} DYNAMIC SERVICE #{service} CORE: RUNNING #{opcode} as #{configsym_w}" #skip
=begin #C++       
  Word configsym_w_h=parent.state_register[1];
  Uint64 configsym_l = (Uint64)configsym_w;
  Uint64 configsym_h = (Uint64)configsym_w_h;
  Uint64 configsym =   configsym_l + (configsym_h<<32);   
#ifdef VERBOSE       
  cout << "Pointer reconstruction: " << configsym << endl;
#endif       
#ifndef SYSC        
  Memory& store_ref=sba_tile.data_store;
  DynFuncPointer fp=(DynFuncPointer)configsym;        
#else
//  port_SC_Memory_if <MemAddress, Data>& store_ref=parent.data_store;
//  SC_DynFuncPointer fp=(SC_DynFuncPointer)configsym;        
#endif                                                        
 
#ifdef VERBOSE       
  cout << parent.service<< "CORE: CALLING dynamic service for subtask "<< parent.current_subtask <<"\n";
#endif          

if (opcode==A_S_CONFRUN) {
  configuration_id_address=addresses.front();addresses.pop_front();
  configuration_address=addresses.front();addresses.pop_front();
}
#ifndef SYSC
result_list=(*fp)(parent_ptr,(void*)(&store_ref),addresses);
#else // SYSC
//const Word res_symbol = 0xD500002AUL; // 42 
//  result_list.push_back(res_symbol);
result_list=sba_tile.data_store.mget(addresses[0]);
#endif
if (opcode==A_S_CONFRUN) {
addresses.push_front(configuration_address);
addresses.push_front(configuration_id_address);
}
      
#ifdef VERBOSE         
  cout << "CALLED dynamic service\n";  
#endif            
        
#ifdef SYSC
  Word configuration_id=parent.state_register[4];   
//  uint argsz = 64; //  TODO
    uint argsz=0;
    for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        MemAddress address=*iter_;
        argsz+=sba_tile.data_store.size(address);
    }
  uint t_setup=parent.cfg.configurations(configuration_id).t_setup;
  uint t_proc_value = parent.cfg.configurations(configuration_id).t_proc_value;
  uint t_core = t_setup + t_proc_value*argsz;
#ifdef SC_VERBOSE            
    OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << parent.service << "    DYNAMIC CORE "<< configuration_id<<": waiting "<<t_core<<" ns\n";            
#endif // SC_VERBOSE

  wait(t_core*_CLK_P, _CLK_U);            
#endif // SYSC        
=end #C++
                
#skip
      # Ruby version
      fstr=parent.lookup_table.read(configsym_w)   
if opcode==A_S_CONFRUN
      a0=addresses.shift
      a1=addresses.shift
end
      fcall=fstr+configsym_w+"(sba_tile,parent,addresses)"
    puts "<DYN CALL>",fcall,"</DYN CALL>" if @v #skip   
    result_list= eval(fcall)
if opcode==A_S_CONFRUN 
    addresses.unshift(a1)
    addresses.unshift(a0)
end
#endskip
    # this is necessary as the built-in IF/RETURN set it to P_reference
    parent.core_return_type= P_data
#iv         
    puts "DYNAMIC SERVICE #{service} CORE RESULT size: #{result_list.size}" 
#ev    
    else
        raise "Opcode #{opcode} not supported for ds_DYNAMIC #{A_S_CONFRUN}" 
    end
    
    return result_list
end # of ds_DYNAMIC

# This is a core that yields the CPU to the Service Manager after every iteration of a loop
# (yield '42) will perform 42 iterations
# now assume that this is working on block of data, then we want the result to be stored somewhere
# and not lose it between iterations!!!
def SBA_SCLib.ds_YIELD(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
    #core
    opcode = parent.opcode #t  Service
    
    #iv
    service = parent.service #t  Service
    puts "#{service} YIELDING SERVICE #{service} CORE: OPCODE #{opcode}" 
    #ev
    #C++ Word_List result_list;MemAddress configuration_id_address; MemAddress configuration_address;
    
    
    # parent.state_register[0]: state
    # parent.state_register[1]: niters
    
    # parent.core_status=CS_done
      # get the niters from the argument
      niters_address = addresses[0] #t MemAddress
      niters = getValue(sba_tile.data_store.mget(niters_address)[0]) #t uint
      i=parent.state_register[0] #t uint
      while (i!=niters)
          # do work
          # store the result of the work in a non-cleanup place
          # this is tricky
                    
          # increment niters
          niters+=1
          # now yield
          # store niters in a register
          parent.state_register[0]=niters 
          # now set the subtask to STS_pending 
          # set the core status to CS_managed
          # return an empty list
          return result_list
      end     
    
    return result_list
end # of ds_YIELD

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------     
#         
#    Perl 5 - Specific Services
#
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------     
# To make the ALU type-aware (int or float), we need to get the types of the arguments.
# So we need the labels of the arguments
# But this is a silly approach: we should simply have an FP ALU separately!

def SBA_SCLib.pl_ALU(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
#iv        
    puts "ALU CORE: processing subtask #{parent.current_subtask}"
#ev
    #C++ Word_List result_list;
    operation=parent.opcode #t Uint

# now if result is an extended Symbol, and assuming we us a single word for numbers,
# we could just take the next element:

    # if @v #skip
#iv        
puts "ALU (#{parent.service}) CORE: #{addresses.length} addresses"
#ev     
#    if addresses.length==0 #skip
#      exit(0) #skip
#    end  #skip
#    puts addresses.inspect #skip
address=addresses[0] #t MemAddress
res_symbol=sba_tile.data_store.mget(address)[0] #t Word
#C++ Word result;   
#C++ Int int_result;    
if getExt(res_symbol)==1
    result=sba_tile.data_store.mget(address)[1]
    int_result=(result>2**(WORDSZ-1))?(result-2**WORDSZ):result #C++ int_result=(Int)result;        
else
    result=getValue(res_symbol) # FIXME: this assumes the ALU is WORDSZ only, i.e. number of words in ext symbol==1
    one=1 #t Word
    int_result= (result>(one<< (FB_Value-1)))?(result-(one<< FB_Value)):result              
end                    
            result=Integer(int_result) #skip
#iv
            puts "ALU CORE: arg 1: Found int #{result} (#{T_i}) @ #{address}"   
#ev                
    if operation==M_ALU_not
        result=1-result
    else
        ii=0; #t int
        for address in addresses #t MemAddresses
            ii+=1
            if ii>1
            tres_symbol=sba_tile.data_store.mget(address)[0] #t Word
            #C++ Word tres;
            #C++ Int int_tres;   
            if getExt(tres_symbol)==1
                tres=sba_tile.data_store.mget(address)[1]
                int_tres=(tres>2**(WORDSZ-1))?(tres-2**WORDSZ):tres #C++ int_tres=(Int)tres;                    
            else
                tres=getValue(tres_symbol)
                one=1 #t Word
                int_tres= (tres>(one<<(FB_Value-1)))?(tres-(one<< FB_Value)):tres  
            end    
            
            tres=Integer(int_tres) #skip            
#iv
                    puts "ALU CORE: arg #{ii}: Found int #{tres} (#{T_i}) @ #{address}"   
#ev                        
            case operation
            when M_ALU_plus
                puts "ALU CORE operation: +" if @v #skip
                result=result+tres #C++ int_result+=int_tres;                    
            when M_ALU_minus
                puts "ALU CORE operation: -" if @v #skip
                result=result-tres #C++ int_result-=int_tres; 
            when M_ALU_times  
                puts "ALU CORE operation: *" if @v #skip 
                result=result*tres #C++ int_result*=int_tres;
            when M_ALU_over
                puts "ALU CORE operation: /" if @v #skip
                result=SBA_SCLib.div(result,tres) #C++ int_result=div(int_result,int_tres);
                # result=result/tres #/
            when M_ALU_lt
                puts "ALU CORE operation: <" if @v #skip
                result=(result<tres)?1:0 #C++ int_result=(int_result<int_tres)?1:0;
            when M_ALU_gt
                puts "ALU CORE operation: >" if @v #skip
                result=(result>tres)?1:0 #C++ int_result=(int_result>int_tres)?1:0;
            when M_ALU_eq
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
    return result_list
end # of ALU
# ----------------------------------------------------------------------------------------------------
 
# Perl FPU -- Ruby only
        
def SBA_SCLib.pl_FPU(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
#core

#iv        
puts "FP ALU CORE: processing subtask #{parent.current_subtask}"
#ev
                
#C++ Word_List result_list;

return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
operation=parent.opcode #t Uint

# now if result is an extended Symbol, and assuming we us a single word for numbers,
# we could just take the next element:

# if @v #skip
#iv        
puts "FP ALU (#{parent.service}) CORE: #{addresses.length} addresses"
#ev     
address=addresses[0] #t MemAddress
# FIXME: for 64-bit, a float is stored in the Name field, a double is extended
result=sba_tile.data_store.mget(address)[0] #t Word
if getExt(result)==1
result=sba_tile.data_store.mget(address)[1]
end    
res_symbol=sba_tile.data_store.mget(address)[0] #t Word    

#C++ result_list.push_back(res_symbol);

if WORDSZ==64                      
    #C++ double flt_result=word2dbl(result); 
    flt_result=[result].pack("Q").unpack("G")[0] #skip
else # WORDSZ==32
    #C++ float flt_result=word2flt(result);
    flt_result=[result].pack("N").unpack("g")[0] #skip 
end # WORDSZ
    result=flt_result #skip
    puts "FP ALU CORE: Found double #{result} (#{getDatatype(res_symbol)}<>#{T_i})" if @v  #skip

if operation==M_FPU_not
    result=1-result
else
    ii=0; #t int
    for address in addresses #t MemAddresses
        ii+=1
        if ii>1
            tres_symbol=sba_tile.data_store.mget(address)[0] #t Word
            tres=sba_tile.data_store.mget(address)[1] #t Word

            if WORDSZ==64
                flt_tres=[tres].pack("Q").unpack("G")[0] #C++ double flt_tres=word2dbl(tres);
            else # WORDSZ==32
                flt_tres=[tres].pack("N").unpack("g")[0] #C++ float flt_tres=word2flt(tres);
            end # WORDSZ
                tres=flt_tres #skip
                puts "FP ALU CORE: Found double #{tres}" if @v #skip
                
        case operation
        when M_FPU_plus
            puts "FP ALU CORE operation: +" if @v #skip
            result=result+tres #C++ flt_result+=flt_tres;                    
        when M_FPU_minus
            puts "FP ALU CORE operation: -" if @v #skip
            result=result-tres #C++ flt_result-=flt_tres; 
        when M_FPU_times  
            puts "FP ALU CORE operation: *" if @v #skip 
            result=result*tres #C++ flt_result*=flt_tres;
        when M_FPU_over
            puts "FP ALU CORE operation: /" if @v #skip
            result=result/tres #C++ flt_result=flt_result/flt_tres;
            # result=result/tres #/
        when M_FPU_lt
            puts "FP ALU CORE operation: <" if @v #skip
            result=(result<tres)?1:0 #C++ flt_result=(flt_result<flt_tres)?1:0;
        when M_FPU_gt
            puts "FP ALU CORE operation: >" if @v #skip
            result=(result>tres)?1:0 #C++ flt_result=(flt_result>flt_tres)?1:0;
        when M_FPU_eq
            puts "FP ALU CORE operation: ==" if @v #skip
            result=(result==tres)?1:0 #C++ flt_result=(flt_result==flt_tres)?1:0;
            #C++ break;}
        else #C++ default:
            raise "Unknown FP ALU CORE service: #{operation}" 
            #C++   exit(0);
        end #;
    end
    end
end
        #iv
        puts "FP ALU CORE RESULT (double): #{result}"  if @v #skip      
        #ev
#skip            
if WORDSZ==64               
         uint64_result=[result].pack("G").unpack("Q")[0]
         result=uint64_result #C++ result=dbl2word(flt_result);
         
else # WORDSZ==32
        uint32_result=[result].pack("g").unpack("N")[0] #WV: untested!
        result=uint32_result #C++ result=flt2word(flt_result);
end # WORDSZ
#endskip                
#iv    
puts "FP ALU CORE RESULT: (uint#{WORDSZ}) #{result}" 
puts "FP ALU (#{parent.service}) CORE (#{parent.current_subtask}):  result: #{result}"
#ev

result_list=[res_symbol,result]  #C++ result_list.push_back(result);

return result_list    
end # of FP_ALU    
    
# --------------------------------------------------------------------------

=begin
The Perl Array service:
Array.new creates a new array and returns the reference to it
In C++, this is 

    List< Word_List >* ap = new List< Word_List >();
    and then casting to Word

In Ruby, we have

    def new
        return []
    end

    def at(aref,idx)
        return aref[idx]
    end

    def push(aref,v)
        aref.push(v)
    end
    def pop(aref)
        return aref.pop
    end

Fundamental questions are:
- what is the format of the actual array?
Internally, it is List< Word_List >
For transfering between services, e.g. as the return value of Array.get,
it must be a Word_List as this is the only type we can return. 

The problem translates to: if I assign by value, how do I copy the array?
I guess the best solution is that we have a "copy constructor"
which takes a reference. So instead of get/set we have only copy. 

A minor problem is updating an element. I guess we simply have an update method,
let's call that one set? 

- what is the format of the array reference? It must have a Word that contains 
the service instance ID, so that LET can trigger a clean-up by sending a delete() call
or maybe even by sending a MM packet that results in a call to delete, this is better I think
So what is the Kind of this symbol? Ref?
=end
    
def SBA_SCLib.pl_Array(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
#core
    #iv        
    puts "Perl Array CORE: processing subtask #{parent.current_subtask}"
    #ev
                    
    #C++ Word_List result_list;
    
#    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
    method=parent.opcode #t Uint
    
    # now if result is an extended Symbol, and assuming we us a single word for numbers,
    # we could just take the next element:
    
    # if @v #skip
    #iv        
    puts "Perl Array (#{parent.service}) CORE: #{addresses.length} addresses"
    #ev
    #C++ Word apw; void* apv; List< Word_List >* ap;
    if method==M_Array_new
            aref = []
    elsif  method==M_Array_destroy
        #C++ MemAddress apw_address=addresses[0];
        #C++ Word apw=sba_tile.data_store.mget(apw_address)[1];
        #C++ void* apv=(void*)apw;
        #C++ List< Word_List >* ap= (List< Word_List >*) apv;
        #C++ delete ap; 
    else 
        aref_address=addresses[0] #t MemAddress
        apw=sba_tile.data_store.mget(aref_address)[1] #t Word
        aref=apw #skip
        #C++ void* apv=(void*)apw;
        #C++ List< Word_List >* ap= (List< Word_List >*) apv;
        #C++ List< Word_List >& aref= *ap;
        case method
            when M_Array_at
                idx_address=addresses[1] #t MemAddress
                idx=getUInt(sba_tile.data_store.mget(idx_address)) #t uint
                result_list = aref[idx] #t Word_List&  
            when M_Array_size
                alength = aref.length #t uint
                result_list.push(EXTSYM)
                result_list.push(alength) 
            when M_Array_push
                val_address=addresses[1]
                val=sba_tile.data_store.mget(val_address)[1];
                aref.push(val)
            when M_Array_pop
                result_list = aref.pop() 
            when M_Array_shift
                result_list = aref.shift() 
            when M_Array_unshift
                val_address=addresses[1]
                val=sba_tile.data_store.mget(val_address)[1];
                aref.unshift(val)
            when M_Array_set
                idx_address=addresses[1] #t MemAddress
                idx=getUInt(sba_tile.data_store.mget(idx_address)) #t uint
                val_address=addresses[2] #t MemAddress
                value_list=sba_tile.data_store.mget(val_address) #t Word_List
                aref[idx]=value_list
            when M_Array_copy
                # "Copy constructor"
                acref_address=addresses[0] #t MemAddress
                acpw=sba_tile.data_store.mget(acref_address)[1] #t Word
                acref=acpw #skip
                aref=acref.dup #skip
                arefsym = 0x7188 # FIXME 
                result_list.push(arefsym)
                result_list.push(aref)                             
            when M_Array_fromRange
                # "Constructor from Range"                  
                rref_address=addresses[0] #t MemAddress
                rpw=sba_tile.data_store.mget(acref_address)[1] #t Word
                rref=rpw #skip
                aref=rref.dup #skip
                arefsym = 0x7188 # FIXME 
                result_list.push(arefsym)
                result_list.push(aref)                             
                #C++ break;}
            else #C++ default:
                raise "Unknown Perl Array CORE service: #{method}" 
                #C++   exit(0);
        end #;    
    end
    return result_list
end # of pl_Array

def SBA_SCLib.pl_Hash(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
#core
    #iv        
    puts "Perl Hash CORE: processing subtask #{parent.current_subtask}"
    #ev
                    
    #C++ Word_List result_list;
    
#    return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
    method=parent.opcode #t Uint
    
    # now if result is an extended Symbol, and assuming we us a single word for numbers,
    # we could just take the next element:
    
    # if @v #skip
    #iv        
    puts "Perl Hash (#{parent.service}) CORE: #{addresses.length} addresses"
    #ev
    #C++ Word hpw; void* hpv; map< String, Word_List >* hp;
    if method==M_Hash_new
        href = []
        hrefsym = 0x7188 # FIXME 
        result_list.push(hrefsym)
        result_list.push(href)             
    elsif  method==M_Hash_destroy
        #C++ MemAddress hpw_address=addresses[0];
        #C++ Word hpw=sba_tile.data_store.mget(hpw_address)[1];
        #C++ void* hpv=(void*)hpw;
        #C++ map< String, Word_List >* hp= (map< String, Word_List >*) hpv;
        #C++ delete ap; 
    else 
        href_address=addresses[0] #t MemAddress
        hpw=sba_tile.data_store.mget(href_address)[1] #t Word
        href=hpw #skip
        #C++ void* hpv=(void*)hpw;
        #C++ map< String, Word_List >* hp= (map< String, Word_List >*) hpv;
        #C++ map< String, Word_List >& href= *hp;
        case method
            when M_Hash_lookup
                key_address=addresses[1] #t MemAddress
                key=getUInt(sba_tile.data_store.mget(idx_address)) #t uint
                result_list = href[idx] #t Word_List&  
            when M_Hash_insert
                alength = aref.length #t uint
                result_list.push(EXTSYM)
                result_list.push(alength) 
            when M_Hash_delete
            when M_Hash_exists
            when M_Hash_size
            when M_Hash_keys 
            when M_Hash_values
            when M_Hash_set
                idx_address=addresses[1] #t MemAddress
                idx=getUInt(sba_tile.data_store.mget(idx_address)) #t uint
                val_address=addresses[2] #t MemAddress
                value_list=sba_tile.data_store.mget(val_address) #t Word_List
                aref[idx]=value_list
            when M_Hash_copy
                # "Copy constructor"
                acref_address=addresses[0] #t MemAddress
                acpw=sba_tile.data_store.mget(acref_address)[1] #t Word
                acref=acpw #skip
                aref=acref.dup #skip
                arefsym = 0x7188 # FIXME 
                result_list.push(arefsym)
                result_list.push(aref)                             
            when M_Hash_fromList
                # "Constructor from Range"                  
                rref_address=addresses[0] #t MemAddress
                rpw=sba_tile.data_store.mget(acref_address)[1] #t Word
                rref=rpw #skip
                aref=rref.dup #skip
                arefsym = 0x7188 # FIXME 
                result_list.push(arefsym)
                result_list.push(aref)
            when M_Hash_toList
                #C++ break;}
            else #C++ default:
                raise "Unknown Perl Hash CORE service: #{method}" 
                #C++   exit(0);
        end #;    
    end
    return result_list
end # of pl_Hash

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------     
#         
# this is a dummy for unused services
def SBA_SCLib.none(sba_tile,parent,addresses) #t Result (na;Base::ServiceCore*;MemAddresses&)
    return 0 #C++ Result res; res.push_back((Word)0); return res;
end        
        
#endif // NO_SERVICES
end # of SBA_SCLib 

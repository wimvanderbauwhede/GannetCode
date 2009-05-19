# ServiceCoreLibrary.rb
#   
# :title: Service-based SoC project - Service Core Library 
#
#    This version of Gannet is for compile-time decomposition and lexical scoping.
#    It does not work with the "old" system any more!!
#    This is the version to be ported to SystemC
#    -"values" is deprecated, we pass on addresses!
#    -The language services are minimal:
#    LET x
#    ASSIGN x
#    LAMBDA 
#    APPLY 
#    IF x
#    We'll keep BEGIN, though I see no use for it
#    -List manipulation services need a total rework. Initially, we'll support
#    LIST
#    HEAD
#    TAIL
#    LENGTH
#    CONCAT
#    and maybe CONS
#    
#    -ALU services are as before: x
#    +,-,*,/;<,==,>
#    Guess we should add AND, OR, NOT
#    
#--
#
#/* ***** BEGIN LICENSE BLOCK *****
# * Version: AFL 2.1
# *
# * The contents of this file are subject to the Academic Free License Version
# * 2.1 (the "License"); you may not use this file except in compliance with
# * the License. You may obtain a copy of the License at
# * http://opensource.org/licenses/afl-2.1.php
# *
# * Software distributed under the License is distributed on an "AS IS" basis,
# * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# * for the specific language governing rights and limitations under the
# * License.
# *
# *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
# *
# * ***** END LICENSE BLOCK ***** */
#
#
#// $Id: ServiceCoreLibrary.rb 2532 2009-04-22 16:15:08Z socgroup $
#++

# -----------------------------------------------------------------------------

=begin #inc
#ifndef SYSC
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/ServiceCore.h" //skipcc
#include "ServiceCoreLibrary.h" //skiph
#include "System.h" //skiph
#include "Tile.h" //skiph
#include "ServiceCore.h" //skiph
#include "cs_DCT.h" //skipcc
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

def SBA_SCLib.getCodeAddress(word) #t CodeAddress
    # extract page form word
    page=(word & F_CodePage)>> FS_Task #t Word
    # extract address from Work
    address=(word & F_CodeAddress) >> FS_Subtask #t Word    
if VM==1
    service=(word & F_Service)>>FS_Name #t Word
    code_address=(service << FS_Service)+(page<<FS_CodePage)+address #t Word    
else # VM==0    
    code_address=(page << FS_CodePage)+address #t Word
end # VM    
#iv    
    puts "Service(SCLib): #{service} | Page: #{page} | Address: #{address} => #{code_address}"
#ev    
	return code_address
end

def SBA_SCLib.mkHeader(packet_type,prio,redir,length,to,return_to,ack_to,return_as) 
	wpacket_type=(packet_type << FS_Packet_type) & F_Packet_type 
	wprio=(prio << FS_Prio) & F_Prio 
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
def SBA_SCLib.getPrio(header) 
	w1=header[0] 
	return (w1 & F_Prio) >> FS_Prio
end
def SBA_SCLib.setPrio(header,field) 
	modheader=[]  
	w0=(header[0] & FN_Prio) + (field << FS_Prio) 
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
#        os="#{packettype_l(SBA_SCLib.getPacket_type(wl))}:#{SBA_SCLib.getPrio(wl)}:#{SBA_SCLib.getRedir(wl)}:#{SBA_SCLib.getLength(wl)}:#{num2name(SBA_SCLib.getTo(wl))}:#{num2name(SBA_SCLib.getReturn_to(wl))}\n#{w2}\n#{w3} (#{ppSymbol(w3)})" 
#        else
#        os="#{SBA_SCLib.getPacket_type(wl)}:#{SBA_SCLib.getPrio(wl)}:#{SBA_SCLib.getRedir(wl)}:#{SBA_SCLib.getLength(wl)}:#{num2name(SBA_SCLib.getTo(wl))}:#{SBA_SCLib.getReturn_to(wl)}\n#{w2}\n#{w3} (#{ppSymbol(w3)})" 
#        end
#		return os
#    end
    def SBA_SCLib.ppHeader(wl) #t string (Header_t)
        w2=wl[1] #t Word
        w3=wl[2] #t Word

#C++	ostringstream outs;
#C++	outs << (uint)getPacket_type(wl) << ":" << (uint)getPrio(wl) <<":"<< (uint)getRedir(wl) <<":"<<(uint)getLength(wl)<<":"<< (uint)getTo(wl) <<":"<<(uint)getReturn_to(wl) <<"\n"<< (uint)w2<<"\n"<<(uint)w3;
        if @v
        os="#{packettype_l(getPacket_type(wl))}:#{getPrio(wl)}:#{getRedir(wl)}:#{getLength(wl)}:#{num2name(getTo(wl)).downcase}:#{num2name(getReturn_to(wl)).downcase}\n#{w2}\n#{ppSymbol(w3)} (#{w3})" #skip
        else 
        os="#{getPacket_type(wl)}:#{getPrio(wl)}:#{getRedir(wl)}:#{getLength(wl)}:#{getTo(wl)}:#{getReturn_to(wl)}\n#{w2}\n#{w3}" #skip
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

#ifndef NO_SERVICES
    #---------------------------------------------------------------------------
    # Helper functions for IO
    #---------------------------------------------------------------------------
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
        header=sym.shift #skip
        raise "Integer Ext length must be 1!" unless getSubtask(header)==1 #skip
        result=sym.shift #C++ Word result=sym.front();sym.pop_front();
if WORDSZ==64
        int_result=result>2**63?result-2**64:result #C++ int64_t int_result=(int64_t)result;
else # WORDSZ==32
        int_result=(result>2**31)?result-2**32:result #C++ int32_t int_result=(int32_t)result;
end # WORDSZ                
        result=Integer(int_result) #skip
        return result #s/res/int_res/
    end
    
    def SBA_SCLib.sym2uint(sym) #t Word (Word_List)
        header=sym.shift #skip 
        raise "Integer Ext length must be 1!" unless getSubtask(header)==1 #skip
        result=sym.shift #C++ Word result=sym.front();sym.pop_front();
        result=Integer(int_result) #skip
        return result 
    end
    
    def SBA_SCLib.sym2bool(sym) #t bool (Word_List)
        header=sym.shift #skip
        raise "Integer Ext length must be 1!" unless getSubtask(header)==1 #skip
        result=sym.shift #C++ Word result=sym.front();sym.pop_front();
    if WORDSZ==64
        int_result=result>2**63?result-2**64:result #C++ int64_t int_result=(int64_t)result;
    else # WORDSZ==32
        int_result=(result>2**31)?result-2**32:result #C++ int32_t int_result=(int32_t)result;
    end # WORDSZ                
        result=Integer(int_result) #skip
        return (result!=0) #s/res/int_res/
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
    def SBA_SCLib.sba_GATEWAY(sba_system,sba_tile,parent,addresses) #t Result (na;na;Base::ServiceCore*;MemAddresses&)
        return 1 #C++ Result res; res.push_back((Word)1); return res;
    end



#ifndef NO_SERVICES    
    #---------------------------------------------------------------------------
    #skip
    def SBA_SCLib.display(sba_system,sba_tile,parent,addresses)
        print "#{parent.service} CORE (#{parent.current_subtask}): \n"
        result=''
        for address in addresses
            if SYM==0
                result+=">>>#{sba_tile.data_store.get(address)}\n"             
            else # SYM==1
                result+=">>>#{sba_tile.data_store.mget(address).inspect}\n"          
            end # SYM   
        end    
        puts result
        return [1] # result
    end # of display
    #endskip
    #---------------------------------------------------------------------------
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
    
    #---------------------------------------------------------------------------
    # To make the ALU type-aware (int or float), we need to get the types of the arguments.
    # So we need the labels of the arguments
    
    def SBA_SCLib.ls_ALU(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core

#iv        
        puts "ALU CORE: processing subtask #{parent.current_subtask}"
#ev

                        
        #C++ Word_List result_list;
if FP==1
        return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
end # FP
        fp=0 #t Int
        for address in addresses #t MemAddresses
            if getDatatype(sba_tile.service_manager.symbol_table[address]) != T_i # of course we could use a bitmask
                fp=1
                break
            end
        end
        operation=parent.opcode #t Uint

# now if result is an extended Symbol, and assuming we us a single word for numbers,
# we could just take the next element:

        # if @v #skip
#iv        
    puts "ALU (#{parent.service}) CORE: #{addresses.length} addresses"
#ev     
    if addresses.length==0 #skip
      exit(0) #skip
    end  #skip
    puts addresses.inspect #skip
	address=addresses[0] #t MemAddress
 	result=sba_tile.data_store.mget(address)[1] #t Word
 	if not result.is_a?(Integer) #skip
 	    puts "NIL: #{result.class}" #skip
 	    exit(0) #skip
 	end #skip
    res_symbol=sba_tile.data_store.mget(address)[0] #t Word
    

#C++ result_list.push_back(res_symbol);

# But only if extended quoted symbols are handled correctly 
            if (FP==1 and getDatatype(res_symbol)==T_i) or FP==0
if WORDSZ==64
                int_result=result>2**63?result-2**64:result #C++ Int int_result=(Int)result;
else # WORDSZ==32
                        int_result=(result>2**31)?result-2**32:result #C++ Int int_result=(Int)result;
end # WORDSZ                
                result=Integer(int_result) #skip
#iv
                puts "ALU CORE: arg 1: Found int #{result} (#{T_i}) @ #{address}"   
#ev                
            else # FP==1   
            if WORDSZ==64
                flt_result=[result].pack("Q").unpack("G")[0]
            else # WORDSZ==32
                flt_result=[result].pack("N").unpack("g")[0] 
            end # WORDSZ
                result=flt_result
                puts "ALU CORE: Found double #{result} (#{getDatatype(res_symbol)}<>#{T_i})" if @v  #skip
            end # FP      

        if operation==A_not
            result=1-result
        else
            ii=0; #t int
            for address in addresses #t MemAddresses
#                label=arglabels[ii]
                ii+=1
                if ii>1
if FP==1                
                    tres_symbol=sba_tile.data_store.mget(address)[0] #t Word
end # FP
                    tres=sba_tile.data_store.mget(address)[1] #t Word

                    if (FP==1 and (tres_symbol & F_Datatype)>>FS_Datatype==T_i) or FP==0 # FP==0
                        int_tres=(tres>2**(WORDSZ-1))?(tres-2**WORDSZ):tres #C++ Int int_tres=(Int)tres;
                        tres=Integer(int_tres) #skip            
#iv
                        puts "ALU CORE: arg #{ii}: Found int #{tres} (#{T_i}) @ #{address}"   
#ev                        
                    else # FP==1       
                    if WORDSZ==64
                        flt_tres=[tres].pack("Q").unpack("G")[0]
                    else # WORDSZ==32
                        flt_tres=[tres].pack("N").unpack("g")[0]
                    end # WORDSZ
                        tres=flt_tres
                        puts "ALU CORE: Found double #{tres}" if @v #skip
                    end # FP

                case operation
                when A_plus
                    puts "ALU CORE operation: +" if @v #skip
                    result=result+tres #C++ int_result+=int_tres;                    
#                    puts "PLUS RES: #{result}"
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
            if (FP==1 and getDatatype(return_as)== T_i) or FP==0 # FP==0
                puts "ALU CORE RESULT (signed int): #{result}" if @v #skip      
                result=Integer((result<0)?(2**WORDSZ+result):result) #C++ result=(Uint)int_result;                
            else # FP==1
                #iv
                puts "ALU CORE RESULT (double): #{result}"  if @v #skip      
                #ev
if WORDSZ==64               
                 uint64_result=[result].pack("G").unpack("Q")[0]
                 result=uint64_result
else # WORDSZ==32
                uint32_result=[result].pack("g").unpack("N")[0] #WV: untested!
                result=uint32_result
end # WORDSZ                
            end # FP
        #iv    
        puts "ALU CORE RESULT: (uint#{WORDSZ}) #{result}" 
        puts "ALU (#{parent.service}) CORE (#{parent.current_subtask}):  result: #{result}"
        #ev
 
        result_list=[res_symbol,result]  #C++ result_list.push_back(result);
        return result_list
    end # of ALU
    
    #---------------------------------------------------------------------------    
    # This is a simple proof-of-concept example of a service that does not return in "zero time". The service takes count_upto
    # "clock cycles" and returns the sum of all counts
    # "clock cycles" are really parse cycles, i.e. the status will toggle between CS_ready and CS_busy
    # With concurrent tasks this might behave weirdly...
    # 
    # This is a way to emulate concurrency: the core is called at every parse cycle and returns immediately, holding its status to CS_busy
    def SBA_SCLib.ls_COUNTER(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
#        service=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
#        operation=(service & F_Name) >> FS_Name #t uint

        address=addresses[0] #t MemAddress
        count_upto=sba_tile.data_store.mget(address)[1] #t Word
        res_symbol=sba_tile.data_store.mget(address)[0] #t Word
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
    
    #---------------------------------------------------------------------------
    # COUNTDOWN is an example of the use of the state register
    # state register 0 containts the actual state: 0 before initialisation, 1 during countdown
    # state register 1 stores the actual counter when in state 0 and decrements it when in state 1
    # Although this looks like a persistent task, the task does not "spend time" like a thread would.   
    def SBA_SCLib.ls_COUNTDOWN(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        #C++ Word_List result_list;
#        service=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word
#        operation=(service & F_Name) >> FS_Name #t uint
        puts "COUNTDOWN called: #{parent.state_register[1]}" #skip
        address=addresses[0] #t MemAddress
        count_downfrom=sba_tile.data_store.mget(address)[1] #t Word
        res_symbol=3707764737 #C++ const Word res_symbol = 0xDD000001UL;
        #C++ result_list.push_back(res_symbol);
        # res_symbol sba_tile.data_store.mget(address)[0] #t Word
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

    #---------------------------------------------------------------------------
    # FIB is a helper to compute the Fibonacci series, similar to COUNTDOWN
    def SBA_SCLib.ls_FIB(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
        # The symbol for a 32-bit signed int is a constant
        res_symbol=3707764737  #C++ const Word res_symbol = 0xDD000001UL;        
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
    end # of COUNTDOWN
            
    # This is one step further: the service core launches a thread and monitors a state register. The thread writes 2 to the state reg
    # when it's done. As long as the reg is 1, core status stays CS_busy
    # This is not portable as such to C++, neither is there much point in doing it for the VM;
    # but this shows that a number of HW service cores could run in parallel. The VM Is effectively
    # polling the service cores on every iteration of the main loop
    # NOTE 10/01/2009: The C++ code doesn't work as the main program exits because the tread is detached
    def SBA_SCLib.ls_THREAD(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
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
    def SBA_SCLib.ls_DELAY(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core
#        puts "DELAY CORE #{parent.tid} called: #{parent.state_register[0]}"
        # The symbol for a 32-bit signed int is a constant
        res_symbol=3707764737  #C++ const Word res_symbol = 0xDD000001UL;        
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
    def SBA_SCLib.ls_HWTHREAD(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
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
                input_data.push(sba_tile.data_store.mget(address)[1]) #s/push/push_back/
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

    #---------------------------------------------------------------------------
    
    #--
    # We need at least following "abstract" services:
    #---------------------------------------------------------------------------
    #++
    
    # The BEGIN core takes in all arguments, checks for Error and returns the last result or Error
    # WV:Do we need an Error Kind?
    def SBA_SCLib.ls_BEGIN(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/ 
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
    end # of BEGIN
    
    #---------------------------------------------------------------------------
    # the actual data store service core
    # WV: For now, we use a global hash to store the DATA packets, and put them in the DATA store every time the service receives a (DATA ...) subtask. 
    # What we really should do is wait for the DATA packets to arrive and store them; if a (DATA ...) subtask would arrive before the packet is there, this subtask would have a 0 type ('empty') until the packet arrives. On the other hand, any DATA packet arriving at the gateway will automtically be sent to the DATA store. So do we really need (DATA ...)?
    # Well, maybe like this: the Originator of the task should not just send DATA. the DATA service will allocate space and request data packets for every subtask it receives. Unrequested data packets will be dropped. Security!!
    
    def SBA_SCLib.ls_DATA(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
        #core
        status=1 #t Word # for OK
        #C++ Word_List status_list;
        #C++ status_list.push_back(status);
    raise "ls_DATA broken!"
     
    end # of DATA
    #---------------------------------------------------------------------------
    def SBA_SCLib.ls_IF(sba_system,sba_tile,parent,addresses)#t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
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
                condval=sba_tile.data_store.mget(condaddr)[1] # quoted numbers!
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
#                if  getQuoted(result)>0
#                    # If it's a quoted expression =>  Just return it
#                    parent.core_return_type=P_data    
#                else # result.Quoted==0
                    if getKind(result) == K_S or getKind(result) == K_U
                        # If it's an expression => Error
                        raise "IF CORE: ERROR: IF arg can't be #{getKind(result)}"
                    elsif getKind(result) == K_L or getKind(result) == K_D
                        raise "IF CORE: ERROR: IF arg can't be #{getKind(result)}"
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

                        puts "IF CORE REDIR/wait for ACK"  if @v #skip
                        # CORE decides to redirect
#                        sba_tile.service_manager.subtask_list.redir(parent.current_subtask,1)
                        # Now what? I thought the IF core _always_ redirects. ack_to is the label for the ACK
                        # So it contains the name of the IF service and the address of the argument, i.e valaddress
                        # See parse_subtask_packet, it's the same code
                        
                        # WV21082008: We need IFTC and RETURNTC: there's no need for an ACK in a tail-called loop
                        # as there is nothing to clean up anyway
                        send_ack_to=0 #t Word
                        if operation==A_RETURN or operation==SC_IF
                            send_ack_to=setName(label,S_IF) 
                            send_ack_to=setSubtask(send_ack_to,valaddress)
#                        sba_tile.service_manager.subtask_list.ack_to(parent.current_subtask,send_ack_to)  
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
#                            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_processed)
                            # WV 15042009: STS_processed leads to a race condition between activate and clean-up
                            sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_inactive)                            
                        #iv
                            puts "IF CORE: TAILCALL: #{operation}"
                        #ev
                        end
                        parent.core_status=CS_managed
                        packet_type=P_reference #t Packet_Type
                        prio=0 #t Prio_t
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
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
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


    #------------------------------------------------------------------------------    

=begin
The IF in the ServiceManager (S_IF) can only take quoted values. It can take K_R (not K_C), K_D and K_B
As far as I can see, this IF does not need ACK as it delivers locally; anyway ACK would be too complicated
So the assumption is that this IF always delivers locally, i.e. (S1 ... (S1-IF ...))
=end

    def SBA_SCLib.ls_S_IF(sba_system,sba_tile,parent,addresses)#t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        operation=parent.opcode #t Word
        valaddress=0 #t MemAddress 
        if operation==A_RETURN 
            valaddress=addresses[0]
        else # must be IF
            condaddr=addresses[0] #t MemAddress
            condval=sba_tile.data_store.mget(condaddr)[1] #t Word
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
            parent.core_return_type= P_reference
        end          
        
        return result_list
        
    end # of ls_S_IF        
    #------------------------------------------------------------------------------    

    def SBA_SCLib.ls_RAND(sba_system,sba_tile,parent,addresses)#t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
#        operation=parent.opcode #t Word
#C++    Word_List result_list;        
        address0=addresses[0] #t MemAddress
        min_val=sba_tile.data_store.mget(address0)[1] #t Word
        address1=addresses[1] #t MemAddress
        max_val=sba_tile.data_store.mget(address1)[1] #t Word
        res_symbol=3707764737 #C++ const Word res_symbol = 0xDD000001UL;
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
    #------------------------------------------------------------------------------      

    #------------------------------------------------------------------------------    

    def SBA_SCLib.ls_RND_MATRIX(sba_system,sba_tile,parent,addresses)#t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
#        operation=parent.opcode #t Word
        result_list=[] #C++ Word_List result_list;        
        min_val=0 #t Word
        max_val=255 #t Word
        for i in 0..63 #t int
            res_val=min_val+rand(max_val) #C++ Word res_val=min_val+(random() % max_val);
            result_list.push(res_val)  #C++ result_list.push_back(res_val);
#iv            
            puts "RAND MATRIX CORE RESULT"
            puts "#{ppPayload(result_list)}" #skip
#ev             
        end
        return result_list
        
    end # of ls_RND_MATRIX     
    #------------------------------------------------------------------------------   

    #------------------------------------------------------------------------------    

    def SBA_SCLib.ls_PROC_MATRIX(sba_system,sba_tile,parent,addresses)#t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
        result_list=[] #C++ Word_List result_list;
#        operation=parent.opcode #t Uint
        first=true #t bool
        for address in addresses #t MemAddresses
            matrix=sba_tile.data_store.mget(address) #t Word_List
            if first            
                for elt in matrix #t Word_List      
                    result_list.push(elt)  #C++ result_list.push_back(elt);
                    puts "PROC returns #{elt}" #skip                    
                    end
                first=false
            end
        end    
        return result_list
    end # of ls_PROC_MATRIX     
    #------------------------------------------------------------------------------          
    # With LET aliased to ASSIGN, in principle we could register all calls to ASSIGN'ed variables.
    # Then LET could multicast to those services only the list of variables to clean up.
    # Direct addressing is very risky, if not impossible, for this: the variable will be stored at different addresses
    # for every service involved. But ASSIGN _knows_ those addresses!
    # 
 
    def SBA_SCLib.ls_LET(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
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
                print "LET (#{parent.service}) CORE: ", "LABEL:", label,"\n"             
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
                    prio=0 #t Prio_t
                    redir=0 #t Redir_t
                    reslist=[] #t Word_List
                    reslist.push(numval) #s/push/push_back/
                    payload_length=1 #t Length_t
=begin
So what happens if it's a tail call?
- redirect
- but exit, i.e. should jump past the return
=end
#                    use_redir=false #t bool
                    use_redir=true #t bool
                    
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
                            sba_tile.transceiver.tx_fifo.push(ref_packet)
                        else
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
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
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
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
#                                nlookup.push(word) #s/push/push_back/
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
                    value_list.push(sba_tile.data_store.mget(address))  #s/push/push_back/
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
                    puts "ADDRESS FROM STACK: #{var_address}" #skip
                    word=setSubtask(var_label,var_address) #t Word
                    word=setStatus(word,DS_present)
                    puts "ASSIGN: NAME: #{var_name}=>ADDRESS:#{var_address}" if @v #skip
#iv
                    puts "ASSIGN: STS=#{sba_tile.service_manager.subtask_list.status(parent.current_subtask)}"
#ev                    
#                    lookup.push(word) #s/push/push_back/
#                    sba_tile.data_store.mput(sba_tile.service_manager.service_core_ram+1,lookup)
#                    puts "#{getName(word)}: #{word}"
                    sba_tile.lookup_table.write(getName(word),word)
                # Store variable definition
                    var_value=sba_tile.data_store.mget(data_address) #t Word_List
#iv
                    puts "ASSIGN: storing",ppPayload(var_value), "@ #{var_address}"
#ev                    
                    sba_tile.data_store.mput(var_address,var_value)
                    result=word
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
                    result_list=newval
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
    #FIXME            list.push(list_length) #s/push/push_back/
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
                                elt_value_as_list.push(list[list_offset]) #s/push/push_back/
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
    
    
    #------------------------------------------------------------------------------
    def SBA_SCLib.ls_LAMBDA(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
        #core 
#iv
        print "LAMBDA CORE (#{parent.current_subtask}): \n"
#ev
        result=[] #t Word_List
        value_list=[] #t Word_List
        for address in addresses #t MemAddresses
                tval=sba_tile.data_store.mget(address)[0] #t Word
                value_list.push(tval) #s/push/push_back/
        end
        for value in value_list #t Word_List
            result.push(value) #s/push/push_back/
        end
#iv
        puts "\nLAMBDA CORE (#{parent.current_subtask}): result:",ppPayload(result)
#ev
        return result
    end # of LAMBDA
    #------------------------------------------------------------------------------

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

    def SBA_SCLib.ls_APPLY(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*; MemAddresses&)  #s/parent/parent_ptr/
            #core 
        service_word=sba_tile.service_manager.subtask_list.called_as(parent.current_subtask) #t Word  
        nargs=parent.n_args #t uint      
    	service=getName(service_word) #t Name_t  
    	#C++ bool use_redir; bool use_unique;
        if service==SC_APPLY
#iv
            print "APPLY CORE (#{parent.current_subtask}): \n"
#ev        
            use_redir=true
            use_unique=true
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
                                print "ARG: ",itemw,"\n"
                            #ev
                                lambda_function_args.push(itemw) #s/push/push_back/
                            elsif getKind(itemw)==K_R
                            #iv
                                print "REF: ",itemw,"\n" 
                            #ev
                                ext=0
                                # Here we can check
                                if parent.lookup_table.count(itemw)==0

        #iv
                                    puts "APPLY: activating code #{itemw} for #{parent.current_subtask}" 
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
                print  "APPLY CORE: #{parent.current_subtask}: CALLER ARG: (#{pp_data_kind}) #{lambda_label}=>#{lambda_def_address}=> <#{pp_data_status}>\n" 
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
                print "APPLY CORE: ","code for ",ref_symbol_word,"\n" 
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
                                    data_label=sba_tile.service_manager.symbol_table[data_address] #t Word
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
#           Prio        => 0
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
                        prio=0 #t Prio_t
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
                            sba_tile.service_manager.subtask_reference_fifo.push(ref_packet)
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



#skip

    # ----------------------------------------------------------------------------
    # WV10062008: CALL is essential! It allows to store quoted refs.
    # BUT: could we not do this simply by using LABEL? i.e.
    # (LET (ASSIGN 'f '(LAMBDA 'x '(* '6 x))) (APPLY (CALL (READ 'f)) '7))
    # (LET (LABEL 'f (LAMBDA 'x '(* '6 x))) (APPLY 'f '7))
    # Maybe we need both, what's the use of the LAMBDA call outside APPLY?
    # More importanly, could I use a LABEL as an argument? 
    def SBA_SCLib.ls_CALL(sba_system,sba_tile,parent,addresses)
        print "#{parent.service} CORE: #{parent.current_subtask}: (CALL \n"
        argaddr=addresses[0]
#        puts "ARG: #{arg}"
        result_list=[] #t Wordi_List;
        puts "ARG ADDR: #{argaddr}" #skip
        result_list=sba_tile.data_store.mget(argaddr)       
        result=result_list[0] #t Word
	puts "ARG VAL: #{result}" #skip
        parent.core_return_type=P_reference
        sba_tile.service_manager.subtask_list.to(parent.current_subtask,getName(result))
        if getKind(result)!=K_R
            raise "CALL only works on references!"
        end
#iv
        puts "#{parent.service} CORE: #{result}"
        puts "#{parent.service} CORE: )"
        puts "#{parent.service} CORE: #{parent.core_return_type} TO: #{sba_tile.service_manager.subtask_list.to(parent.current_subtask)}"
#ev
        return result_list
    end # of ls_CALL
    # ----------------------------------------------------------------------------
    # UNSYMBOL returns the Subtask field of unextended symbols or the extension words of extended symbols
    # I don't really like the name nor the idea that there is no Scheme equivalent, but it is an important service
    # Maybe RAW is a better name? or VAL? 
    def SBA_SCLib.ls_UNSYMBOL(sba_system,sba_tile,parent,addresses)
        print "#{parent.service} CORE: #{parent.current_subtask}: (UNSYMBOL \n"
        argaddr=addresses[0]
        datasymbol_list=sba_tile.data_store.mget(argaddr) #t Word_List;
	result_list=[] #t Word_List
	for datasymbol in datasymbol_list
		if getExt(datasymbol)==0
		puts "Not EXT"
			datavalue=getSubtask(datasymbol) #t Word 
			result_list.push(datavalue)
		else
		puts "EXT: #{getSubtask(datasymbol)}"
			for i in 1..getSubtask(datasymbol)
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
    
    # LOOP syntax: (loop 'task condition)
    # The task must be quoted as it's dispatched by the core
    # WV10062008: I don't think we need this, it can be done with LABEL
    def SBA_SCLib.ls_LOOP(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
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
    #------------------------------------------------------------------------------   
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
    def SBA_SCLib.ls_FIFO_NOGOOD(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
        #core
        result=[]
        print "#{parent.service} CORE: #{parent.current_subtask}: (FIFO \n"   	
        value_list=[] #t Word_List
        # get the variable name ('f1') and the task reference ('r_task')
        for address in addresses
            if SYM==0
                print "#{parent.service} CORE: #{address}\t#{sba_tile.data_store.get(address)}\n"
                value_list.push(sba_tile.data_store.get(address))                 
            else # SYM==1
                try_array=sba_tile.data_store.mget(address)
                print "#{parent.service} CORE: #{address}\t#{pretty(try_array)}\n"
                value_list.push(sba_tile.data_store.mget(address)) 
            end # SYM
        end  
        var=value_list[0] # first symbol is the variabe declaration
        task_ref=value_list[1] # second symbol is the function reference
        create_value=value_list[2] # return value of CREATE 
        
        if SYM==0
            if var.is_a?(Array)
                var_name=var.shift # the function or variable name  
                print "#{parent.service} CORE: WARNING: var_name is an Array! #{var.inspect}\n"   
            else
                var_name=var
            end                      
        else # SYM==1             
            var_name=var[0]
        end # SYM
        result=var_name #t uint64            
        
        if SYM==0
            print result,"\n"
        else # SYM==1
            print SBA_Symbol.new(result),"\n"
        end # SYM
        parent.core_return_type=P_data	            
        # Skip clean-up
        sba_tile.service_manager.subtask_list.status(parent.current_subtask,STS_cleanup)
        
        return result
    end # of FIFO
    #------------------------------------------------------------------------------    
 
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
    def SBA_SCLib.ls_FIFO(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
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
     	stream_id_word=sba_tile.data_store.mget(stream_id_address)[1] #t Word
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
    def SBA_SCLib.ls_IO(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
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
        port = port_symbol[1]  #t Word
        eof=[builtin_symbol,0] #C++ Word_List eof; eof.push_back(builtin_symbol); eof.push_back((Word)(0));      
        pass=[builtin_symbol,1] #C++ Word_List pass; pass.push_back(builtin_symbol); pass.push_back((Word)(1));
if WORDSZ==64
        fail=[builtin_symbol,Integer(2**64-1)] #C++ Word min1=(Uint64)(-1);        
else # WORDSZ==32
        fail=[builtin_symbol,4294967295] #C++ Word min1=(Uint32)(-1);
end # WORDSZ 
#C++    Word_List fail; fail.push_back(builtin_symbol); fail.push_back(min1);
#C++    FILE* fd;
        if service==A_FOPEN
            filename_address=addresses[1] #t MemAddress
            filename_bytes=sba_tile.data_store.mget(filename_address) #t Word_List
            filename=sym2str(filename_bytes) #t string
#iv
            puts "CORE FOPEN: FILENAME: #{filename}"
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
        elsif service==A_FCLOSE
            if parent.lookup_table.count(port)
            	fd=parent.lookup_table.read(port) #C++ fd=(FILE*)(parent.lookup_table.read(port));
            	fd.close() #C++ fclose(fd);
            	return pass
            else
            	return fail
            end        
        elsif service==A_IOREAD 
            nbytes_address=addresses[1] #t MemAddress
            nbytes=sba_tile.data_store.mget(nbytes_address)[1] #t Word
            if nbytes!=0 
                raise "CORE IOREAD: reading chunks of #{nbytes} bytes is not yet implemented" #s/raise/std::cerr << /
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
            puts "CORE IOREAD: <#{inp}>" #skip
            if inp!=nil
                inp_sym = string2symbol(inp) #t Word_List # must return as a built-in symbol, so need a function. Should return an empty symbol if inp is nil
                puts inp_sym.inspect #skip
                return inp_sym
            else
if WORDSZ==64
                emptyK_B = mkSymbol(K_B,T_i,1,1,0,0,0) #t Word
else # WORDSZ==32
                emptyK_B =3422552064 #C++ Word emptyK_B =0xCC000000UL;                                  
end # WORDSZ
                emptysymbol = [emptyK_B]  #C++ Word_List emptysymbol;emptysymbol.push_back(emptyK_B);
                return emptysymbol
            end
                # or maybe we need a conditional outside the function
        elsif service==A_IOWRITE
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
                        #C++ fprintf(fd,"%d",data);
                    elsif  datatype==T_f
                        data=sym2flt(data_symbol) #t Float            
                        #C++ fprintf(fd,"%f",data);
                    elsif  datatype==(T_b&1) and kind==K_Q 
                        data=sym2bool(data_symbol) #t bool           
                        #C++ fprintf(fd,"%u",data);
                    else
                        raise "CORE IOWRITE: datatype #{datatype}"
                    end                
                     fd.puts data #skip
                     return pass
                 end
             end
        elsif service==A_DISPLAY
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

        def SBA_SCLib.cs_DCT(sba_system,sba_tile,parent,addresses) #t Word_List (na;na;Base::ServiceCore*;MemAddresses&)  #s/parent/parent_ptr/
            #core
            
            #C++ Word_List result_list;
        
    # now if result is an extended Symbol, and assuming we us a single word for numbers,
    # we could just take the next element:
    
        puts "DCT (#{parent.service}) CORE: #{addresses.length} addresses" if @v #skip
        address=addresses[0] #t MemAddress
        nruns=sba_tile.data_store.mget(address)[1] #t Word
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

#---------------------------------------------------------------------------

# this is a dummy for unused services
def SBA_SCLib.none(sba_system,sba_tile,parent,addresses) #t Result (na;na;Base::ServiceCore*;MemAddresses&)
    return 0 #C++ Result res; res.push_back((Word)0); return res;
end        
        
#endif // NO_SERVICES
end # of SBA_SCLib 

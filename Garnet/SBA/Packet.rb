# SBA_Packet
#   
# :title: Garnet Service-based SoC project - SBA Packet class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: Packet.rb 2535 2009-04-29 14:00:17Z socgroup $ 

=begin #inc
#ifdef VERBOSE
#include <string>
#include <sstream>
#endif
#include "Types.h" //skipcc
#include "ServiceConfiguration.h" //skipcc
#include "Packet.h" //skiph

=end #inc

# This module contains accessor functions for the fundamental SBA types: Word, Packet, Header fields, Payload
module SBA  
    
#    def SBA.included(mod) #skip
#      puts "#{self} (Packet.rb) included in #{mod}" #skip
#    end #skip

#skipcc
=begin #types for symbols and packets
//==============================================================================
//
// General Types for Gannet -- included from C++/SBA/Packet_Types.h
//
//==============================================================================


#ifdef WV_SYSTEMC // but in the current SystemC model we don't use these
typedef sc_uint<FB_Kind> Kind_t; 
typedef sc_uint<FB_Datatype> Datatype_t; 
typedef sc_uint<FB_Ext> Ext_t; 
typedef sc_uint<FB_Quoted> Quoted_t; 
typedef sc_uint<FB_Task> Task_t; 
typedef sc_uint<FB_Subtask> Subtask_t; 
typedef sc_uint<FB_Name> Name_t; 
typedef sc_uint<FB_Count> Count_t; 

typedef sc_uint<FB_Packet_type> Packet_type_t; 
typedef sc_uint<FB_Ctrl> Ctrl_t; 
typedef sc_uint<FB_Redir> Redir_t; 
typedef sc_uint<FB_Length> Length_t; 
typedef sc_uint<FB_To> To_t; 
typedef sc_uint<FB_Return_to> Return_to_t; 

#else
typedef uint8 Kind_t; //3
typedef uint8 Datatype_t; //1
typedef uint8 Ext_t; //1
typedef uint8 Quoted_t; //1
typedef uint8 Task_t; //2
typedef uint16 Subtask_t; //16
typedef uint8 Mode_t; //2
    
typedef uint8 Packet_type_t; //3
typedef uint8 Ctrl_t; //2
typedef uint8 Redir_t; //3    

#if WORDSZ==64
typedef uint32 Name_t; //16
typedef uint32 Value_t; //16
typedef uint16 NSymbols_t; //16    
typedef uint16 Count_t; //16

typedef uint16 Length_t; //16
typedef uint16 Return_to_t; //16
typedef uint16 To_t; //16
#elif WORDSZ==32
typedef uint16 Value_t; //16
typedef uint8 NSymbols_t; //16    
typedef uint8 Name_t; //8
typedef uint8 Count_t; //0

typedef uint16 Length_t; //8
typedef uint8 Return_to_t; //8
typedef uint8 To_t; //8    
#endif // WORDSZ
#endif // WV_SYSTEMC

typedef Task_t DS_t; //2
typedef Word Symbol_t;
typedef Word Bool_t; // bool encoded as K_B Word

=end #types
#endskipcc

# ----------------------------------------------------------
# Symbol manupulation functions
# ----------------------------------------------------------
# (Kind_t;Datatype_t;Ext_t;Quoted_t;Task_t;Subtask_t;Name_t)
def mkSymbol(kind,datatype,ext,quoted,task,subtask,name) #t Word (Word;Word;Word;Word;Word;Word;Word)
	wkind=(kind << FS_Kind) & F_Kind #t Word
	wdatatype=(datatype << FS_Datatype) & F_Datatype #t Word
	wext=(ext << FS_Ext) & F_Ext #t Word
	wquoted=(quoted << FS_Quoted) & F_Quoted #t Word
	wtask=(task << FS_Task) & F_Task #t Word
	wsubtask=(subtask << FS_Subtask) & F_Subtask #t Word
	wname=(name << FS_Name) & F_Name #t Word
	word=wkind+wdatatype+wext+wquoted+wtask+wsubtask+wname #t Word
	return word
end
def getKind(word) #t Kind_t (Word)
	return (word & F_Kind) >> FS_Kind
end
def setKind(word,field) #t Word (Word;Word)
	return (word & FN_Kind) + (field << FS_Kind)
end
def getDatatype(word) #t Datatype_t (Word)
	return (word & F_Datatype) >> FS_Datatype
end
def setDatatype(word,field) #t Word (Word;Word)
	return (word & FN_Datatype) + (field << FS_Datatype)
end
def getExt(word) #t Ext_t (Word)
	return (word & F_Ext) >> FS_Ext
end
def setExt(word,field) #t Word (Word;Word)
	return (word & FN_Ext) + (field << FS_Ext)
end
def getQuoted(word) #t Quoted_t (Word)
	return (word & F_Quoted) >> FS_Quoted
end
def setQuoted(word,field) #t Word (Word;Word)
	return (word & FN_Quoted) + (field << FS_Quoted)
end
def getTask(word) #t Task_t (Word)
	return (word & F_Task) >> FS_Task
end
def setTask(word,field) #t Word (Word;Word)
	return (word & FN_Task) + (field << FS_Task)
end
def getSubtask(word) #t Subtask_t (Word)
	return (word & F_Subtask) >> FS_Subtask
end

def getDataAddress(word) #t MemAddress (Word)
    return ((word & F_Subtask) >> FS_Subtask) & FW_DataAddress    
end

def setSubtask(word,field) #t Word (Word;Word)
	return (word & FN_Subtask) + (field << FS_Subtask)
end
def getName(word) #t Name_t (Word)
	return (word & F_Name) >> FS_Name
end
def setName(word,field) #t Word (Word;Word)
	return (word & FN_Name) + (field << FS_Name)
end
def getStatus(word) #t DS_t (Word)
    return getTask(word)
end
def setStatus(word,status) #t Word (Word;DS_t)
    return setTask(word,status)
end

def getValue(word) #t Value_t (Word)
    return (word & F_Value) >> FS_Value
end
def setValue(word,field) #t Word (Word;Word)
    return (word & FN_Value) + (field << FS_Value)
end
def getNSymbols(word) #t NSymbols_t (Word)
    return (word & F_NSymbols) >> FS_NSymbols
end
def setNSymbols(word,field) #t Word (Word;Word)
    return (word & FN_NSymbols) + (field << FS_NSymbols)
end
def getNPadBytes(word) #t uint (Word)
    return (word & F_NPadBytes) >> FS_NPadBytes
end

def getInt(words) #t Int (Word_List)   
    #C++ Int int_result; Word result;    
    if getExt(words[0])==1
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
    
def getUInt(words) #t Word (Word_List)   
    #C++ Word result;    
    if getExt(words[0])==1
        result=words[1]
    else
        result=getValue(words[0]) 
    end                        
    return result
end 

def getWord(words) #t Word (Word_List)   
    #C++ Word result;    
    if getExt(words[0])==1
        result=words[1]
    else
        result=getValue(words[0]) 
    end                        
    return result
end 
# We call it float, but we will compute on doubles
def getFloat(words) #t double (Word_List)  
    #C++ double result;     
    if getExt(words[0])==1
        word=words[1] #t Word
if WORDSZ==64
    #C++        double *dbl_p=(double*)word;
    #C++        result= *dbl_p;
    result=[word].pack("Q").unpack("G")[0] #skip
else # WORDSZ==32 
    #C++        float *flt_p=(float*)word;
    #C++        float flt_result= *flt_p;
    #C++        result=(double)flt_result;
    result=[word].pack("N").unpack("g")[0] #skip 
end # WORDSZ
    else # not extended
if WORDSZ==64     
        word=getValue(words[0]) #t Word       
#C++        float *flt_p=(float*)word;
#C++        float flt_result= *flt_p;
#C++        result=(double)flt_result;
        result=[word].pack("N").unpack("g")[0] #skip    
else # WORDSZ==32
          raise "Floats must be extended for 32-bit FPU" #skip
          #C++ cerr << "Floats must be extended for 32-bit FPU\n"; exit(-1);
end # WORDSZ             
    end                    
    
    return result
    
end

def getChar(words) #t char (Word_List)
    val = getInt(words[0]) #t uint
    ch = val & 0xFF #C++ char ch=(char)(val & 0xFF);
    return ch
end

def getString(words) #t string (Word_List)
    # Strings _must_ be extended, never mind 4-byte strings!
    header = words.shift #t Word 
    nwords=getNSymbols(header) #t uint    
    padding=getNPadBytes(header) #t uint

=begin #C++
    string str;
    str.reserve(nwords*NBYTES-padding);
    for(uint i=0;i<nwords;i++) {    
        uint npad=(i==nwords-1)?padding:0;
        Word strword=words.shift();
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
    # 8 bytes, we don't do Unicode    "
    # Endianness issue!
    str=words.reverse.pack("Q"*nwords)
    str=str.reverse
else # WORDSZ==32
    # 4 bytes, idem
    str=words.pack("N"*nwords)
end # WORDSZ       
    # here we should remove any NULL bytes from the string. The problem is that we don't know how many
    # so I have to scan the string from the back
#    if padding>0
#        str=str[0..str.length-1-padding]
#    end
#endskip        
    return str
end


def getOpcode(word) #t uint (Word)
    return (getName(word)>>FS_Opcode) & FW_Opcode
end

def getSCId(word) #t uint (Word)
    return (getName(word)>>FS_SCId) & FW_SCId
end

def getSCLId(word) #t uint (Word)
    return (getName(word)>>FS_SCLId) & FW_SCLId
end

def getSNId(word) #t uint (Word)
    return (getName(word)>>FS_SNId) & FW_SNId
end      
      
#--
# CodeAddress is a 16-bit number for WORDSZ=32. We have 10 bits address space in the HW 
# We take 2 bits for the Page (from Task); the Subtask field provides 5 bits, so we can store 8-word chunks as we have 3 bits left
# But in the VM, we need to take the Service into account (because of LAMBDA/APPLY), that's 8 bits. So we need Service:Task:part(Subtask) in 16+2 bits
# because we send a Code packet to APPLY with the Return_as containing the address in the Subtask field
# unless the Subtask Address is less than 16+2 bits.
# Well, let's assume that we use 8:2:8, then we can address 4 Tasks, 256 Services, 256 subtasks per service.
#++

def getCodeAddress(word) #t CodeAddress
    # extract page form word
    page=(word & F_CodePage)>> FS_Task #t Word
    # extract address from Word
    address=(word & F_CodeAddress) >> FS_Subtask #t Word    
if VM==1
    service=(word & F_Service)>>FS_Name #t Word
    code_address=(service << FS_Service)+(page << FS_CodePage)+address #t Word    
if VERBOSE==1    
        puts "Word: #{ppSymbol(word)}"
        puts "Service: #{service} | Page: #{page} | Address: #{address} => #{code_address}"
end # VERBOSE
else # VM==0    
    # the bus in HW is 10 bits wide, 2 bits for the page, so 8 bits for the actual address. For 8-word chunks, we can use 5 bits, i.e. 32 subtasks
    code_address=(page << FS_CodePage)+address #t Word 
if VERBOSE==1   
        puts "Page: #{page} | Address: #{address} => #{code_address}"
end # VERBOSE    
end # VM     
	return code_address
end

def setCodeAddress(word,task,subtask) #t Word (Word;Word;Word) 
if VM==1
raise "FIXME! setCodeAddress is not implemented"
end # VM
	wt=(word & FN_CodePage) + (task << FS_Task) #t Word
	wst=(wt & FN_CodeAddress) + (subtask << FS_CodeAddress) #t Word
	return wst;
end

def getNArgs(word) #t uint (Word)
    return (getSubtask(word) & F_NArgs)
end

def getToken(word) #t uint (Word)
    return (getSubtask(word) >> FS_Token) & FW_Token
end

def getNCons(word) #t uint (Word)
    return (getSubtask(word) >> FS_NCons) & FW_NCons
end


def getReg(word) #t uint (Word)
    return (getSubtask(word) >> FS_Reg) & FW_Reg
end

def getMode(word) #t uint (Word)
    return ((getSubtask(word) >> FS_Mode) & FW_Mode)
end

def getOffset(word) #t uint (Word)
    return (word >> FS_Offset) & FW_Offset 
end

def getSize(word) #t uint (Word)
    return (word >> FS_Size) & FW_Size 
end

# ----------------------------------------------------------
# Header manupulation functions
# ----------------------------------------------------------
# (Packet_type_t;Ctrl_t;Redir_t;Length_t;To_t;Return_to_t;Word;Word)
def mkHeader(packet_type,prio,redir,length,to,return_to,ack_to,return_as) #t Header_t (Word;Word;Word;Word;Word;Word;Word;Word)
	wpacket_type=(packet_type << FS_Packet_type) & F_Packet_type #t Word
	wprio=(prio << FS_Ctrl) & F_Ctrl #t Word
	wredir=(redir << FS_Redir) & F_Redir #t Word
	wlength=(length << FS_Length) & F_Length #t Word
	wto=(to << FS_To) & F_To #t Word
	wreturn_to=(return_to << FS_Return_to) & F_Return_to #t Word
	w1=wpacket_type+wprio+wredir+wlength+wto+wreturn_to #t Word
	return [w1,ack_to,return_as] #skip
=begin #C++
     Header_t wl; 
     wl.push_back(w1);
     wl.push_back(ack_to);
     wl.push_back(return_as);
     return wl;
=end #C++
end
def getPacket_type(header) #t Packet_type_t (const Header_t&)
	w1=header[0] #t Word
	return (w1 & F_Packet_type) >> FS_Packet_type
end
def setPacket_type(header,field) #t Header_t (Header_t;Word)
	modheader=[]  #t Header_t #s/=..//
	w0=(header[0] & FN_Packet_type) + (field << FS_Packet_type) #t Word
	modheader.push(w0)
	modheader.push(header[1])
	modheader.push(header[2])
	return modheader
end

def getCtrl(header) #t Ctrl_t (Header_t&)
	w1=header[0] #t Word
	return (w1 & F_Ctrl) >> FS_Ctrl
end

def setCtrl(header,field) #t Header_t (Header_t&;Word)
	modheader=[]  #t Header_t #s/=..//
	w0=(header[0] & FN_Ctrl) + (field << FS_Ctrl) #t Word
	modheader.push(w0)
	modheader.push(header[1])
	modheader.push(header[2])
	return modheader
end
def getRedir(header) #t Redir_t (Header_t&)
	w1=header[0] #t Word
	return (w1 & F_Redir) >> FS_Redir
end
def setRedir(header,field) #t Header_t (Header_t&;Word)
	modheader=[]  #t Header_t #s/=..//
	w0=(header[0] & FN_Redir) + (field << FS_Redir) #t Word
	modheader.push(w0)
	modheader.push(header[1])
	modheader.push(header[2])
	return modheader
end
def getLength(header) #t Length_t (const Header_t&)
	w1=header[0] #t Word
	return (w1 & F_Length) >> FS_Length
end

def setLength(header,field) #t Header_t (Header_t&;Word)
	modheader=[]  #t Header_t #s/=..//
	w0=(header[0] & FN_Length) + (field << FS_Length) #t Word
	modheader.push(w0)
	modheader.push(header[1])
	modheader.push(header[2])
	return modheader
end
def getTo(header) #t To_t (const Header_t&)
	w1=header[0] #t Word
	return (w1 & F_To) >> FS_To
end
def setTo(header,field) #t Header_t (Header_t&;Word)
	modheader=[]  #t Header_t #s/=..//
	w0=(header[0] & FN_To) + (field << FS_To) #t Word
	modheader.push(w0)
	modheader.push(header[1])
	modheader.push(header[2])
	return modheader
end
def getReturn_to(header) #t Return_to_t (const Header_t&)
	w1=header[0] #t Word
	return (w1 & F_Return_to) >> FS_Return_to
end
def setReturn_to(header,field) #t Header_t (Header_t&;Word)
	modheader=[]  #t Header_t #s/=..//
	w0=(header[0] & FN_Return_to) + (field << FS_Return_to) #t Word
	modheader.push(w0)
	modheader.push(header[1])
	modheader.push(header[2])
	return modheader
end

	def getAck_to(header) #t Word (const Header_t&)
        return header[1]
    end
#skipcc   
=begin #h
	Word getAck_to_p(const Packet_t& packet);
    Word getReturn_as_p(const Packet_t& packet) ;
	Return_to_t getReturn_to_p(const Packet_t& packet);    
    Redir_t getRedir_p(const Packet_t& packet) ;
    Ctrl_t getCtrl_p(const Packet_t& packet) ;
    Packet_type_t getPacket_type_p(const Packet_t& packet);
    Length_t getLength_p(const Packet_t& packet);
=end #h
#endskipcc
#skiph    
=begin #cc
	Word SBA::getAck_to_p(const Packet_t& packet) {
        return packet[1]; 
    }
	Word SBA::getReturn_as_p(const Packet_t& packet) {
        return packet[2];
    }
	Return_to_t SBA::getReturn_to_p(const Packet_t& packet) {
        Word w1 = packet[0];
        return (w1 & F_Return_to) >> FS_Return_to;
    }    
    
    Redir_t SBA::getRedir_p(const Packet_t& packet) {
        Word w1=packet[0];
    	return (w1 & F_Redir) >> FS_Redir;
    }   
    Ctrl_t SBA::getCtrl_p(const Packet_t& packet) {
        Word w1=packet[0];
        return (w1 & F_Ctrl) >> FS_Ctrl;
    }   
    
    Packet_type_t SBA::getPacket_type_p(const Packet_t& packet) {
    	Word w1=packet[0];
	    return (w1 & F_Packet_type) >> FS_Packet_type;
	}
    Length_t SBA::getLength_p(const Packet_t& packet) {
    	Word w1=packet[0];
	    return (w1 & F_Length) >> FS_Length;
	}    
=end #cc
#endskiph

	def getReturn_as(header) #t Word (const Header_t&)
        return header[2]
    end

    def setReturn_as(header,field) #t Header_t (Header_t&;Word)
         modheader=[]  #t Header_t #s/=..//
         modheader.push(header[0]) 
         modheader.push(header[1])
         modheader.push(field)
         return modheader
    end

    def setAck_to(header,field) #t Header_t (Header_t&;Word)
         modheader=[]  #t Header_t #s/=..//
         modheader.push(header[0]) 
         modheader.push(field)
         modheader.push(header[2])
         return modheader
    end

	def getType(header) #t Packet_type_t (const Header_t&) 
		return getPacket_type(header)
	end

	def setType(header,field) #t Header_t (Header_t&;Packet_type_t&)
		return setPacket_type(header,field)
	end

#skip
	def getAck_to_p(packet) 
        return packet[1]
	end
	
    def getReturn_as_p( packet) 
        return packet[2]
    end
	def getReturn_to_p(packet) 
		w1=packet[0] #t Word
    	return (w1 & F_Return_to) >> FS_Return_to	
	end
    def getRedir_p(packet) 
    	w1=packet[0] #t Word
	   return (w1 & F_Redir) >> FS_Redir    
    end
    def getCtrl_p(packet)  
        w1=packet[0] 
        return (w1 & F_Ctrl) >> FS_Ctrl    
    end    
    def getPacket_type_p(packet) 
    	w1=packet[0] #t Word
	   return (w1 & F_Packet_type) >> FS_Packet_type    
    end
    def getLength_p( packet)
    	w1=packet[0] 
	   return (w1 & F_Length) >> FS_Length  
    end    
#endskip

# ----------------------------------------------------------
# Packet manupulation functions
# ----------------------------------------------------------

    def mkPacket(header,payload) #t Packet_t (Header_t&;Word_List&)
        packet=[] #C++ Word_List packet;
        for i in 0..HEADER_SZ-1 #t uint
            packet.push(header[i])
        end
        for w in payload #t Word_List
            packet.push(w)
        end
        return packet
    end

    def getHeader(packet) #t Header_t (Packet_t&)
    	header=[] #C++ Header_t header;
    	header.push(packet[0]) 
    	header.push(packet[1]) 
    	header.push(packet[2]) 
        return header 
    end    

    def setHeader(packet,header) #t Word_List (Packet_t&;Header_t&)
        npacket=[] #C++ Word_List npacket;
        for w in header #t Word_List
            npacket.push(w)
        end
        payload=getPayload(packet) #t Word_List
        for w in payload #t Word_List
            npacket.push(w)
        end
        return npacket
    end
        
    def getPayload(packet) #t Word_List (Word_List)
        pl=[] #C++ Word_List pl;
        i=0 #t uint
        for w in packet #t Word_List
            pl.push(w) unless i<3 #C++ if (i>=3) {pl.push_back(w);}
            i+=1
        end
        return pl
    end   
    
    def getField(result,offset,size) #t Word_List (Word_List;uint;uint)
        field=[] #C++ Word_List field;
        i=0 #t uint
        if size==0
            if offset==0
                return result
            else                                    
                size=result.length()
            end
        end
        for w in result #t Word_List
            field.push(w) unless i<offset #C++ if (i>=offset) {field.push_back(w);}
            i+=1
            if i>offset+size
                break
            end            
        end
        return field        
    end
    
def mkBool(bval) #t Word (uint)
    boolsym=mkSymbol(K_B,T_i,0,1,0,0,bval)
    return boolsym        
end    
             
#skip

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

#endskip

    def to_signed_int_list(wl) #t deque<Int>  (Word_List)
        numlist=[] #C++   deque<Int> numlist;
        for w in wl #t Word_List
        #C++ Int sw=(Int)w;
#skip
            if w>2**63 
                sw=w-2**64
            else 
                sw=w
            end
#endskip
            numlist.push(sw)
        end
        return numlist
    end     

            
#iv    

# ----------------------------------------------------------
# Pretty-print functions
# ----------------------------------------------------------

    def ppSymbol(w) #t string (Word)
        os="" #C++ ostringstream outs;
        if (w.is_a?(String) )  #skip
        os=w #skip
        else #skip
#C++	outs << (uint)getKind(w) <<":"<< (uint)getDatatype(w) <<":"<< (uint)getExt(w) <<":"<<(uint)getQuoted(w)<<":"<< (uint)getTask(w) <<":"<< (uint)getSubtask(w) <<":"<< (uint)getName(w);
#C++	string os=outs.str();	
#        if VERBOSE==1
#        os="#{kind_l(getKind(w))}:#{getDatatype(w)}:#{getExt(w)}:#{getQuoted(w)}:#{getTask(w)}:#{getSubtask(w)}:#{getName(w)}" #skip
#        else # VERBOSE==0
        os="#{getKind(w)}:#{getDatatype(w)}:#{getExt(w)}:#{getQuoted(w)}:#{getTask(w)}:#{getSubtask(w)}:#{getName(w)}" #skip
#        end # VERBOSE
        end #skip
		return os
    end
    
    def ppHeader(wl) #t string (Header_t)
        w2=wl[1] #t Word
        w3=wl[2] #t Word

#C++	ostringstream outs;
#C++	outs << (uint)getPacket_type(wl) << ":" << (uint)getCtrl(wl) <<":"<< (uint)getRedir(wl) <<":"<<(uint)getLength(wl)<<":"<< (uint)getTo(wl) <<":"<<(uint)getReturn_to(wl) <<"\n"<< (uint)w2<<"\n"<<(uint)w3;
#        if VERBOSE==1
#        os="#{packettype_l(getPacket_type(wl))}:#{getCtrl(wl)}:#{getRedir(wl)}:#{getLength(wl)}:#{num2name(getTo(wl)).downcase}:#{num2name(getReturn_to(wl)).downcase}\n#{w2}\n#{ppSymbol(w3)} (#{w3})" #skip
#        else # VERBOSE==0
        os="#{getPacket_type(wl)}:#{getCtrl(wl)}:#{getRedir(wl)}:#{getLength(wl)}:#{getTo(wl)}:#{getReturn_to(wl)}\n#{ppSymbol(w2)}\n#{ppSymbol(w3)}" #skip
#        end # VERBOSE
        #C++	string os=outs.str();
		return os
    end
    
    def ppPayload(wl) #t string (Word_List)
            os="" #C++ ostringstream outs;
            maxnwords=4 #t uint
            if wl.length()<maxnwords
                maxnwords=wl.length()
            end   
            if wl.length()>0                
                for i in 0..maxnwords-1 #t uint
#C++	        outs << ppSymbol(wl[i]) <<"\n";
                    if wl[i].kind_of?( Integer) #skip	
                    os+="#{ppSymbol(wl[i])}\n" #skip
                    else #skip
                        os+="#{ppSymbol(NIHIL)}\n"
                    end #skip
                end
                rest=wl.length()-maxnwords #t uint
                if rest!=0
                os+="... and #{rest} more words\n" #s/os../outs<</
                end
#C++	        string os=outs.str();                                          
                return os
            else
            os="" #t string                             
            return os            
            end
		
    end
    
    def ppPacket(wl) #t string (Word_List)
#        puts "--------",caller[0].sub!(/^.*\//,''),"--------" #skip
#C++	ostringstream outs;
        
#C++	outs << ppHeader(getHeader(wl)) <<"\n------------\n"<< ppPayload(getPayload(wl))<<"\n";	
        os="#{ppHeader(getHeader(wl))}\n------------\n#{ppPayload(getPayload(wl))}\n" #C++	string os=outs.str();
		return os    
    end
#ev    
end #skipcc
#H } // Namespace SBA
#skip
# Objects are better than primitives
class SBA_Packet_Fifo
  attr_reader :packets  
  attr_reader :status
	def initialize()
		@packets=[]
		@status=0 
  end
  
#  def has_packets()
#    return @status
#  end
#
#	def length
#	   return @packets.length
#	end
#	def push(val)
#    	if @status==0
#	   end
#	   @status=1
#	   @packets.push(val)
#	end
#	def shift()
#	   val=@packets.shift()
#	   if @packets.length==0
#	       @status=0
#	   else 
#	       @status=1
#	   end
#	   return val
#	end
#	def clear()
#	   @packets=[]
#	end   
  def has_packets()
    return @status
  end
	def length
	   return @packets.length
	end
  
	def push(val)
	   @packets.push(val)
     if @status==0    
        @status=1
     end     
	end
  
	def shift()
   # We don't really want to block on shift()
# So I have to check when the fifo has 1 elt, then 
# set status to 0 and shift that elt. As we check status we won't come back there
# then we use the blocking call simply to set the status back to 1  
#	   val=@packets.deq()
#	   if @packets.length==0
#	       @fifo_status=0
#	   else 
#	       @fifo_status=1
#	   end
    
      if @packets.length==1
        @status=0      
      end
      val=@packets.shift()  
    
	   return val
	end
	def clear()
	   @packets.clear
	end 
end # of SBA_Packet_Fifo

class SBA_TX_Packet_Fifo < SBA_Packet_Fifo
end # of SBA_TX_Packet_Fifo

if USE_THREADS==1
require 'thread' #skip
end
class SBA_RX_Packet_Fifo
  attr_reader :packets  
  attr_reader :status
if USE_THREADS==0
  def initialize(parent)
   @p=parent 
    @packets=[]
    @status=0 
  end
  
  def has_packets()
    return (@status==1)
  end

  def length
     return @packets.length
  end
  def push(val)
      if @status==0
     end
     @status=1
     @packets.push(val)
  end
  def shift()
     val=@packets.shift()
     if @packets.length==0
         @status=0
     else 
         @status=1
     end
     return val
  end
  def clear()
     @packets=[]
  end   
else # USE_THREADS==1
  def initialize(parent)
      @p=parent
    @packets=Queue.new
    @status=0 
  end
  def has_packets()
    # we want to block until the status is true
    # so status() will block until it can return true
    # The Queue is a bit limited, it only blocks on deq
    # if empty      
#    puts "#{@p.service}: SBA_RX_Packet_Fifo: BLOCKING"      
    @packets.enq(@packets.deq)
#    puts "#{@p.service}: SBA_RX_Packet_Fifo: UNBLOCKING #{@p.service}"  
    @status=1
    return true # @status
  end
  def length
     return @packets.length
  end
  
  def push(val)
#      puts "#{@p.service}: SBA_RX_Packet_Fifo: service #{@p.service} GOT PACKET!"
#      puts "#{@p.service}: SBA_RX_Packet_Fifo: number of packets in Queue: #{@packets.length}"
#      puts val.inspect
      @packets.enq(val)
#      puts "#{@p.service}: SBA_RX_Packet_Fifo: service #{@p.service} ENQ'd PACKET!"
      # But somehow this does not result in has_packets() unblocking!
     if @status==0    
        @status=1
     end
#     puts "#{@p.service}: SBA_RX_Packet_Fifo: number of packets in Queue: #{@packets.length}"     
  end
  
  def shift()
#      puts "#{@p.service}: SBA_RX_Packet_Fifo: service #{@p.service} SHIFT PACKET!"
#      puts "#{@p.service}: SBA_RX_Packet_Fifo: number of packets in Queue: #{@packets.length}"

      # We don't really want to block on shift()
# So I have to check when the fifo has 1 elt, then 
# set status to 0 and shift that elt. As we check status we won't come back there
# then we use the blocking call simply to set the status back to 1  
#    val=@packets.deq()
#    if @packets.length==0
#        @fifo_status=0
#    else 
#        @fifo_status=1
#    end
    
      if @packets.length==1
        @status=0      
      end
      val=@packets.deq()  
#      puts "#{@p.service}: SBA_RX_Packet_Fifo: number of packets in Queue: #{@packets.length}"
     return val
  end
  def clear()
     @packets.clear
  end 
end # USE_THREADS
end # of SBA_RX_Packet_Fifo

#endskip


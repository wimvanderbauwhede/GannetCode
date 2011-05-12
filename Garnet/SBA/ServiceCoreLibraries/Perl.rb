# Perl.rb
#   
# :title: Gannet Service-based SoC project - Service Core Library for Perl 5
#    
#--
# *
# *  (c) 2004-2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#++

# -----------------------------------------------------------------------------

=begin #inc

#include <fstream>
#include <sstream>
#include "Types.h" //skipcc
#include "Packet.h" //skipcc
#include "Base/ServiceCore.h" //skipcc
#include "ServiceCoreLibrary.h" //skiph
#include "System.h" //skiph
#include "Tile.h" //skiph
#include "ServiceCore.h" //skiph

=end #inc 

# This module contains the implementations of all Gannet-Perl services. 
require "SBA/ServiceCoreLibraries/SBAnew.rb"
class Perl < SBAnew

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
=begin
Keep Vim happy
=end

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

def pl_ALU(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
    #core
#iv        
    puts "ALU CORE: processing subtask #{parent.current_subtask}"
#ev
    #C++ Word_List result_list;
    operation=parent.opcode #t Uint
    addresses=paretn.addresses() #t MemAddresses
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
                result=div(result,tres) #C++ int_result=div(int_result,int_tres);
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
    parent.result( result_list )
end # of ALU
# ----------------------------------------------------------------------------------------------------
 
# Perl FPU -- Ruby only
        
def pl_FPU(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
#core

#iv        
puts "FP ALU CORE: processing subtask #{parent.current_subtask}"
#ev
                
#C++ Word_List result_list;

return_as=sba_tile.service_manager.subtask_list.return_as(parent.current_subtask) #t Word
operation=parent.opcode #t Uint
    addresses=paretn.addresses() #t MemAddresses

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

    parent.result( result_list )

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
    
def pl_Array(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
#core
    #iv        
    puts "Perl Array CORE: processing subtask #{parent.current_subtask}"
    #ev
    addresses=parent.addresses() #t MemAddresses& 
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
    parent.result( result_list )
end # of pl_Array

def pl_Hash(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
#core
    #iv        
    puts "Perl Hash CORE: processing subtask #{parent.current_subtask}"
    #ev
    addresses=parent.addresses() #t MemAddresses&                
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
    parent.result( result_list)
end # of pl_Hash

def pl_PCRE(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
end

def pl_Math(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
end

def pl_Range(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
end

def pl_String(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
end

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------     
#         
# this is a dummy for unused services
def none(sba_tile,parent) #t void (na;Base::ServiceCore*) #s/parent/parent_ptr/
    return 0 #C++ Result res; res.push_back((Word)0); return res;
end        
        
#endif // NO_SERVICES
end # of SBA_SCLib 

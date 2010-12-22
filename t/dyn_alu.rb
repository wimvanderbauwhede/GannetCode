    def SBA_SCLib.ds_ALU(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
        #core

#iv        
        puts "ALU CORE: processing subtask #{parent.current_subtask}"
#ev
                        
        #C++ Word_List result_list;
        opcode_address=addresses.shift #C++ MemAddress opcode_address=addresses.front();addresses.pop_front();
        operation=sba_tile.data_store.mget(opcode_address)[1] #t Uint

# now if result is an extended Symbol, and assuming we us a single word for numbers,
# we could just take the next element:

#iv        
    puts "DYN ALU (#{parent.service}) CORE: #{addresses.length} addresses"
#ev     
    if addresses.length==0 #skip
      exit(0) #skip
    end  #skip

	address=addresses[0] #t MemAddress
 	result=sba_tile.data_store.mget(address)[1] #t Word
 	if not result.is_a?(Integer) #skip
 	    puts "NIL: #{result.class}" #skip
 	    exit(0) #skip
 	end #skip
    res_symbol=sba_tile.data_store.mget(address)[0] #t Word
    
#C++ result_list.push_back(res_symbol);

# But only if extended quoted symbols are handled correctly 

if WORDSZ==64
                int_result=result>2**63?result-2**64:result #C++ Int int_result=(Int)result;
else # WORDSZ==32
                        int_result=(result>2**31)?result-2**32:result #C++ Int int_result=(Int)result;
end # WORDSZ                
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
                    tres=sba_tile.data_store.mget(address)[1] #t Word

                        int_tres=(tres>2**(WORDSZ-1))?(tres-2**WORDSZ):tres #C++ Int int_tres=(Int)tres;
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
 
        result_list=[res_symbol,result]  #C++ result_list.push_back(result);
        return result_list
    end # of ALU

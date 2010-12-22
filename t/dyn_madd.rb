# matrix addition for RGB or YCbCr image blocks

def SBA_SCLib.ds_MADD(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
  puts "DYN CORE MADD" #skip
    result_list=[] #C++ Word_List result_list;
    addr1=addresses[0] #t MemAddress
    addr2=addresses[1] #t MemAddress
    matrix1=sba_tile.data_store.mget(addr1) #t Word_List
    matrix2=sba_tile.data_store.mget(addr2) #t Word_List
  
    for i in 0..matrix1.length-1 #t uint
        elt1= matrix1[i] #t uint
        elt11=elt1&255 #t uint
        elt12=(elt1>>8)&255 #t uint
        elt13=(elt1>>16)&255 #t uint
        elt2=matrix2[i] #t uint
        elt21=elt2&255 #t uint
        elt22=(elt2>>8)&255 #t uint
        elt23=(elt2>>16)&255 #t uint        
        sum1=elt11+elt21 #t uint
        sum2=elt12+elt22 #t uint
        sum3=elt13+elt23 #t uint
        sum1=(sum1>255) ?  255 : sum1
        sum2=(sum2>255) ?  255 : sum2
        sum3=(sum3>255) ?  255 : sum3
        sum =  sum1+(sum2 << 8)+(sum3 << 16)
        result_list.push(sum)  #C++ result_list.push_back(sum);
#        puts "DYN CORE MADD returns #{sum}" #skip                    
    end

    return result_list
end # of ds_MADD 

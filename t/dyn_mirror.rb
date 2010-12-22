# Mirroring an image block

def SBA_SCLib.ds_MIRROR(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
  puts "DYN CORE MIRROR" #skip
    result_list=[] #C++ Word_List result_list;
    addr1=addresses[0] #t MemAddress
    addr2=addresses[1] #t MemAddress
    matrix1=sba_tile.data_store.mget(addr1) #t Word_List
    matrix2=sba_tile.data_store.mget(addr2) #t Word_List
  
    for i in 0..matrix1.length-1 #t uint
    # this if of course wrong
        mir_i=matrix1.length-1-i 
        elt1= matrix1[mir_i] #t uint       
        result_list.push(elt1)  #C++ result_list.push_back(elt1);
#        puts "DYN CORE MIRROR returns #{elt1}" #skip                    
    end

    return result_list
end # of ds_MIRROR 

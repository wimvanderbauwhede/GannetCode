# Dynamic service core for Sobel edge detection

# the matrix stores the Y, Cb and Cr (or R,G,B) components in the 3 LSBs
def SBA_SCLib.ds_SOBEL_HOR(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
    puts "DYN CORE SOBEL HOR" #skip
    m_G=[[1,2,1],[0,0,0],[-1,-2,-1]] #C++ int m_G[3][3];
    #C++ m_G[0]={1,2,1};
    #C++ m_G[0]={0,0,0};
    #C++ m_G[0]={-1,-2,-1};
    
    result_list=[] #C++ Word_List result_list;
    addr1=addresses[0] #t MemAddress
    addr2=addresses[1] #t MemAddress
    blocksz=getValue(sba_tile.data_store.mget(addr2)[0]) #t uint
    matrix1=sba_tile.data_store.mget(addr1) #t Word_List
  
    for i in 0..matrix1.length-1 #t uint
        c1=0 #t uint
        c2=0 #t uint
        c3=0 #t uint
        w=matrix1[i]
        wy=w&255
        wcb=(w>>8)&255
        wcr=(w>>16)&255
        for j in 0..2 #t uint
            for k in 0..2 #t uint
            ii=i+(k-1)+(j-1)*blocksz #t uint
            elt1=matrix1[i] #t uint
            if (ii>0 and ii<blocksz*blocksz)
                elt1= matrix1[ii] #t uint
            end
            elt11=(elt1&255) #t uint
            elt12=((elt1>>8)&255) #t uint
            elt13=((elt1>>16)&255) #t uint
            c1 = c1+m_G[j][k]*elt11
            c2 = c2+m_G[j][k]*elt12
            c3 = c3+m_G[j][k]*elt13
            end
        end
        c1t=(c1<0) ? (-c1) : c1 #t uint
        c2t=(c2<0) ? (-c2) : c2 #t uint
        c3t=(c3<0) ? (-c3) : c3 #t uint
        c1b=(c1t>255) ? 255 : c1t
        c2b=(c2t>255) ? 255 : c2t
        c3b=(c3t>255) ? 255 : c3t
        puts "SOBEL HOR: #{c1b},#{c2b},#{c3b}" #skip
        w =  (c1b&255)+((c2b&255)<< 8)+((c3b&255)<< 16) #t uint
        result_list.push(w)  #C++ result_list.push_back(w);
#        puts "DYN CORE SOBEL HOR returns #{c1},#{c2,#{c3}" #skip                    
    end
    return result_list
end # of ds_SOBEL_HOR 

def SBA_SCLib.ds_SOBEL_VERT(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
    puts "DYN CORE SOBEL VERT" #skip
    m_G=[[1,0,-1],[2,0,-2],[1,0,-1]] #C++ int m_G[3][3];
    #C++ m_G[0]={1,0,-1};
    #C++ m_G[0]={2,0,-2};
    #C++ m_G[0]={1,0,-1};

    result_list=[] #C++ Word_List result_list;
    addr1=addresses[0] #t MemAddress
    addr2=addresses[1] #t MemAddress
    blocksz=getValue(sba_tile.data_store.mget(addr2)[0]) #t uint
    matrix1=sba_tile.data_store.mget(addr1) #t Word_List
  
    for i in 0..matrix1.length-1 #t uint
        c1=0 #t uint
        c2=0 #t uint
        c3=0 #t uint
        w=matrix1[i]
        wy=w&255
        wcb=(w>>8)&255
        wcr=(w>>16)&255
        for j in 0..2 #t uint
            for k in 0..2 #t uint
            ii=i+(k-1)+(j-1)*blocksz #t uint
            elt1=matrix1[i] #t uint
            if (ii>0 and ii<blocksz*blocksz)
                elt1= matrix1[ii] #t uint
            end
            elt11=(elt1&255) #t uint
            elt12=((elt1>>8)&255) #t uint
            elt13=((elt1>>16)&255) #t uint
            c1 = c1+m_G[j][k]*elt11
            c2 = c2+m_G[j][k]*elt12
            c3 = c3+m_G[j][k]*elt13
            end
        end
        c1t=(c1<0) ? (-c1) : c1 #t uint
        c2t=(c2<0) ? (-c2) : c2 #t uint
        c3t=(c3<0) ? (-c3) : c3 #t uint
        c1b=(c1t>255) ? 255 : c1t
        c2b=(c2t>255) ? 255 : c2t
        c3b=(c3t>255) ? 255 : c3t
        puts "SOBEL HOR: #{c1b},#{c2b},#{c3b}" #skip
        w =  (c1b&255)+((c2b&255)<< 8)+((c3b&255)<< 16) #t uint
        result_list.push(w)  #C++ result_list.push_back(w);
#        puts "DYN CORE SOBEL VERT returns #{c1},#{c2,#{c3}" #skip                    
    end
    return result_list
end # of ds_SOBEL_VERT

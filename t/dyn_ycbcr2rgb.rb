# Dynamic service core for YCbCr to RGB conversion
# Based on code from IJG jpeg-7 library

# the matrix stores the Y, Cb and Cr (or R,G,B) components in the 3 LSBs
def SBA_SCLib.ds_YCbCr2RGB(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
  puts "DYN CORE YCbCr2RGB" #skip
    result_list=[] #C++ Word_List result_list;
    addr1=addresses[0] #t MemAddress
    matrix1=sba_tile.data_store.mget(addr1) #t Word_List    
      
    c_MAXJSAMPLE=255 
    c_CENTERJSAMPLE=128
    c_SCALEBITS	= 16	
    c_ONE_HALF	=  1 << (c_SCALEBITS-1) 
    
    cr_r_tab=[]
    cb_b_tab=[]
    cr_g_tab=[]
    cb_g_tab=[]
    
     x = -c_CENTERJSAMPLE
  for i in 0.. c_MAXJSAMPLE+1
  x=x+1
    # i is the actual input pixel value, in the range 0..MAXJSAMPLE */
    # The Cb or Cr value we are thinking of is x = i - CENTERJSAMPLE */
    # Cr=>R value is nearest int to 1.40200 * x */
    cr_r_tab[i] = (fixp(1.40200,c_SCALEBITS) * x + c_ONE_HALF)>> c_SCALEBITS
    # Cb=>B value is nearest int to 1.77200 * x */
    cb_b_tab[i] = (fixp(1.77200,c_SCALEBITS) * x + c_ONE_HALF)>> c_SCALEBITS
    # Cr=>G value is scaled-up -0.71414 * x */
    cr_g_tab[i] = -fixp(0.71414,c_SCALEBITS) * x
    # Cb=>G value is scaled-up -0.34414 * x */
    # We also add in ONE_HALF so that need not do it in inner loop */
    cb_g_tab[i] = -fixp(0.34414,c_SCALEBITS) * x + c_ONE_HALF
  end    
    

    
    for i in 0..matrix1.length-1 #t uint
        elt1= matrix1[i] #t uint
        y=elt1&255 #t uint
        cb=(elt1>>8)&255 #t uint
        cr=(elt1>>16)&255 #t uint        
      r =   range_limit( y + cr_r_tab[cr] )
      g =  range_limit( y + ((cb_g_tab[cb] + cr_g_tab[cr])>>c_SCALEBITS) )
      b =  range_limit( y + cb_b_tab[cb] )

        w =  (r&255)+((g&255)<< 8)+((b&255)<< 16)
        result_list.push(w)  #C++ result_list.push_back(w);
#        puts "DYN YCbCr2RGB returns #{r},#{g},#{b}" #skip                    
    end

    return result_list
end # of ds_YCbCr2RGB 

def SBA_SCLib.fixp(x,scalebits)	
    (x * (1<< scalebits) + 0.5).to_int
end    

def SBA_SCLib.range_limit (x)
    if x<0 
        return 0
    elsif x>255
        return 255
    else
        return x.to_int
    end
end


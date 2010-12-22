# Dynamic service core for RGB-to YCbCr conversion
# Based on code from IJG jpeg-7 library

# the matrix stores the Y, Cb and Cr (or R,G,B) components in the 3 LSBs
def SBA_SCLib.ds_RGB2YCbCr(sba_tile,parent,addresses) #t Word_List (na;Base::ServiceCore*;MemAddresses&) #s/parent/parent_ptr/
    #core
  puts "DYN CORE RGB2YCbCr" #skip
    result_list=[] #C++ Word_List result_list;
    addr1=addresses[0] #t MemAddress
    matrix1=sba_tile.data_store.mget(addr1) #t Word_List      
    
    c_MAXJSAMPLE=255 
    c_CENTERJSAMPLE=128
    c_SCALEBITS	= 16	
    c_CBCR_OFFSET	= c_CENTERJSAMPLE << c_SCALEBITS
    c_ONE_HALF	=  1 << (c_SCALEBITS-1)
    
    c_R_Y_OFF	=	0			# offset to R => Y section */
    c_G_Y_OFF	=	(1*(c_MAXJSAMPLE+1))	# offset to G => Y section */
    c_B_Y_OFF	=	(2*(c_MAXJSAMPLE+1))	#etc. */
    c_R_CB_OFF	=(3*(c_MAXJSAMPLE+1))
    c_G_CB_OFF	=(4*(c_MAXJSAMPLE+1))
    c_B_CB_OFF	=(5*(c_MAXJSAMPLE+1))
    c_R_CR_OFF	=c_B_CB_OFF		# B=>Cb, R=>Cr are the same */
    c_G_CR_OFF	=(6*(c_MAXJSAMPLE+1))
    c_B_CR_OFF	=(7*(c_MAXJSAMPLE+1))
    c_TABLE_SIZE=	(8*(c_MAXJSAMPLE+1))
    # Note that this table is actually static, to be computed only once
    # For MORA we could either use 8 bits and be inaccurate but fast
    # or 16 bits and be accurate but seriously complicated 
    ctab=[]
    for i in 0 .. c_MAXJSAMPLE+1
        ctab[i+c_R_Y_OFF] = fixp(0.29900,c_SCALEBITS) * i
        ctab[i+c_G_Y_OFF] = fixp(0.58700,c_SCALEBITS) * i;
        ctab[i+c_B_Y_OFF] = fixp(0.11400,c_SCALEBITS) * i + c_ONE_HALF
        ctab[i+c_R_CB_OFF] = -fixp(0.16874,c_SCALEBITS) * i
        ctab[i+c_G_CB_OFF] = -fixp(0.33126,c_SCALEBITS) * i
        
        # We use a rounding fudge-factor of 0.5-epsilon for Cb and Cr.
        # This ensures that the maximum output will round to MAXJSAMPLE
        # not MAXJSAMPLE+1, and thus that we don't have to range-limit.
        ctab[i+c_B_CB_OFF] = fixp(0.50000,c_SCALEBITS) * i + c_CBCR_OFFSET + c_ONE_HALF-1
    #  B=>Cb and R=>Cr tables are the same
    #    ctab[i+c_R_CR_OFF] = fixp(0.50000,c_SCALEBITS) * i    + c_CBCR_OFFSET + c_ONE_HALF-1;
        ctab[i+c_G_CR_OFF] = -fixp(0.41869,c_SCALEBITS) * i
        ctab[i+c_B_CR_OFF] = -fixp(0.08131,c_SCALEBITS) * i
    end
# So all we do is look up the coefficients and add them. In low-accuracy MORA
# the coeff table fits in the B memory so all we need is two add instructions
# Not quite: we need to look up the coefficients based on the value of the pixel in A memory
# and unfortunately we need the R,G and B component! We have just enough memory for this
# So it could still be done in 2 adds, using the acc register
# but we need a loop around those adds, so another 3 instructions, i.e. 5. I'm assuming we don't need a mem load but I'm not sure...    

    for i in 0..matrix1.length-1 #t uint
        elt1= matrix1[i] #t uint
        r=elt1&255 #t uint
        g=(elt1>>8)&255 #t uint
        b=(elt1>>16)&255 #t uint        
      # Y 
      yr = ctab[r+c_R_Y_OFF] 
      yg = ctab[g+c_G_Y_OFF] 
      yb = ctab[b+c_B_Y_OFF]
# puts yr,yg,yb, b,c_B_Y_OFF
      y = ((yr + yg + yb) >> c_SCALEBITS).to_int
      # Cb
      cb = ((ctab[r+c_R_CB_OFF] + ctab[g+c_G_CB_OFF] + ctab[b+c_B_CB_OFF]) >> c_SCALEBITS).to_int
      # Cr
      cr = ((ctab[r+c_R_CR_OFF] + ctab[g+c_G_CR_OFF] + ctab[b+c_B_CR_OFF]) >> c_SCALEBITS).to_int

        w =  (y&255)+((cb&255)<< 8)+((cr&255)<< 16)
        result_list.push(w)  #C++ result_list.push_back(w);
#        puts "DYN CORE RGB2YCbCr returns #{y},#{cb},#{cr}" #skip                    
    end
#        puts "DYN CORE RGB2YCbCr RESULT size #{result_list.size}"

    return result_list
end # of ds_RGB2YCbCr 

def SBA_SCLib.fixp(x,scalebits)	
    (x * (1<< scalebits) + 0.5).to_int
end    



class SBA_Symbol
    include SBA_Bytecode #skip
    def initialize(kind_or_num,*more_args) 
        if more_args.length>=6
            kind=kind_or_num
            fields=[kind]+more_args
            from_fields(fields)
        elsif more_args.length==0
            @num=kind_or_num
            if @num>2**32 and WORDSZ==32
                #must be a symbol as the compiler does not give us big ints
                raise "Word too big for 32 bits"
            end
        end
    end
    def copy #t SBA_Symbol
        s=SBA_Symbol.new(@num)
        return s
    end
    
    def to_num #t uint64
        return @num
    end
    def to_wordlist #t Word_List
        return [@num]
    end
    
    def to_fields #t Word_List
        bw=byteword(@num) #t Word_List
        raise "IS THIS EVEr USED?"
        puts "NUM: #{@num} BW: #{bw}" 
        if WORDSZ==64
            kte=bw[0] #t uint64
            kind=kte>>4 #t uint64
            type=(kte-(kind<<4))>>1 #t uint64
            ext=(kte-(kind<<4)-(type<<1)) #t uint64
            quoted=(bw[1]>>6) & 3 #t uint64
            task=bw[1] & 63 #t uint64
            subtask=256*bw[2]+bw[3] #t uint64
            name=256*bw[4]+bw[5] #t uint64
            count=256*bw[6]+bw[7] #t uint64
        else # WORDSZ=32
            if bw.length!=4
                raise "Wrong bw length:#{bw.length}"
            else 
                puts bw.inspect
            end
            
            kteqt=bw[0]
            kind=(kteqt & 0xe0)>>5
            type=(kteqt & 0x10)>>4
            ext=(kteqt & 0x08)>>3
            quoted=(kteqt & 0x04)>>2
            task=(kteqt & 0x03)
            subtask = 256*bw[1]+bw[2]
            name=bw[3]
            count=0
        end # WORDSZ
        
        return [kind,type,ext,quoted,task,subtask,name,count]
    end
    
    def from_fields(fields)
     (kind,datatype,ext,quoted,task,subtask,name,count)=fields
        #     if WORDSZ==64
        #        kte=kind.to_i*16+datatype.to_i*2+ext.to_i*1 #t uint64 #s/\.to_i//g
        #        t=task.to_i #t uint64 #s/\.to_i//
        #        q=quoted.to_i #t uint64 #s/\.to_i//
        #        qt=(q<<6)+t
        #        st=subtask.to_i #t uint64 
        #        stl=(st % 256) #t uint64
        #        sth=(st-stl)/256 #t uint64
        #        name=name.to_i #t uint64 #s/\.to_i//
        #        nl=(name % 256) #t uint64
        #        nh=(name-nl)/256 #t uint64
        #        ct=count.to_i #t uint64 #s/\.to_i//
        #        cl=(ct % 256) #t uint64
        #        ch=(ct-cl)/256 #t uint64
        #        @num=(kte<<56)+(qt<<48)+(sth<<40)+(stl<<32)+(nh<<24)+(nl<<16)+(ch<<8)+cl        
        #        else
        #        raise "TODO: from_fields"
        @num=((kind & FW_Kind) <<FS_Kind)
        @num+=((datatype & FW_Datatype)<<FS_Datatype)
        @num+=((ext & FW_Ext) << FS_Ext)
        @num+=((quoted & FW_Quoted)<<FS_Quoted)
        @num+=((task & FW_Task)<<FS_Task)
        @num+=((subtask & FW_Subtask)<<FS_Subtask)
        @num+=((name & FW_Name)<<FS_Name)
        @num+=((count & FW_Count)<<FS_Count)
#        end
    end
    
    def field(i,*arg)     
    if WORDSZ==64
        width=[4,3,1,2,6,16,16,16] 
        offset=[60,57,56,54,48,32,16,0]
        else # WORDSZ==32
        width=[3,1,1,1,2,16,8,0] 
        offset=[29,28,27,26,24,8,0,0]
        end # WORDSZ
        
        if arg.length==0                
            num1= bitslice(@num,offset[i],width[i],*arg)            
#            puts "NUM:#{@num};OFFSET:#{offset[i]};WIDTH:#{width[i]} => FIELD: #{i}:#{num1}"
            return num1
        else
            @num=bitslice(@num,offset[i],width[i],*arg)
        end        
        
    end
    
    def Kind(*arg)
        field(0,*arg)
    end
    def Datatype(*arg)
        field(1,*arg)
    end
    def Ext(*arg)
        field(2,*arg)
    end
        
    def Quoted(*arg)
        field(3,*arg)
    end
    def Task(*arg)
        field(4,*arg)
    end
    def Subtask(*arg)
        field(5,*arg)
    end
    def Name(*arg)
        field(6,*arg)
    end
    def Count(*arg)
        field(7,*arg)
    end
        
    def to_s
        str="#{kind_l(Kind())}:#{type_l(Datatype())}:#{Ext()}:#{Quoted()==1?'q':' '}:#{Task()}:#{Subtask()}:#{Name()}:#{Count()} "
        return str 
    end
    
    alias to_l to_num
    alias to_word to_num
    
end # of SBA_Symbol
# -----------------------------------------------------------------------------    
class SBA_Packet_Header
    include SBA_Bytecode
    def initialize(type_or_wordlist,*more_args)
        nargs=8
        if more_args.length==nargs-1 # Type[8], Prio[8(5)], (Redir[3],) Length[16], To[16], (Return_to[16],) Send_to[16(64)], Return_as[64]
            type=type_or_wordlist
            fields = [type]+more_args
            from_fields(fields)
        elsif more_args.length==0
            @wordlist=type_or_wordlist
        else
            raise "SBA_Packet_Header: Wrong number of args to constructor: #{more_args.length}<>#{nargs}"
        end
    end              
    
    def copy
        return SBA_Packet_Header.new(@wordlist)
    end
    
    def to_wordlist
        return @wordlist
    end
    
    def to_fields
#        hw=byteword(@wordlist[0])
#        if WORDSZ==64
#        type=hw[0]
#        prio_addr=hw[1]
#        prio=(prio_addr>>3) & 31
#        addr=prio_addr & 7
#        length=256*hw[2]+hw[3]
#        to=256*hw[4]+hw[5]
#        
#        from=256*hw[6]+hw[7]
#        rt=@wordlist[1]
#        ra=SBA_Symbol.new(@wordlist[2])            
#        return [type,prio,addr,length,to,from,rt,ra]
#        else
        word=@wordlist[0]
        type=(word & F_Packet_type) >> FS_Packet_type
        prio=(word & F_Prio) >> FS_Prio
        addr=(word & F_Redir) >> FS_Redir
        length=(word & F_Length) >> FS_Length
        to=    (word & F_To) >> FS_To
        from=(word & F_Return_to) >> FS_Return_to
        rt=@wordlist[1]
#        ra=SBA_Symbol.new(@wordlist[2])            
        ra=@wordlist[2]
        return [type,prio,addr,length,to,from,rt,ra]        
#        end
    end
    
    def from_fields(fields)
     (type,prio,addr,len,to,from,rt,ra)=fields
     if WORDSZ==64
        ll=(len % 256) #t uint64
        lh=(len-ll)/256 #t uint64
        
        tl=(to % 256) #t uint64
        th=(to-tl)/256 #t uint64
        
        prio_addr=(prio<<3)+addr
        
        frl=(from % 256) #t uint64
        frh=(from-frl)/256 #t uint64
        
        byteword1=[type,prio_addr,lh,ll,th,tl,frh,frl]
        else # WORDSZ==32
        type_prio_redir=(type << 5)+(prio<<2)+addr
        byteword1=[type_prio_redir,len,to,from]    
        end # WORDSZ
        
        num1=byteword2num(byteword1) #t uint64
        if rt.is_a?(SBA_Symbol)
            raise "Send_to field must be Word"
        end
        num2=rt
        if ra.is_a?(SBA_Symbol)
            raise "Return_As field must be Word"
            num3=ra.to_num #t uint64
        elsif ra.is_a?(Integer)
            num3=ra #t uint64
        else
            raise "Return_As field must be Symbol or Integer"
        end
           #     puts "NUM3",SBA_Symbol.new(num3)
        @wordlist=[num1,num2,num3]             
    end
    
    def field(i,*arg)
        if arg.length==0            
            return to_fields()[i]
        else
            fields=to_fields()
            fields[i]=arg[0]
            from_fields(fields)
        end
    end
    
    def Type(*arg)
        field(0,*arg)
    end
    
    def Priority(*arg)
        field(1,*arg)
    end
    
    def Redir(*arg)
        field(2,*arg)
    end
    
    def Length(*arg)
        field(3,*arg)
    end
    
    def To(*arg)        
        field(4,*arg)
    end
    
    def Return_to(*arg)        
        field(5,*arg)
    end        
    
    def Send_to(*arg)
        field(6,*arg)
    end
    
    def Return_as(*arg)                        
        field(7,*arg)
    end
    
    def to_s
        str="#{packettype_l(Type())}:l=#{Length()}:#{Redir()}:#{num2name(To())}:#{num2name(Return_to())}\n#{Send_to()}\n" #t ostringstream
        w=Return_as() #t Symbol
        str+=w.to_s
    end
end # of SBA_Packet_Header
# -----------------------------------------------------------------------------
class SBA_Packet_Payload
    
    def initialize(content)
        if content.is_a?(Array)
            if content[0].is_a?(Array)
                raise "SBA_Packet_Payload content can't be Array of Array: #{content.inspect}"
            end        
            if content[0].is_a?(SBA_Symbol)
                @wordlist=from_symbol_list(content)
            elsif content[0].is_a?(Integer) #FIXME: should test all elements!
                @wordlist=content
            else
                raise "Payload only supports lists of Symbols and Integers so far: \n#{content.inspect}."
            end
        else
            if content.is_a?(SBA_Symbol)
                @wordlist=from_symbol(content)
            elsif  content.is_a?(Integer) 
                @wordlist=[content]
            else
                raise "Payload: WARNING: only supports Symbols and Integers so far: #{content.inspect}."
            end
        end
    end
    
    def copy #t Packet_Payload
        pp=SBA_Packet_Payload.new(@wordlist)
        return pp
    end
    
    def to_wordlist
        return @wordlist
    end
    
    def from_symbol(s)  
        return s.to_wordlist
    end
    
    def from_symbol_list(sl)
        nl=[]
        for s in sl
            if s.is_a?(SBA_Symbol)
                nl.push(s.to_num)
            elsif s.is_a?(Integer) or s.is_a?(Float)
                nl.push(s)
            elsif  s.is_a?(String)
                puts "Payload: WARNING: String #{s.inspect}"
                nl.push(s.to_i)
            end
        end
        return nl
    end
    
    def to_symbol_list #t Symbol_List
        symbol_list=[] #t Symbol_List #s/=\[\]//
        ext=0 #t uint
        for num in @wordlist #t Word_List
            if num.is_a?(Array) #skip
                raise "Should not be an Array: #{num.inspect}" #skip
            end #skip
            if ext==0
                symbol=SBA_Symbol.new(num)
                symbol_list.push(symbol) 
                ext=symbol.Ext
            else
                # I think maybe we should not touch the extended symbols at all
                ext-=1
                tsym=SBA_Symbol.new(num)
                symbol_list.push(tsym)
            end
        end
        return symbol_list
    end
    
    def to_symbol #t Symbol 
        num=@wordlist[0] #t uint64 
        symbol=SBA_Symbol.new(num) #t Symbol
        return symbol
    end      
    def to_word #t uint64
        num=@wordlist[0] #t uint64 
        return num
    end
    
    # signed ints are stored as 2's complement, so must convert
    # we only support 64-bit signed ints
    def to_signed_int
        num=@wordlist[0]
        if num>2**63
            return num-2**64
        else 
            return num
        end
    end
    def to_signed_int_list
        numlist=[]
        for num in @wordlist
            if num>2**63
                snum=num-2**64
            else 
                snum= num
            end
            numlist.push(snum)
        end
        return numlist
    end    
    def to_float
        float=[@wordlist[0]].pack("Q").unpack("G")[0]
        return float
    end
    
    def to_float_list
        numlist=[]
        for num in @wordlist
            fnum=[num].pack("Q").unpack("G")[0]          
            numlist.push(fnum)
        end 
        return numlist
    end    
    
    def to_s
        str=""
        for w in @wordlist
            str+="#{SBA_Symbol.new(w)}\n"        
        end
        return str
    end
end # of SBA_Packet_Payload
#  -----------------------------------------------------------------------------  
class SBA_Packet
    include SBA_Bytecode
    def initialize(header_or_wordlist, *payload)
        if payload.length==1
            header=header_or_wordlist
            if(header.is_a?(SBA_Packet_Header) and payload[0].is_a?(SBA_Packet_Payload))
           @wordlist = header.to_wordlist+payload[0].to_wordlist
           else
            @wordlist = header+payload[0]
            end
        elsif  payload.length==0
            @wordlist=header_or_wordlist
        else
            raise "Wrong number of args in SBA_Packet constructor"
        end
    end
    
    def copy
        cp=SBA_Packet.new(@wordlist)
        return cp
    end
    
    def to_wordlist
        return @wordlist
    end
    
    def Header(*arg) #t Packet_Header
        if arg.length==0
            hw=[] #t Word_List #s/=\[\]/(2)/
            hw[0]=@wordlist[0]
            hw[1]=@wordlist[1]                           
            hw[2]=@wordlist[2]           
            h=SBA_Packet_Header.new(hw)                
            return h
        else
            h=arg[0]
            hw=h.to_wordlist
            @wordlist[0]=hw[0]
            @wordlist[1]=hw[1]
            @wordlist[2]=hw[2]                       
        end
    end
    
    def Payload(*arg) #t Packet_Payload
        if arg.length==0
            plw=[] #t Word_List
            i=0 #t int
            for w in @wordlist
                i+=1
                next if i<4
                plw.push(w)
            end
            pl=SBA_Packet_Payload.new(plw)
            return pl
        else
            p=arg[0]
            pw=p.to_wordlist
            hw[0]=@wordlist[0]
            hw[1]=@wordlist[1]
            hw[2]=@wordlist[2]           
            @wordlist=hw+pw
        end
    end
    
    def to_s
        str=""
        str+=Header().to_s()
        str+="\n------------\n"
        str+=Payload().to_s()+"\n"
        return str
    end # of to_s
    
end # of SBA_Packet

# -----------------------------------------------------------------------------

# Objects are better than primitives
class SBA_Packet_Fifo
    attr_accessor :packets,:status
    def initialize
        @packets=[]
        @status=0 
    end
end # of SBA_Packet_Fifo


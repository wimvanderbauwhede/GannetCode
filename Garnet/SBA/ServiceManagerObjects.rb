# ServiceManagerObjects.rb
#
# :title: Service-based SoC project - Service Manager objects 
#
=begin
    All classes in this file are for use inside the ServiceManager
=end
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#//==============================================================================
#//
#// Gannet Service-based SoC project - Service Manager objects 
#//
#//==============================================================================
#
#// $Id: ServiceManagerObjects.rb 2540 2009-05-06 15:56:53Z socgroup $
#++

require "SBA/Packet.rb"
if DISTR==0 and VM==0
require "SBA/Network.rb"
end
require "SBA/Memory.rb"

class SBA_RequestTable
    attr_accessor :table
    def initialize
        @table=[]
        for i in 0..NREGS-1
            @table[i]=[]
        end    
    end    
    def push(addr,val)
        @table[addr].push(val)
    end    
    # Shift a request off the list at the given address.
    def shift(addr)
        val=@table[addr].shift
        return val
    end    
    def size(addr)
        return @table[addr].size
    end    
end

class SBA_RegData_StatusWord 
    def initialize
        @statusword=0
    end    
    def set( i,  s) 
         status_i = (s << i)
         statusmask_i = RDS_MASK - (1 << i)
        @statusword = (@statusword & statusmask_i) + status_i
    end
    def get( i) 
         status = (@statusword >> i) & 1
        return status;
    end
end

class SBA_RegisterEntry
    attr_accessor :data_address, :status, :code_address,:subtask_address,:offset,:fsize
    def initialize(_da,_ca,_sa,_st)
        @statusword=SBA_RegData_StatusWord.new()
        @status=_st
        @data_address=_da
        @code_address=_ca 
        @subtask_address=_sa
        @offset=0
        @fsize=0
    end
end # of SBA_Register

# -----------------------------------------------------------------------------
# A simple stack model. Its main purpose is to initialise the stack.
# SBA_Stack#push and SBA_Stack#pop are actually implemented as shift() and unshift() 
# because otherwise the highest address values would be used first.
class SBA_Stack
    # Fill the stack with addresses
    def initialize(stack_size,offset)
        @stack=[]
        for i in 0..stack_size-1
            @stack[i]=i+offset
        end
    end
    
    # Push a freed address back onto the stack.
    def push(item)
#        @stack.unshift(item)
        @stack.push(item)
        return @stack.length
    end
    
    # Pop an address to use off the stack.
    def pop
#        return @stack.shift
        return @stack.pop
    end
    
    def size 
        return @stack.length
    end
end


# -----------------------------------------------------------------------------
# A high-level model for the subtask argument list. 
# OBSOLETE
class SBA_Subtask_Argument_List
    def initialize
        @args_status_hash={}
        @args_kind_hash={}
        @args_list=[]
        @by_status={}
    end
    
    def add(label,status,kind)
        @args_list.push(label)
        @args_status_hash[label]=status
        @args_kind_hash[label]=kind
    end
    
    def remove(label)
        @args_status_hash.delete(label)
        @args_kind_hash.delete(label)		
        new_args_list=[]
        for item in @args_list	
            new_args_list.push(item) unless item==label
        end
        @args_list=new_args_list
    end
    
    def labels
        return @args_list
    end
    
    def status(label)
        return @args_status_hash[label]
    end
    
    def kind(label)
        return @args_kind_hash[label]
    end	
    
    def set_kind(label,kind)
        @args_kind_hash[label]=kind
    end	
    
    def set_status(label,status)
        @args_status_hash[label]=status
    end
    
    def has(label)
        return @args_status_hash.has_key?(label)
    end
    
    def by_status
        for key in @args_list
            @by_status[@args_status_hash[key]].push(key)
        end
        return @by_status
    end
    
    def check_status
        subtask_status=STS_pending
        for key in @args_status_hash.keys
            t_status=@args_status_hash[key]
            tvalue=t_status
#            puts "CHECK:#{SBA_Symbol.new(key)}:#{t_status}"
            if t_status==DS_requested
                tvalue=DS_absent
            end
            subtask_status=subtask_status*tvalue
        end
        return subtask_status
    end
end # of SBA_Subtask_Argument_List
# -----------------------------------------------------------------------------
# A subtask list item stores all information required to manage a Subtask
# Can be saved as a block of 21 64-bit words. With redirect fiels, 23 words.

# 32-bit, compacted: 8 Words
# We assume that ref symbols are always K_R:NA:ext=0:quot=0, i.e. we can use these 6 bits for something else
# redir # 3bits
# to # 1byte
# return_to # 1byte
# called_as # 2bits for Task + 1byte for Name
# return_as # 2bytes for Subtask+1byte for Name
# ack_to # 2bytes for Subtask+1byte for Name
# nargs # 4bits, is Subtask field from Service symbol.  4bits=15args max
# subtask_status # 3bits
# mode: 2 bits
# arg_addresses # 8-16 * 2bytes
#     => 1+1+1+3+3= 9bytes
#    3+2+4+3=1byte 4bits 
#    10bytes+4bits = 2 words+ 2bytes+4bits, space left: 5 words + 1byte+ 4 bits
#                =>  10 args to fit into 8 words = 32 bytes
#                
# Unfortunately, for lists the subtask list item must know the code to which it belongs, i.e. it must store the code address
# that is another 16 bits, or minimum 12 bits, if we assume 10 bits address space + 2 bits paging (Task)
# Fortunately, we seem to have exactly 12 bits left!
# code_address # 12 bits. 
# To create the code reference we need code address => Task:Subtask and service => Name
# 3+2+4+3+2=14 =>2*8
# 1+1+1+2+1=6*8
# 12+3 => 2*8
# i.e. 10*8, leaves 6*8 to have 4*32
# We must redo this carefully, using 10 bits for the Subtask
class Subtask_List_Item 
    attr_accessor :status,:nargs,:mode,:arguments,:called_as,:to,:return_to,:return_as,:ack_to,:waiting_for_ack,
        :redir,:code_address,:reg,:nargs_absent,:service_id,:offset,:fsize,:result_address
    def initialize
        @status=STS_new # 3 bits of 1st byte of 1st Word
        @nargs_absent=MAX_NARGS # WV: only for VM
        @nargs=0 # Number of arguments, between 0 and 16, so 5 bits (of 1st byte of 1st Word). 
        @mode=0 #2 bits. Stream=1.Acc=2. Cache=3? else 0
	    @arguments=[]
        @called_as='' # Symbol, 2nd Word
        @to='' # 2 bytes, 5th&6th byte of 1st Word
        @return_to='' # 2 bytes, 7th&8th byte of 1st Word
        @return_as=''  # Symbol, 3rd Word
# for redirection and multicasting
        @redir=0 # 3 bits , combine with @mode? Or squirrel into Type field of @return_ack?
        @ack_to='' # 1 Word, 4 fields if redir>1, Symbol ifredir==1, empty otherwise
        @waiting_for_ack=0 # 1
        
        @code_address=0 # 12, if we don't include the service! else 16
# multi-threaded core support        
        @service_id=0 # 8 bits? or 4, if we limit the service to 4
# register support        
        @reg=0 # 3 bits
# tuple support
        @offset=0 # Assuming max. 256 Words payload => 1 byte
        @fsize=0 # Assuming max. 256 Words payload => 1 byte
# support for results returned to memory
        @result_address=0              
    end    

end # of Subtask_List_Item 
# -----------------------------------------------------------------------------
# A model for the crucial subtask list. 
# 
# In HW, this is of course simply a RAM with fixed-size fields containing a Subtask_List_Item
# The starting address of the Subtask_List_Item is calculated directly from the subtask, 
# simply: (subtask-1)*subtask_list_item_size (assuming the 0 subtask is reserved)
# 
# add: is simply a pop() + RAM write()
# remove: is simply a push(); we might reset the Subtask_Status to 'deleted'
# has_args: 
class SBA_Subtask_List
    
    def initialize
        @subtasks_hash={}		
    end
    
    def add(subtask)
        @subtasks_hash[subtask]=Subtask_List_Item.new()
    end
    
    def remove(subtask)
        @subtasks_hash[subtask].status=STS_deleted
        @subtasks_hash[subtask].nargs_absent=MAX_NARGS
        @subtasks_hash[subtask].nargs=0        

#        @subtasks_hash.delete(subtask)
    end
    
    def has(subtask)
        return @subtasks_hash.has_key?(subtask)
    end
    
    def has_args(subtask)
        #        return @subtasks_hash[subtask].has_key?('arguments') && @subtasks_hash[subtask].arguments.labels.length>0
        return @subtasks_hash[subtask].arguments.labels.length>0    
    end
    
    def arguments(subtask,*val)    
        if val.length==0
            return @subtasks_hash[subtask].arguments
        else
            @subtasks_hash[subtask].arguments=val.shift
        end        
    end
    
    def set(subtask,attr_name,attr_value)
        @subtasks_hash[subtask][attr_name]=attr_value
    end           
    
    def get(subtask,attr_name)
        return @subtasks_hash[subtask][attr_name]
    end
    
    def status(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].status
        else
            @subtasks_hash[subtask].status=val.shift
        end
    end
        
    def to(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].to
        else
            @subtasks_hash[subtask].to=val.shift
        end
    end    
    
    def return_to(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].return_to
        else
            @subtasks_hash[subtask].return_to=val.shift
        end
    end 
    
    def return_as(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].return_as
        else
            @subtasks_hash[subtask].return_as=val.shift
        end
    end 
    
    def called_as(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].called_as
        else
            @subtasks_hash[subtask].called_as=val.shift
        end
    end 
    
    def code_address(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].code_address
        else
            @subtasks_hash[subtask].code_address=val.shift
        end
    end     

    def service_id(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].service_id
        else
            @subtasks_hash[subtask].service_id=val.shift
        end
    end     
        
    def nargs(subtask,*val)
    if val.length==0
            return @subtasks_hash[subtask].nargs
        else
            @subtasks_hash[subtask].nargs=val.shift
        end
    end

    def nargs_absent(subtask,*val)
    if val.length==0
            return @subtasks_hash[subtask].nargs_absent
        else
            @subtasks_hash[subtask].nargs_absent=val.shift
        end
    end
    
    def decr_nargs_absent(subtask)
        @subtasks_hash[subtask].nargs_absent-=1
#        return @subtasks_hash[subtask].nargs_absent
    end

    def incr_nargs_absent(subtask)
        @subtasks_hash[subtask].nargs_absent+=1
#        return @subtasks_hash[subtask].nargs_absent
    end    
    
    def mode(subtask,*val)
    if val.length==0
            return @subtasks_hash[subtask].mode
        else
            @subtasks_hash[subtask].mode=val.shift
        end
    end

    def reg(subtask,*val)
    if val.length==0
            return @subtasks_hash[subtask].reg
        else
            @subtasks_hash[subtask].reg=val.shift
        end
    end
        
    def redir(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].redir
        else
            @subtasks_hash[subtask].redir=val.shift
        end
    end 
    
    def ack_to(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].ack_to
        else
            @subtasks_hash[subtask].ack_to=val.shift
        end
    end        

    def waiting_for_ack(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].waiting_for_ack
        else
            @subtasks_hash[subtask].waiting_for_ack=val.shift
        end
    end            
    
    def offset(subtask,*val)
    if val.length==0
            return @subtasks_hash[subtask].offset
        else
            @subtasks_hash[subtask].offset=val.shift
        end
    end
    
    def fsize(subtask,*val)
    if val.length==0
            return @subtasks_hash[subtask].fsize
        else
            @subtasks_hash[subtask].fsize=val.shift
        end
    end
    
    def result_address(subtask,*val)
        if val.length==0
            return @subtasks_hash[subtask].result_address
        else
            @subtasks_hash[subtask].result_address=val.shift
        end
    end

    def subtasks()
        return @subtasks_hash.keys
    end
    
    def entry(subtask)
        return @subtasks_hash[subtask]
    end
end # of SBA_Subtask_List



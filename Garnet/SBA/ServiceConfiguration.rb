# ServiceConfiguration.rb
#  
# Gannet Service-based SoC project - Service Configuration module
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: ServiceConfiguration.rb 2534 2009-04-23 11:21:04Z socgroup $

#require "SBA/ServiceCoreLibrary.rb"
module SBA_ServiceConfiguration

=begin
    NOT FOR 64-bit!
Currently, for 32-bit Gannet, the Kind field is 3 bits, the Data field 1 bit.
We really need 2 bits for the Data field, but not in all cases (I think only for K_B).
The problem is that we have 5 Kinds, not 4. And for CACHE, we'd need a new K_C Kind,
so 6. But if we assume that we only need 2 bits Data for K_B, then we can do this:
    
K_D|T_x => 001|x, 1
K_A|T_x => 010|x, 2
K_L|T_x => 011|x, 3
K_R|T_x => 100|x, 4
K_C|T_x => 101|x, 5  
K_B|T_x => 11|xx, so K_B actually spans 6 and 7

TODO: change as follows; but it means we must recompile all .td files

K_R|T_x => 00|xx, so K_R spans 0 and 1
K_A|T_x => 010|x, 2 
K_L|T_x => 011|x, 3 
K_D|T_x => 100|x, 4
K_C|T_x => 101|x, 5  
K_B|T_x => 11|xx, so K_B spans 6 and 7

=end
    
    Symbol_Kinds={ # 4 bits => we could have 16 of them. Or 3 bits is enough
        'LABEL'=>15, #skip # used only by Compiler
        'Unknown'=>16, # 0000 # only used by Compiler   
        'S'=>0, # 0000 # only used by Compiler   
        'D'=>1, # 0001 # 'Data', resurrected for CACHE/BUFFER/ACC
        'A'=>2, # 0110 # 'Argument', should really be 'A'
        'L'=>3, # 0011 # 'Lexical', this was used to request lexical variables but maybe K_D could take this function 
        'R'=> 4, # 0100 # 'Request', the most important Kind
        #'S'=> 0, # 0101 # 'Service', so we don't really need this
        'C'=> 5,
        'B'=> 6, # 0110 # 'Built-in', for constants
        'Q'=> 7 # 0111 # used as spill-over for K_B|T_s and K_B|T_b in 64-bit
        # 'E'=>8, # 1000 # not used
        # 'SL'=>, # 1011
        # 'SD'=>, # 1001
        # 'SU'=>, # 1010
    }
    
# The actual values are not that important, bit we could say
# 000 Any
# 001 Int 
# 010 Float
# 011 String? Or Char? 
# 1xx: a list of the above. So 111 would be a string, a list of chars    
#  T_d | T_i | T_f | T_c | T_L | T_I | T_F | T_s | T_q | T_x | T_Error
    Symbol_Types={ # 3 bits. Actually 1 bit is enough if we get rid of that stupid T_u in APPLY
        'd'=>0, # 000 # Data, i.e. Any
        'i'=>1, # 001 # signed int => LSB=1
        'f'=>2, # 010 # IEEE float/double => LSB=0
        'c'=>3, # 011 # char
        'L'=>4, # 100 # List (of Data)
        'I'=>5, # 101 # Word_List
        'F'=>6, # 110 # List of Float    
        's'=>7, # 111 # String (List of chars)
    }    

    
#  T_x | T_d | T_u | T_i | T_f | T_s | T_l | T_n | T_b | T_q
#  If it's a number it's either Float or Int in 2's complement. So 1 bit can indicate this.
#  If it's not a number then can use the bit for something else
#    Symbol_Types={ # 3 bits. Actually 1 bit is enough if we get rid of that stupid T_u in APPLY
#        'i'=>0, # 011 # signed int => LSB=1
#        'f'=>1, # 100 # IEEE float => LSB=0
#        's'=>2, # 010 # string
#        'b'=>3, # 011 # bool -- OBSOLETE, use i
#        'd'=>4, # 100 # Data, i.e. Any
#        'l'=>5, #skip  
#        'L'=>6, #skip 
#        'q'=>7, #skip
#        'x'=>8  #skip
#    }    
    
# For compatibility with streaming data, we either add a Mode field (stream or 'datagram') or we use a bit in the Type field
# We use the MSB to indicate the mode.
    Packet_Types={ # >7, so needs at least 4 bits. We have a 3 bits ... but things like MM can be done using a reference!
# and for non-reconfigurable systems, no need for the last three.
        'error'=>0, # Payload could be error message. In general not Symbols
        'subtask'=>1, # list of Symbols. At least 2 elts: (S ...); Can be stream
        'code'=>2, # list of Symbols; Can be stream
        'reference'=>3, # 1-elt list of Symbols; Can be stream
        'request'=>4, # 1-elt list of Symbols; Can be stream 
        'data'=>5, # preferred; Can be stream
        'mm'=>6, # list of Symbols. Usually 1-elt
        'fragment'=>7, 
        'lookup'=>8, # "what is the address of service n?", with n a number => 1-elt list of uint64
        'address'=>9, # 
        'advertise'=>10 # "my address is n", with n a number => 1-elt list of uint64
    }
    
    Core_Status={
    'idle'=>0,
    'ready'=>1,
    'busy'=>2,
    'done'=>3,
    'managed'=>4,
    'done_eos'=>5,
    'skip'=>6,
    'eos'=>7 
    }
    
    Subtask_Status={ 
   'new'=>0, # 000 activated
   'pending'=>1, # 001 means being parsed or waiting for execution I guess
   'processing'=>2, # 010 being executed
   'processed'=>3, # 011 execution finished
   'cleanup'=>4, # 100 should be 'cleaned up'?
   'blocked'=>5, # 101
   'inactive'=>6, # 110 I'll use this for recursion where we reclaim addresses but don't remove the subtask
   'deleted'=>7, # 111 purely for HW mem management: indicates that the subtask at a particular address is deleted
   'skip'=>8,
   'eos'=>9
    } 

# If error=4 and cleared=3, we need 2 bits for the actual Data_Status, another bit for error.
# As memory is at least byte-addressable and most likely 32-bit, we have plenty of bits left for
# other information regarding the storage. So we use a bitmask an have e.g. 2 bits for ACK support
# and 2 bits for streaming/fifo support
    Data_Status={
   'absent'=>0, # 00
   'present'=>1, # 01
   'requested'=>2,  # 10
   'eos'=>3, # 11 EOS is a special case of present
   'cleared'=>4, # 100 does not fit into 2 bits! So cleared becomes absent, should be OK
   'error'=>5 # 101 does not fit into 2 bits!
    }
    
    Code_Status={
    'Absent'=>0,
    'ActReq'=>1,
    'Present'=>2,
    'Reset'=>3 # Activation Request && Present => Present
    }
    
    RegData_Status={
    'absent'=>0,
    'present'=>1
#    'requested'=>2
#    'eos'=>3    
    }
    
    Mode ={
    'normal'=>0,
    'cache'=>1,
    'var'=>1,
    'acc'=>1,
    'stream'=>2,
    'buf'=>2,
    'eos'=>3
    }    
    
=begin Core status:

 * 0: Idle
 * 1: Ready
 * 2: Busy
 * 3: Done (but memory not cleaned up)
 * 4: Managed (currently not used)

Subtask status: (used in Subtask List)
 * 0: New subtask/Not fully parsed
 * 1: Ready for processing/Pending
 * 2: About to be processed or Being processed - over to Core
 * 3: Done/Processed
 * 4: Cleaning up (MM)
 * 5: Blocked (for language issues)
enum Subtask_Status {s_New,s_Pending,s_Processing,s_Processed,s_CleanUp,s_Blocked};

Data status: used for subtask arguments (in the Subtask Argument List) and Data (in the Lookup table)
 * 0: Absent
 * 1: Present
 * 2: Requested
 * 3: (Not used)
 * 4: Cleared
 enum Data_Status {s_Absent,s_Present,s_Requested, s_Not_Used,s_Cleared};
 
=end    

#skip    
    def kinds
        Symbol_Kinds
    end
    def types
        Symbol_Types
    end
    def packettypes
        Packet_Types
    end
    
    def corestatus
        Core_Status
    end
    def subtaskstatus
        Subtask_Status
    end        
    def datastatus
        Data_Status
    end

    def codestatus
        Code_Status
    end
        
    def regdatastatus
        RegData_Status
    end    
    
    def procmode
        Mode
    end        
    
    def kind_l(num)
      #if num==0 
      #  return 0 
      #else
        for item in Symbol_Kinds.keys
          if Symbol_Kinds[item]==num
            return item
          end
        end
      #end
    end
    
    def type_l(num)
      if num==0 
        return 0 
      else    
        for item in Symbol_Types.keys
          if Symbol_Types[item]==num
            return item
          end
        end
      end        
    end
    
    def packettype_l(num)
        for item in Packet_Types.keys
          if Packet_Types[item]==num
            return item
          end
        end
    end    
end # of module SBA_ServiceConfiguration

include SBA_ServiceConfiguration
#endskip

# Note that constants must match /^[A-Z][A-Z][A-Z0-9_]*/ 
# NONE=0
NA=0

if DISTR==1
GWPORT=7188
end # DISTR

# GANNET_LANGUAGE=1
STATE_REG_SZ=8
SBA_USE_ADDRESS_STACKS=1
SBA_BRIDGE_ADDR=32
SBA_BRIDGE_HW_FIFO_SZ=64
# WV17082008: the TX fifo in the Gateway must store all packets for a task, can be large
PACKET_FIFO_SZ=64 
#ifdef STATIC_ALLOC

if DISTR==0
MAX_NSERVICES = 64
MAX_NTASKS=4 # determines the number of tasks, so related to F_Task etc
else # DISTR
MAX_NSERVICES = 1
MAX_NTASKS=1 # determines the number of tasks, so related to F_Task etc
end # DISTR
MAX_NDYNCONFIGS = 64
MAX_NARGS=16 # determines size of arg address fifo
MAX_BYC_SZ=1024 # max size of program bytecode 
MAX_NPENDING_TASKS=8 # tasks waiting to be executed
#endif // STATIC_ALLOC

NARGS_SZ=8
REGS_SZ=3

MAX_CODE_PACKET_SZ=32 # in fact, the HW assumes the code itself to be 8 Words. This is too small for most lambdas, I fear.
MAX_DATA_PACKET_SZ=16 # currently, the HW has 8 Words per chunk, so the max packet size is 11; for DCT we'd need 64+3
HEADER_SZ=3
REF_PACKET_SZ=4 # 3 words header + 1 ref symbol
FIXED_PACKET_SZ=4 # for Quarc with S&F
MAX_PACKET_SZ=514 # waste space for convenience. Length field is 9 bits so max payload is 511; header=3 
MAX_LIST_SZ=256 # waste space for convenience
NREGS=8 # Actually, we could have a few more I think
NREQS=8
if SBA_USE_ADDRESS_STACKS==1
    SUBTASKS_SZ=256
    SUBTASKS_OF=0 #WV14112008: was 1 in r1884, later on changed to 0 to be compatible with C++. Set to 1 now for debugging recursive lambdas
    BUILTINS_SZ=384 # 128 per type
    BUILTINS_OF=3 # for DEBUG! should be 0!
    DATA_SZ=1023 # i.e. 10 bits
    DATA_OF=1 # reserve 0
    SERVICE_CORE_RAM_SZ=64
#    LEVELS_SZ=256
#    LEVELS_OF=1024
#    IR_SUBTASKS_SZ=256
#    IR_SUBTASKS_OF=0
#    the Code stack starts from 1, not 0, so unless we subtract -1, we need 1 address extra, so 257
#    That's ugly so I take 512
#    Just as well as code addresses are constructed with the Task so they go up to ???
    CODE_SZ=2048
    REC_STACK_SZ=16
end # SBA_USE_ADDRESS_STACKS
#GWT_RES_SZ=16
#GWT_DATA_SZ=16

NCORE_THREADS=4
MULTI_THREADED_CORE=0

MAX_NACCESSORS = 8
RDS_MASK = 0xFF


if WORDSZ==64 #  # --------------------------------------------------

NBYTES=8
 
# 64 bits = 8 bytes. 1 byte = 0xZZ
# Kind:Typ:E | Qu:Task   | Subtask               | Name 
# 0110:000:1 | 01:000000 | 0000:0000 | 0000:0001 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000
# 6   :1     | 4   :0    | 0   :0    | 0   :1    | 0   :0    | 0   :0    | 0   :0    | 0   :0     
EXTSYM = 0x6140_0001_0000_0000
#    EXTSYM = 0xC140_0001_0000_0000
# 0110:000:0 | 01:000000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000
ZERO = 0x6240_0000_0000_0000
ONE = 0x6240_0000_0000_0001
# NIHIL is an extended symbol of length 0
NIHIL = 0x6140_0000_0000_0000
F_AllOnes=0xffffffffffffffff

# 8 bits (HW uses 10 bits address space. 2 bits paging, 3 bits for chunk size, 5 bits for number of chunks
# FIXME: shouldn't I extend this for 64 bits, no need to keep it so small?
# We have 6 bits for Task, 16 for Subtask. Check subdivision of Subtask for Code use ...
# Instead of the 5 bits for HW, let's go for 10 bits
# TODO: 0x0000_03ff_0000_0000
F_CodeAddress =  0x0000_001f_0000_0000 
# 8 bits (HW needs 10?)
# TODO: 0xffff_fc00_ffff_ffff
FN_CodeAddress =  0xffff_ffe0_ffff_ffff 
# identical to FS_Subtask
FS_CodeAddress = 32 
FB_CodeAddress = 5 # TODO: 10
FW_CodeAddress = 0x1F # TODO 0x3FF
# FW_CodeAddress=31 # i.e. max. number of subtask code segments is 31! TODO: 1023


# identical to F_Task, so 6 bits
F_CodePage =  0x003f_0000_0000_0000 
FW_CodePage=0x3F
FB_CodePage=6
 
# identical to FN_Task
FN_CodePage =  0xfcff_ffff_ffff_ffff
# The code address is 
#    page=(word & F_CodePage)>> FS_Task #t Word
#    address=(word & F_CodeAddress) >> FS_Subtask #t Word    
#    service=(word & F_Service)>>FS_Name #t Word
#    code_address=(service << FS_Service)+(page << FS_CodePage)+address #t Word
FS_CodePage = FB_CodeAddress 
# 10 bits, i.e. FB_CodeAddress. With this shift, we need (FS_Task-FS_CodePage) to compress the address
F_Service= 0x0000_0000_0000_00ff 
FN_Service = 0xfff_ffff_ffff_ff00
FS_Service=FS_CodePage+FB_CodePage

# Symbol: 4|3|1 | 2|6 | 16 | 32
F_Count =    0x0000_0000_0000_0000 # no Count field!

F_Name =     0x0000_0000_ffff_ffff
F_Subtask =  0x0000_ffff_0000_0000

F_Task =     0x003f_0000_0000_0000
F_Quoted =   0x00c0_0000_0000_0000

F_Ext =      0x0100_0000_0000_0000
F_Datatype = 0x0e00_0000_0000_0000 
F_Kind =     0xf000_0000_0000_0000

# Reverse masks 
FN_Count =      0xffff_ffff_ffff_ffff
FN_Name =       0xffff_ffff_0000_0000
FN_Subtask =    0xffff_0000_ffff_ffff
FN_Task =       0xffc0_ffff_ffff_ffff 
FN_Quoted =     0xff3f_ffff_ffff_ffff
FN_Ext =        0xfcff_ffff_ffff_ffff 
FN_Datatype =   0xf1ff_ffff_ffff_ffff
FN_Kind =       0x0fff_ffff_ffff_ffff

# Shifts 
FS_Count = 0
FS_Name = 0
FS_Subtask = 32
FS_Task = 48
FS_Quoted = 54
FS_Ext = 56
FS_Datatype = 57
FS_Kind = 60

# Field width masks
FW_Kind=0xf
FW_Datatype=0x7
FW_Ext=0x1
FW_Quoted=0x3
FW_Task=0x3f
FW_Subtask=0xffff
FW_Name=0xffff_ffff
FW_Count=0x0

# Field width as number of bits
FB_Kind=4
FB_Datatype=3
FB_Ext=1
FB_Quoted=2
FB_Task=6
FB_Subtask=16
FB_Name=32
FB_Count=0

# We store a 32-bit number in the 4 LSBs, i.e. the Name field
F_Value=0x0000_0000_ffff_ffff
FN_Value=0xffff_ffff_0000_0000
FS_Value=0
FW_Value=0xffff_ffff
FB_Value=32
    
# Identical to the Name field FIXME: we need padding!!
F_NSymbols=0x0000_0000_ffff_ffff
FN_NSymbols=0xffff_ffff_0000_0000
FS_NSymbols=0

# For Packet header
# Header Word1: 8 | 5|3 | 16 | 16 | 16
F_Packet_type=0xff00_0000_0000_0000
F_Ctrl=0x00f8_0000_0000_0000
F_Redir=0x0007_0000_0000_0000
F_Length=0x0000_ffff_0000_0000
F_To=0x0000_0000_ffff_0000
F_Return_to=0x000_0000_0000_ffff
F_Send_to=[0x0000_0000_0000_ffff,0x0000_0000_ffff_0000,0x0000_ffff_0000_0000,0xffff_0000_0000_0000]

# Reverse masks
FN_Packet_type= 0x00ff_ffff_ffff_ffff
FN_Ctrl=        0xff07_ffff_ffff_ffff
FN_Redir=       0xfff8_ffff_ffff_ffff
FN_Length=      0xffff_0000_ffff_ffff
FN_To=          0xffff_ffff_0000_ffff
FN_Return_to=   0xffff_ffff_ffff_0000
FN_Send_to=[0xffff_ffff_ffff_0000,0xffff_ffff_0000_ffff,0xffff_0000_ffff_ffff,0x0000_ffff_ffff_ffff]

# Field width masks
FW_Packet_type=0xFF
FW_Ctrl=0xF8
FW_Redir=0x07
FW_Length=0xFFFF
FW_To=0xFFFF
FW_Return_to=0xFFFF

# Field width as number of bits
FB_Packet_type=8
FB_Ctrl=5
FB_Redir=3
FB_Length=16
FB_To=16
FB_Return_to=16

# Shifts 
FS_Packet_type=56
FS_Ctrl=51
FS_Redir=48
FS_Length=32
FS_To=16
FS_Return_to=0
FS_Send_to=[0,16,32,48]

# For ACC/BUFFER/CACHE
F_NArgs = 0xFF # 8 bits
FW_NArgs = F_NArgs

FW_Reg=0xF # 4 bits
FB_Reg=4
FS_Reg=10
FW_Mode=0x3 # 2 bits
FS_Mode=14

# Looks like we have ?? bits left in the K_D symbol
# Token: 5 bits, next to Name field (TODO: make larger for 64 bits)
FW_Token=0x1F
F_Token=0x1F0000
FS_Token=16
# NCons: 3 bits, next to Token (TODO: make larger for 64 bits)
 
F_NCons=0xE00000
FW_NCons=0x7
FS_NCons=19

FW_DataAddress=0x03_ff # 10 bits (TODO: make larger for 64 bits)

# We have Subtask == Mode|1Bit|Reg|DataAddress (2|[1]|3|[2]|8) == Mode|1Bit|Reg|2Bits|NArgs

# K_S needs Mode (buf=2|var=1|normal=0), K_D needs mode (stream=2,var=1)
# K_S needs Reg, so does K_D
# K_S needs NArgs
# K_S is never quoted, K_D can be quoted 
# K_S needs SCId|Opcode, K_D needs Name
# K_S maybe needs SId as well, later on 

# So in conclusion: 
# 4  :3         :1    :2       :6       :16                             :32
# K_S:(Datatype):(Ext):(Quoted):Task    :Mode|1Bit|Reg|2Bits|NArgs      :SCId|Opcode
# K_D:(Datatype):(Ext):Quoted  :Task    :Mode|1Bit|Reg|10Bits           :Name
# K_R:(Datatype):(Ext):Quoted  :CodePage:6Bits|5Bits|CodeAddress        :Name
# K_C:(Datatype):(Ext):Quoted  :Task    :Mode|1Bit|Reg|5Bits|CodeAddress:Name
# K_B:Datatype  :0    :Quoted  :Task    :16Bits                         :Value
# K_B:Datatype  :1    :Quoted  :Task    :16Bits                         :NSymbols 
# K_L:(Datatype):(Ext):Quoted  :Task    :DataAddress                    :Name
# K_A:(Datatype):(Ext):Quoted  :Task    :*                              :Name

# As we need only 5 bits for Code Address in HW we can leave Reg and Mode
# as part of the Subtask field
# For the VM it might be more tricky... I think we'll have to review using Datatype to indicate
# that a task is to be stored/looked up at APPLY. There's no way we can squeeze Reg, Mode, Name and CodeAddress in 16 bits
# 
## We want to store the register address in Datatype+Ext+Quoted, so 3 bits
## So we need another FS_Reg. I think we have FS_Reg_D and FS_Reg_C
#F_Reg_C = 0x1c_00_00_00
#FS_Reg_C = FS_Quoted
#FB_Reg_C = 3
#FW_Reg_C= 7

# For multi-threaded cores, we need F_SCLId, F_SCId and F_Opcode

F_Opcode=0xFF
FW_Opcode=0xFF
FS_Opcode=0
FB_Opcode=8

F_SCId = 0xFF00
FW_SCId=0xFF
FS_SCId=8
FB_SCId=8
# 256 libraries is a lot. I guess 64 should be enough
F_SCLId= 0x00FF0000
FW_SCLId = 0xFF
FS_SCLId = 16
FB_SCLId=8

# let's say 256 nodes is enough; we could go to 1024 by taking 2 bits from SCLId
F_SNId= 0xFF000000
FW_SNId = 0xFF
FS_SNId = 24
FB_SNId = 8

# tuple support  
FW_Offset = 0xFFFF
FS_Offset = 0
FW_Size = 0xFFFF
FS_Size = 16

# All ones
F_Symbol = 0xffff_ffff_ffff_ffff


elsif WORDSZ==32 # ------------------------------------------------------------
NBYTES=4

# 0xDD000001UL means 110|1|1|1|01| 0000 0000 0000 0000 | 0000 0001  
# it should actually be 110|0|1|1|00|0000 0000 0000 0001 | 0000
# Extended Symbol, with a "payload" of 1

# 1100|1100|
EXTSYM = 0xCC000100 
# 1101|0|1|00|
ONE  = 0xD400_0001
ZERO = 0xD400_0000
NIHIL= 0xCC000000 

F_AllOnes = 0xffffffff
# 5 bits (HW uses 10 bits address space. 2 bits paging, 3 bits for chunk size, 5 bits for number of chunks
F_CodeAddress = 0x00_00_1f_00 
# 5 bits (HW needs 10?)
FN_CodeAddress = 0xff_ff_e0_ff 
# identical to FS_Subtask. so use FS_Subtask!
FS_CodeAddress = 8 
FW_CodeAddress=31 # i.e. max. number of subtask code segments is 31!
FB_CodeAddress=5

# identical to F_Task
F_CodePage =  0x03_00_00_00 
FW_CodePage=3
FB_CodePage=2 
# identical to FN_Task
FN_CodePage =  0xfc_ff_ff_ff
# 5 bits, i.e. FB_CodeAddress
FS_CodePage = 5 # we want 11|11111|xxx for full addressing in HW but we don't encode the 3 LSb's!

# identical to F_Name, so use F_Name!
F_Service= 0x00_00_00_ff 
FN_Service = 0xff_ff_ff_00
FS_Service=FS_CodePage+FB_CodePage

# F_Status is identical to F_Task and therefore to F_CodePage

# Symbol: 3|1|1|1|2 | 16 | 8
F_Count =    0x00_00_00_00 # no Count field, actually
F_Name =     0x00_00_00_ff
F_Subtask =  0x00_ff_ff_00
F_Task =     0x03_00_00_00
F_Quoted =   0x04_00_00_00
F_Ext =      0x08_00_00_00
F_Datatype = 0x10_00_00_00
F_Kind =     0xe0_00_00_00

# Reverse masks
FN_Count =      0xff_ff_ff_ff
FN_Name =       0xff_ff_ff_00
FN_Subtask =    0xff_00_00_ff
FN_Task =       0xfc_ff_ff_ff
FN_Quoted =     0xfb_ff_ff_ff
FN_Ext =        0xf7_ff_ff_ff
FN_Datatype =   0xef_ff_ff_ff
FN_Kind =       0x1f_ff_ff_ff

# Shifts 
FS_Count = 0
FS_Name = 0
FS_Subtask = 8
FS_Task = 24
FS_Quoted = 26
FS_Ext = 27
FS_Datatype = 28
FS_Kind = 29

# Field widths as mask, i.e. 7=3'b111
FW_Kind=7
FW_Datatype=1
FW_Ext=1
FW_Quoted=1
FW_Task=3
FW_Subtask=0xffff
FW_Name=255
FW_Count=0

# Field width as number of bits
FB_Kind=3
FB_Datatype=1
FB_Ext=1
FB_Quoted=1
FB_Task=2
FB_Subtask=16
FB_Name=8
FB_Count=0

# For Packet header
# Type: min. 2 pref 4 -> say 3
# Ctrl: min. 2 pref 4 -> say 3 (Buf,Eos,Ack)
# Addr: 2             -> say 2?
# Length: 1 byte + 1 bit (511 Words)
# To: is a Name so 1 byte
# Return_to: idem
# Word 1: 3|3|1 | 9 | 8 | 8
F_Packet_type=0xe0_00_00_00
# 1d = 1_1100; must be 111
F_Ctrl=0x1d_00_00_00
F_Redir=0x02_00_00_00 
F_Length=0x01_ff_00_00
F_To=0x00_00_ff_00
F_Return_to=0x00_00_00_ff
F_Send_to=[0x00_00_00_ff,0x00_00_ff_00,0x00_ff_00_00,0xff_00_00_00]

# Reverse masks
FN_Packet_type= 0x1f_ff_ff_ff
FN_Ctrl=        0xe3_ff_ff_ff
FN_Redir=       0xfd_ff_ff_ff
FN_Length=      0xfe_00_ff_ff
FN_To=          0xff_ff_00_ff
FN_Return_to=   0xff_ff_ff_00
FN_Send_to=[0xff_ff_ff_00,0xff_ff_00_ff,0xff_00_ff_ff,0x00_ff_ff_ff]

# Field width masks
FW_Packet_type=7
FW_Ctrl=7
FW_Redir=1
FW_Length=511
FW_To=255
FW_Return_to=255

# Field width as number of bits
FB_Packet_type=3
FB_Ctrl=3
FB_Redir=1
FB_Length=9
FB_To=8
FB_Return_to=8

# Shifts 
FS_Packet_type=29
FS_Ctrl=26
FS_Redir=25
FS_Length=16
FS_To=8
FS_Return_to=0
FS_Send_to=[0,8,16,24]

# NArgs is a subfield of Subtask
F_NArgs = 0xFF
FW_NArgs = F_NArgs
# For ACC/BUFFER/CACHE
# The idea is that a P_request contains a K_D with a Mode field telling it if it is a stream request or not
# For some reason I have 2 bits with Stream being the value 2. 
# 3 bits so max 8 registers!
# Shifts are from start of Subtask field
FW_Reg=0xE 
FS_Reg=10
FW_Mode=0x3 # 0=normal,1=var/acc,2=buf/stream,3=eos
# We have 8 bits for N_ARGS, so 8 remain. We use 3 for the registers, so we keep 5. Let's say we use 2 MSBs of Subtask
FS_Mode=14

# Looks like we have 10 bit left in the K_D symbol
# Token: 5 bits, next to Name field
FW_Token=0x1F
FS_Token=8
# NCons: 3 bits, next to Token
FW_NCons=0x07
FS_NCons=13

FW_DataAddress=0x03_ff # 10 bits

# The compiler uses Ext=1 for everything>16 bits so we might as well have 16 bits here
# This means we use the Name field (1 byte) and the 1st byte of the Subtask field
F_Value=0x00_00_ff_ff
FN_Value=0xff_ff_00_00
FS_Value=0
FW_Value=0xffff
FB_Value=16

# Identical to the Name field
F_NSymbols=0x00_00_00_ff
FN_NSymbols=0xff_ff_ff_00
FS_NSymbols=0

# We have Subtask == Mode|1Bit|Reg|DataAddress (2|1|3|10) == Mode|1Bit|Reg|2Bits|NArgs

# K_S needs Mode (buf=2|var=1|normal=0), K_D needs mode (stream=2,var=1)
# K_S needs Reg, so does K_D
# K_S needs NArgs
# K_S is never quoted, K_D can be quoted 
# K_S needs SCId|Opcode, K_D needs Name
# 
# So in conclusion: 
# K_S:(Datatype):(Ext):(Quoted):Task:Mode|1Bit|Reg|2Bits|NArgs:SCId|Opcode
# K_D:(Datatype):(Ext):Quoted:Task:Mode|1Bit|Reg|10Bits:Name
# K_R:(Datatype):(Ext):Quoted:Task:6Bits|CodeAddress:Name
# K_C:(Datatype):(Ext):Quoted:Task:Mode|1Bit|Reg|xBits|CodeAddress:Name
# K_B:Datatype:Ext:Quoted:Task:( (16Bits:NSymbols) || (Value:8Bits) )
# K_L:(Datatype):(Ext):Quoted:Task:DataAddress:Name
# K_A:(Datatype):(Ext):Quoted:Task:*:Name


# As we need only 10 bits for Code Address in HW (actually only 5!) we can leave Reg and Mode
# as part of the Subtask field
# For the VM it might be more tricky... I think we'll have to review using Datatype to indicate
# that a task is to be stored/looked up at APPLY. There's no way we can squeeze Reg, Mode, Name and CodeAddress in 16 bits
# 
## We want to store the register address in Datatype+Ext+Quoted, so 3 bits
## So we need another FS_Reg. I think we have FS_Reg_D and FS_Reg_C
#F_Reg_C = 0x1c_00_00_00
#FS_Reg_C = FS_Quoted
#FB_Reg_C = 3
#FW_Reg_C= 7

# For multi-threaded cores, we need F_SCId and F_Opcode
# We use the upper 3 bits of the Name byte for the SCId, the lower 5 for the Opcode
F_SCId = 0xe0
FS_SCId=5
FB_SCId=3
FW_SCId=0x7

F_Opcode=0x1f
FS_Opcode=0
FB_Opcode=5
FW_Opcode=0x1f

# tuple support    
FW_Offset = 0xFF
FS_Offset = 0
FW_Size = 0xFF
FS_Size = 8
# in 32-bit we only encode the SNId in the K_R Name field
FW_SNId = FW_Name 
FS_SNId = 0

# All ones
F_Symbol = 0xff_ff_ff_ff
else # WORDSZ not 64 or 32 # --------------------------------------------------
raise "Only 64 and 32 bit word widths supported. Make sure WORDSZ is set to 32 or 64."
end # WORDSZ

#skip

if WORDSZ==64     
    MAX_WORD=2**64-1
elsif WORDSZ==32
    MAX_WORD=2**32-1
end # WORDSZ
#    NA=0
    NULL=0
    NONE=-MAX_WORD
    Quote=7
    for key in kinds.keys
        eval "K_#{key} = #{kinds[key]}"
    end
#        eval "K_A = K_U"
    for key in types.keys
        eval "T_#{key} = #{types[key]}"
    end
    for key in packettypes.keys
        eval "P_#{key} = #{packettypes[key]}"
    end
    for key in corestatus.keys
        eval "CS_#{key} = #{corestatus[key]}"
    end
    for key in subtaskstatus.keys
        eval "STS_#{key} = #{subtaskstatus[key]}"
    end
    for key in datastatus.keys
        eval "DS_#{key} = #{datastatus[key]}"
    end
    for key in codestatus.keys
        eval "CS_#{key} = #{codestatus[key]}"
    end
    for key in regdatastatus.keys
        eval "RDS_#{key} = #{regdatastatus[key]}"
    end
    for key in procmode.keys
        eval "M_#{key} = #{procmode[key]}"
    end
        
# end

# This YAML output is currently only used by the Gannet assembler    
if TO_YAML==1
    require 'yaml'
    service_configuration={}
    service_configuration['Symbol_Kinds']=kinds()
    service_configuration['Symbol_Types']=types()
    service_configuration['Packet_Types']=packettypes()
    service_configuration['Core_Status']=corestatus()
    service_configuration['Subtask_Status']=subtaskstatus()
    service_configuration['Data_Status']=datastatus()
    service_configuration['Code_Status']=codestatus()
    service_configuration['FS_SCId']=FS_SCId
    if ENV.has_key?('GANNET_DIR')
        scfile="#{ENV['GANNET_DIR']}/Assembler/ServiceConfiguration.yml"
    else
        scfile="ServiceConfiguration.yml"
    end
    File.open( scfile, 'w' ) do |out|
        YAML.dump(service_configuration, out )
    end
    exit
end # TO_YAML
#endskip

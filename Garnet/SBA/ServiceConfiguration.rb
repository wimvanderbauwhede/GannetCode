#/** \file ServiceConfiguration.rb
#  
# \brief Gannet Service-based SoC project - Service Configuration module
#
#*/

#*
#*  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#*  
#
#//==============================================================================
#//
#// Gannet Service-based SoC project - Service Configuration module
#//
#//==============================================================================
#
#// $Id: ServiceConfiguration.rb 2534 2009-04-23 11:21:04Z socgroup $

#require "SBA/ServiceCoreLibrary.rb"
module SBA_ServiceConfiguration

=begin

Currently, for 32-bit Gannet, the Kind field is 3 bits, the Data field 1 bit.
We really need 2 bits for the Data field, but not in all cases (I think only for K_B).
The problem is that we have 5 Kinds, not 4. And for CACHE, we'd need a new K_C Kind,
so 6. But if we assume that we only need 2 bits Data for K_B, then we can do this:

K_D|T_x => 001|x, 1
K_U|T_x => 010|x, 2 # because S_APPLY==2
K_L|T_x => 011|x, 3 # because S_LET==3
K_R|T_x => 100|x, 4
K_C|T_x => 101|x, 5  
K_B|T_x => 11|xx, so K_B actually spans 6 and 7

TODO: change as follows; but it means we must recompile all .td files

K_R|T_x => 00|xx, so K_R spans 0 and 1
K_U|T_x => 010|x, 2 # because S_APPLY==2
K_L|T_x => 011|x, 3 # because S_LET==3
K_D|T_x => 100|x, 4
K_C|T_x => 101|x, 5  
K_B|T_x => 11|xx, so K_B spans 6 and 7

=end
    
    Symbol_Kinds={ # 4 bits => we could have 16 of them. Or 3 bits is enough
        'LABEL'=>15, #skip # used only by Compiler
        'Unknown'=>16, # 0000 # only used by Compiler   
        'S'=>0, # 0000 # only used by Compiler   
        'D'=>1, # 0001 # 'Data', resurrected for CACHE/BUFFER/ACC
        'U'=>2, # 0110 # 'Argument', should really be 'A'
        'L'=>3, # 0011 # 'Lexical', this was used to request lexical variables but maybe K_D could take this function 
        'R'=> 4, # 0100 # 'Request', the most important Kind
        #'S'=> 0, # 0101 # 'Service', so we don't really need this
        'C'=> 5,
        'B'=> 6, # 0110 # 'Built-in', for constants
        'Q'=> 7 # 0111 # used as spill-over for K_B|T_s and K_B|T_b
        # 'E'=>8, # 1000 # not used
        # 'SL'=>, # 1011
        # 'SD'=>, # 1001
        # 'SU'=>, # 1010
    }
#  T_x | T_d | T_u | T_i | T_f | T_s | T_l | T_n | T_b | T_q
#  If it's a number it's either Float or Int in 2's complement. So 1 bit can indicate this.
#  If it's not a number then can use the bit for something else
    Symbol_Types={ # 3 bits. Actually 1 bit is enough if we get rid of that stupid T_u in APPLY
        'i'=>0, # 011 # signed int => LSB=1
        'f'=>1, # 100 # IEEE float => LSB=0
        's'=>2, # 010 # string
        'b'=>3, # 011 # bool
        'd'=>4, # 100
        'l'=>5, #skip 
        'L'=>6, #skip 
        'q'=>7, #skip
        'x'=>8  #skip
    }    
    
# For compatibility with streaming data, we either add a Mode field (stream or 'datagram') or we use a bit in the Type field
# Adding the Mode is a lot of hassle. Say we use the MSB to indicate the mode.
    Packet_Types={ # >7, so needs at least 4 bits. We have a 3 bits ... but things like MM can be done using a reference!
# and for non-reconfigurable systems, no need for the last three.
        'error'=>0, # Payload could be error message. In general not Symbols
        'subtask'=>1, # list of Symbols. At least 2 elts: (S ...); Can be stream
        'code'=>2, # list of Symbols; Can be stream
        'reference'=>3, # 1-elt list of Symbols; Can be stream
        'request'=>4, # 1-elt list of Symbols; Can be stream 
        'data'=>5, # preferred; Can be stream
        'mm'=>6, # list of Symbols. Usually 1-elt 
        'lookup'=>7, # "what is the address of service n?", with n a number => 1-elt list of uint64
        'address'=>8, # 
        'advertise'=>9 # "my address is n", with n a number => 1-elt list of uint64
    }
    
    Core_Status={
    'idle'=>0,
    'ready'=>1,
    'busy'=>2,
    'done'=>3,
    'managed'=>4
        #    '-'=>5,
    }
    
    # We need a status "inactive"; 
    # And maybe also "parsed", I'm not sure
    Subtask_Status={ 
   'new'=>0, # 000 activated
   'pending'=>1, # 001 means being parsed or waiting for execution I guess
   'processing'=>2, # 010 being executed
   'processed'=>3, # 011 execution finished
   'cleanup'=>4, # 100 should be 'cleaned up'?
   'blocked'=>5, # 101
   'inactive'=>6, # 110 I'll use this for recursion where we reclaim addresses but don't remove the subtask
   'deleted'=>7 # 111 purely for HW mem management: indicates that the subtask at a particular address is deleted
    } 

# If error=4 and cleared=3, we need 2 bits for the actual Data_Status, another bit for error.
# As memory is at least byte-addressable and most likely 32-bit, we have plenty of bits left for
# other information regarding the storage. So we use a bitmask an have e.g. 2 bits for ACK support
# and 2 bits for streaming/fifo support
    Data_Status={
   'absent'=>0,
   'present'=>1,
   'requested'=>2, 
   'error'=>3,
   'cleared'=>4
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
    }
    
    Mode ={
    'normal'=>0,
    'cache'=>1,
    'var'=>1,
    'acc'=>1,
    'stream'=>2,
    'buf'=>2,
    'unused'=>3
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

# GANNET_LANGUAGE=1
STATE_REG_SZ=8
SBA_USE_ADDRESS_STACKS=1
SBA_BRIDGE_ADDR=32
SBA_BRIDGE_HW_FIFO_SZ=64
# WV17082008: the TX fifo in the Gateway must store all packets for a task, can be large
PACKET_FIFO_SZ=64 
#ifdef STATIC_ALLOC
MAX_NTASKS=4 # determines the number of tasks, so related to F_Task etc
MAX_NSERVICES = 64
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
MAX_PACKET_SZ=32 # waste space for convenience
MAX_LIST_SZ=256 # waste space for convenience
NREGS=8
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

F_AllOnes=0xffffffffffffffff

# 8 bits (HW uses 10 bits address space. 2 bits paging, 3 bits for chunk size, 5 bits for number of chunks
F_CodeAddress = 0x00_00_1f_00 
# 8 bits (HW needs 10?)
FN_CodeAddress = 0xff_ff_e0_ff 
# identical to FS_Subtask
FS_CodeAddress = 8 

# identical to F_Task
F_CodePage =  0x003f_0000_0000_0000 
FW_CodePage=3
FB_CodePage=2 
FW_CodeAddress=31 # i.e. max. number of subtask code segments is 31!
FB_CodeAddress=5
# identical to FN_Task
FN_CodePage =  0xfc_ff_ff_ff
# 8 bits. With this shift, we need (FS_Task-FS_CodePage) to compress the address
FS_CodePage = 8 
F_Service= 0x00_00_00_ff 
FN_Service = 0xff_ff_ff_00
FS_Service=FS_CodePage+FB_CodePage

# Symbol:4|3|1 | 2|6 | 16 | 16 | 16
F_Count =    0x0000_0000_0000_ffff
F_Name =     0x0000_0000_ffff_0000
F_Subtask =  0x0000_ffff_0000_0000
F_Task =     0x003f_0000_0000_0000
F_Quoted =   0x00c0_0000_0000_0000
F_Ext =      0x0100_0000_0000_0000
F_Datatype = 0x0e00_0000_0000_0000 
F_Kind =     0xf000_0000_0000_0000

# Reverse masks 
FN_Count =      0xffff_ffff_ffff_0000
FN_Name =       0xffff_ffff_0000_ffff
FN_Subtask =    0xffff_0000_ffff_ffff
FN_Task =       0xffc0_ffff_ffff_ffff 
FN_Quoted =     0xff3f_ffff_ffff_ffff
FN_Ext =        0xfcff_ffff_ffff_ffff 
FN_Datatype =   0xf1ff_ffff_ffff_ffff
FN_Kind =       0x0fff_ffff_ffff_ffff

# Shifts 
FS_Count = 0
FS_Name = 16
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
FW_Name=0xffff
FW_Count=0xffff

# Field width as number of bits
FB_Kind=4
FB_Datatype=3
FB_Ext=1
FB_Quoted=2
FB_Task=6
FB_Subtask=16
FB_Name=16
FB_Count=16


# For Packet header
# Header Word1: 8 | 5|3 | 16 | 16 | 16
F_Packet_type=0xff00_0000_0000_0000
F_Prio=0x00f8_0000_0000_0000
F_Redir=0x0007_0000_0000_0000
F_Length=0x0000_ffff_0000_0000
F_To=0x0000_0000_ffff_0000
F_Return_to=0x000_0000_0000_ffff
F_Send_to=[0x0000_0000_0000_ffff,0x0000_0000_ffff_0000,0x0000_ffff_0000_0000,0xffff_0000_0000_0000]

# Reverse masks
FN_Packet_type= 0x00ff_ffff_ffff_ffff
FN_Prio=        0xff07_ffff_ffff_ffff
FN_Redir=       0xfff8_ffff_ffff_ffff
FN_Length=      0xffff_0000_ffff_ffff
FN_To=          0xffff_ffff_0000_ffff
FN_Return_to=   0xffff_ffff_ffff_0000
FN_Send_to=[0xffff_ffff_ffff_0000,0xffff_ffff_0000_ffff,0xffff_0000_ffff_ffff,0x0000_ffff_ffff_ffff]

# Field width masks
FW_Packet_type=0xFF
FW_Prio=0xF8
FW_Redir=0x07
FW_Length=0xFFFF
FW_To=0xFFFF
FW_Return_to=0xFFFF

# Field width as number of bits
FB_Packet_type=8
FB_Prio=5
FB_Redir=3
FB_Length=16
FB_To=16
FB_Return_to=16

# Shifts 
FS_Packet_type=56
FS_Prio=51
FS_Redir=48
FS_Length=32
FS_To=16
FS_Return_to=0
FS_Send_to=[0,16,32,48]

# For ACC/BUFFER/CACHE
F_NArgs = 255 # 8 bits
F_Reg=0x07 # 3 bits
FS_Reg=16
F_Mode=3 # 2 bits
FS_Mode=24
# All ones
F_Symbol = 0xffffffffffffffff

F_Offset = 0xFFFF
FS_Offset = 0
F_Size = 0xFFFF
FS_Size = 16

elsif WORDSZ==32 # ------------------------------------------------------------
NBYTES=4
F_AllOnes = 0xffffffff
# 5 bits (HW uses 10 bits address space. 2 bits paging, 3 bits for chunk size, 5 bits for number of chunks
F_CodeAddress = 0x00_00_1f_00 
# 5 bits (HW needs 10?)
FN_CodeAddress = 0xff_ff_e0_ff 
# identical to FS_Subtask. so use FS_Subtask!
FS_CodeAddress = 8 

# identical to F_Task
F_CodePage =  0x03_00_00_00 
FW_CodePage=3
FB_CodePage=2 
FW_CodeAddress=31 # i.e. max. number of subtask code segments is 31!
FB_CodeAddress=5
# identical to FN_Task
FN_CodePage =  0xfc_ff_ff_ff
# 8 bits. With this shift, we need (FS_Task-FS_CodePage) to compress the address
FS_CodePage = 5 # we want 11|11111|xxx for full addressing in HW but we don't encode the 3 LSB's!

# identical to F_Name, so use F_Name!
F_Service= 0x00_00_00_ff 
FN_Service = 0xff_ff_ff_00
FS_Service=FS_CodePage+FB_CodePage

# F_Status is identical to F_Task and therefore to F_CodePage



# Symbol:3|1|1|1|2 | 16 | 8
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
# Prio: min. 2 pref 4 -> say 3
# Addr: 2             -> say 2?
# Length: 1 byte
# To: is a Name so 1 byte
# Return_to: idem
# Word 1: 3|3|2 | 8 | 8 | 8
F_Packet_type=0xe0_00_00_00
# 1d = 1_1100; must be 111
F_Prio=0x1d_00_00_00
F_Redir=0x03_00_00_00
F_Length=0x00_ff_00_00
F_To=0x00_00_ff_00
F_Return_to=0x00_00_00_ff
F_Send_to=[0x00_00_00_ff,0x00_00_ff_00,0x00_ff_00_00,0xff_00_00_00]

# Reverse masks
FN_Packet_type= 0x1f_ff_ff_ff
FN_Prio=        0xe3_ff_ff_ff
FN_Redir=       0xfc_ff_ff_ff
FN_Length=      0xff_00_ff_ff
FN_To=          0xff_ff_00_ff
FN_Return_to=   0xff_ff_ff_00
FN_Send_to=[0xff_ff_ff_00,0xff_ff_00_ff,0xff_00_ff_ff,0x00_ff_ff_ff]

# Field width masks
FW_Packet_type=7
FW_Prio=7
FW_Redir=4
FW_Length=255
FW_To=255
FW_Return_to=255

# Field width as number of bits
FB_Packet_type=3
FB_Prio=3
FB_Redir=2
FB_Length=8
FB_To=8
FB_Return_to=8

# Shifts 
FS_Packet_type=29
FS_Prio=26
FS_Redir=24
FS_Length=16
FS_To=8
FS_Return_to=0
FS_Send_to=[0,8,16,24]

# NArgs is a subfield of Subtask
F_NArgs = 255

# For ACC/BUFFER/CACHE
# The idea is that a P_request contains a K_D with a Mode field telling it if it is a stream request or not
# For some reason I have 2 bits with Stream being the value 2. 

F_Reg=0x07
FS_Reg=10
F_Mode=3 # 0=normal,1=var/acc,2=buf/stream

# We have 8 bits for N_ARGS, so 8 remain. We use 3 for the registers, so we keep 5. Let's say we use 2 MSBs of Subtask
FS_Mode=14
F_DataAddress=0x03_ff # 10 bits

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
# K_B:Datatype:Ext:Quoted:Task:(16Bits:NSymbols | Value)
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

F_SCId = 0xe0
FS_SCId=5
FB_SCId=3
FW_SCId=7

F_Opcode=0x1f
FS_Opcode=0
FB_Opcode=5
FW_Opcode=0x1f

# tuple support    
F_Offset = 0xFF
FS_Offset = 0
F_Size = 0xFF
FS_Size = 8

# All ones
F_Symbol = 0xff_ff_ff_ff
else # WORDSZ not 64 or 32 # --------------------------------------------------
raise "Only 64 and 32 bit word widths supported. Make sure WORDSZ is set to 32 or 64."
end # WORDSZ

#skip
# if NUM==0    
#    NA='NA'
#    NULL='NULL'
#    NONE='NONE'
#    Quote='Quote'   
# if WORDSZ==64     
#    MAX_WORD=2**64-1
# elsif WORDSZ==32
#    MAX_WORD=2**32-1
# end # WORDSZ
#    for key in kinds.keys
#        eval "K_#{key} = '#{key}'"
#    end
#    
#    for key in types.keys
#        eval "T_#{key} = '#{key}'"
#    end
#    
#    for key in packettypes.keys
#        eval "P_#{key} = '#{key}'"
#    end
#    
#    for key in corestatus.keys
#        eval "CS_#{key} = '#{key}'"
#    end
#    
#    for key in subtaskstatus.keys
#        eval "STS_#{key} = #{subtaskstatus[key]}"
#    end
#    
#    for key in datastatus.keys
#        eval "DS_#{key} = #{datastatus[key]}"
#    end
    
# else # NUM==1
if WORDSZ==64     
    MAX_WORD=2**64-1
elsif WORDSZ==32
    MAX_WORD=2**32-1
end # WORDSZ
    NA=0
    NULL=0
    NONE=-MAX_WORD
    Quote=7
    for key in kinds.keys
        eval "K_#{key} = #{kinds[key]}"
    end
        eval "K_A = K_U"
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
    if ENV.has_key?('GANNET_DIR')
        scfile="#{ENV['GANNET_DIR']}/Garnet-HW/ServiceConfiguration.yml"
    else
        scfile="ServiceConfiguration.yml"
    end
    File.open( scfile, 'w' ) do |out|
        YAML.dump(service_configuration, out )
    end
end # TO_YAML
#endskip

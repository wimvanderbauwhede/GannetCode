{-# LANGUAGE CPP #-}
module Gannet.SBA.Constants (
------ constants
c_CODE_SZ,
c_REC_STACK_SZ,
c_N_REGS,
c_NBITS_NOT_EXT,
c_FS_SNId,
c_FW_SNId,
c_FS_SCId,
c_FW_SCId,
c_FS_Opcode,
c_FW_Opcode,
c_WORDSZ,
-- field operations
shiftSNId,
shiftSCId,
shiftOpcode,
getSNId,
getSCId,
getOpcode
) where

import Data.Bits

shiftSNId snid =  (snid .&. (fromInteger c_FW_SNId)) `shiftL` (fromInteger c_FS_SNId)
shiftSCId scid =  (scid .&. (fromInteger c_FW_SCId)) `shiftL` (fromInteger c_FS_SCId)
shiftOpcode opc =  (opc .&. (fromInteger c_FW_Opcode)) `shiftL` (fromInteger c_FS_Opcode)
            
getSNId num = (num `shiftR` (fromInteger c_FS_SNId)) .&. c_FW_SNId            
getSCId num = (num `shiftR` (fromInteger c_FS_SCId)) .&. c_FW_SCId
getOpcode num = (num `shiftR` (fromInteger c_FS_Opcode)) .&. c_FW_Opcode

-- TODO: change for 64-bit?
c_CODE_SZ = 256
c_REC_STACK_SZ = 16
c_N_REGS = 8

c_SHIFT_SCId = 32



-- Max size of signed integer for not-extended builtin
#if WORDSZ==32
-- 32-bit Word
c_NBITS_NOT_EXT = 0x7FFFFF
#else
-- 64-bit Word
c_NBITS_NOT_EXT = 0x7FFFFFFFFFFF
#endif 


-- Shifts and masks for SCId and Opcode
c_FS_Opcode = 0
#if WORDSZ==32
-- 32-bit Word
c_FS_SCId = 5 --  means we support 8 Service Classes with 32 opcodes each
c_FW_SCId = 0x7
c_FW_Opcode = 0x1F
c_FW_SNId = 0 -- No room for the Node Id in 32-bit
c_FS_SNId = 0
#else
-- 64-bit Word
c_FS_SCId = 8  --  means we support 256 Service Classes with 256 opcodes each
c_FW_SCId = 0xFF
c_FW_Opcode = 0xFF
c_FW_SNId = 0xFFFF
c_FS_SNId = 16
#endif
 
c_WORDSZ :: Int
#if WORDSZ==32
-- 32-bit Word
c_WORDSZ = 32
#else
-- 64-bit Word
c_WORDSZ = 64
#endif

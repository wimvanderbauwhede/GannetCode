{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}
module Gannet.SBA.SystemConfiguration (
Services,
Aliases,
ALUNames,
services,
serviceids,
servicetokens,
aliases,
alunames,
lookupServiceId,
lookupServiceName,
c_CODE_SZ,
c_REC_STACK_SZ,
c_N_REGS,
c_NBITS_NOT_EXT,
c_SHIFT_SCId
) where
import qualified Data.Map as Hash
import Gannet.SBA.Types
import Gannet.SBA.ConfigReader
import {-# SOURCE #-} Main ( ymlFileName )

type Services = Hash.Map String (Integer,Integer)
type Aliases = Hash.Map String (String,Integer,Integer)
type ALUNames = Hash.Map String String

lookupServiceName :: GannetToken -> GannetToken 
lookupServiceName opname@(GannetTokenS gl) =
    let
        (GannetTokenS gl) = opname
        GannetLabelS aname = gl
        sname = case Hash.lookup aname services of
                Just _ -> opname
                Nothing -> case Hash.lookup aname alunames of 
                        Just alias -> case Hash.lookup alias aliases of
                                Just (aaname,_,_) -> (GannetTokenS (GannetLabelS aaname))
                                Nothing -> error $ "No parent service for "++aname++"\n"
                        Nothing -> case Hash.lookup aname aliases of
                                Just (asname,_,_) -> (GannetTokenS (GannetLabelS asname))
                                Nothing -> error $ "No parent service for "++aname++"\n"                                                                       
    in
        sname    
lookupServiceName opname = opname
        
lookupServiceId :: GannetToken -> Integer
lookupServiceId opname@(GannetTokenS gl) =
    let
        (GannetTokenS gl) = opname
        GannetLabelS aname = gl
        sid = case Hash.lookup aname services of
                Just (sid,scid) -> sid
                Nothing -> case Hash.lookup aname alunames of 
                        Just alias -> case Hash.lookup alias aliases of
                                Just (aaname,sid,_) -> sid
                                Nothing -> error $ "No parent service for "++aname++"\n"
                        Nothing -> case Hash.lookup aname aliases of
                                Just (asname,sid,_) -> sid
                                Nothing -> error $ "No parent service for "++aname++"\n"                                                                       
    in
        sid 
lookupServiceId opname = 0

services :: Services
services =  Hash.fromList servicelist 

serviceids :: [Integer]
serviceids = map (\s->(let (_,(num,_)) = s in num)) servicelist

servicecoreids :: [Integer]
servicecoreids = map (\s->(let (_,(_,num)) = s in num)) servicelist

servicetokens :: [GannetToken]
servicetokens = map (\s->(let (token,_) = s in (GannetTokenQ token))) servicelist

aliases :: Aliases
aliases = Hash.fromList aliaslist

alunames :: ALUNames
alunames = Hash.fromList alunamelist

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

(servicelist, aliaslist, alunamelist)= readSBAConfig ymlFileName -- defaults to SBA.yml

{- 
Internals should contain only functions that will not have to be modified
when adding new features to the compiler.

That is only the case for 
remapSubtask
getAssignVarStack
symKindDet
symDatatype
symExt

symCount contains a reference to "data", it is unlikely that I will re-introduce the DATA service or even the DATA construct

createSymCtxt might need modification to support Registers
compileSym, symKindEtc are likely to change to accommodate Registers
-}

module Gannet.Symbolizer.Internals (
    createSymCtxt,
    compileSym,
    symKindEtc,
    symKindDet,
    symDatatype,
    symExt,
    symCount
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Context
import Gannet.State.Scope
import Gannet.Numerifier

import qualified Data.Map as Hash

{-
createSymCtxt creates the GannetSymbol corresponding to the Token x (passed as a TokenTree)
It also update the current service, caller stack, current lambda and lambda stack.
To handle BUF etc, we also need Mode and Reg. So either we pass these on as extra
args, or we squirrel then into ctxt, or we make a new function
I think I'll go for the Context: simply add the previous symbol to the Context
That would be K_D:Mode=BUF:Register=(varname,regval)
-}

createSymCtxt :: TokenTree -> [TokenTree] -> Context -> (SymbolTree,Context)
createSymCtxt x tl ctxt =
    let
                -- compileSym compiles the GannetSymbol
                -- remapSubtask remaps the code address
                xgsym1=compileSym x K_Unknown tl ctxt                                
                (xgsym,ctxt2)
                    | (kind xgsym1)==K_Label  = (xgsym1{kind=K_R},ctxt)
--                    | (kind xgsym1) == K_S || (kind xgsym1) == K_C = remapSubtask xgsym1 ctxt
                    | otherwise = (xgsym1,ctxt)
                skind=kind xgsym
                servicename = stringInToken x
                currentservice= 
                    if skind==K_S 
                        then servicename
                        else current ctxt
                callerservices=
                    if skind==K_S
                        then
                            servicename:(callerstack ctxt)
                        else
                            callerstack ctxt
                ncurrentlambda
                    | (stringInToken x)=="lambda" = subtaskc ctxt
                    | otherwise = currentlambda ctxt
                nlambdastack
                    | (stringInToken x)=="lambda" = (currentlambda ctxt):(lambdastack ctxt)
                    | otherwise = lambdastack ctxt
                xsym = Symbol xgsym
                -- LABEL handling
                -- the first K_S after the label triggers insertion of the label in the reflabelt: label => xgsym{kind=K_R}
                -- 
                nreflabelt 
                    | (skind==K_S) && (reflabel ctxt /= emptyGT) = Hash.insert (reflabel ctxt) xgsym{kind=K_R} (reflabelt ctxt)
                    | otherwise = reflabelt ctxt
                nctxt=ctxt2{    current=currentservice,
                            callerstack=callerservices,
                            currentlambda=ncurrentlambda,
                            lambdastack=nlambdastack,
                            reflabel=emptyGT, -- not sure about this
                            reflabelt=nreflabelt,    
                            prevsym=emptyGS
                            }
    in
        (xsym,nctxt)                

{-
-- remap the subtask using per-service address counters
                
remapSubtask :: GannetSymbol -> Context -> (GannetSymbol,Context)
remapSubtask gs ctxt =
    let
        sid = lookupServiceId (name gs)
        maddr = case Hash.lookup sid (addrcounters ctxt) of
                    Just a -> a
                    Nothing -> -1
        nctxt 
            | maddr>0 =
                let
                    naddr = maddr+1
                    ncounters=Hash.insert sid naddr (addrcounters ctxt)
                in
                    ctxt{addrcounters=ncounters}
            | otherwise = ctxt    
        addr
            | maddr>0 = maddr
            | otherwise = (subtask gs)
        
    in
        (gs{subtask=addr},nctxt)
-}

{-
If it's K_Unknown and not deterministic, it should be in the scope.
if skind == K_Unknown && symKindDet name == K_Unknown 
then -- look up in scope table
else
 
-}

compileSym :: TokenTree -> GSymbolKind -> [TokenTree] -> Context -> GannetSymbol
compileSym token skind tl ctxt  
    | Hash.member name (reflabelt ctxt) = 
        case Hash.lookup name (reflabelt ctxt) of
            Just gs -> gs{kind=K_Label}
            Nothing -> errorGS        
    | skind == K_Unknown && symKindDet name == K_Unknown = 
        if (Hash.member name (datastore ctxt)) -- is it K_D?
            then 
                MkGannetSymbol K_D T_d 0 0 task 0 name 1 0 M_normal emptyGReg
            else    
                getGSfromScope name (letc ctxt) (getAssignVarStack name ctxt) (scope ctxt) 100
    | otherwise = 
        let
            (kind,subtask,lambda) = symKindEtc name skind ctxt
            datatype = symDatatype name
            ext = symExt name
            quoted=0            
            count
                | kind == K_L = lambda
                | otherwise = symCount tl kind
--            clambda
--                | kind == K_L = subtaskc ctxt
--                | otherwise = lambda
            gvsym = prevsym ctxt
            gs = MkGannetSymbol kind datatype ext quoted task subtask name count lambda (mode gvsym) (reg gvsym)
        in    
            gs
    where
        (Token name) = token        
        task=taskc ctxt
                
getAssignVarStack :: GannetToken -> Context -> [Integer]
getAssignVarStack var ctxt = 
    case Hash.lookup var (assignvarstacks ctxt) of
        Just st -> st
        Nothing -> []                

{-
If it's not a specific keyword and the kind is K_Unknown, try to work out the kind.

-for ASSIGN, LAMBDA and DATA, the kind is passed in via skind
- K_B, K_Q and K_S are deterministic
- so what remains calls to K_L, K_A and K_D
-}
        
symKindEtc :: GannetToken -> GSymbolKind -> Context -> (GSymbolKind,Integer,Integer)
symKindEtc gt skind ctxt 
    | skind == K_Q = (K_Q,0,0)
    | (skind==K_Unknown) && (dkind/=K_Unknown) = -- deterministic
        let
            dsubtask
                | dkind == K_B = dsubtaskc -- We need unique identifiers for extended symbols
                | otherwise = dsubtaskc
        in
            (dkind,dsubtask,0)        
    | skind == K_D = (K_D,0,0)
    | skind == K_L = (K_L,dletc,dlambda)
    | skind == K_A = (K_A,dsubtaskc,dlambda)            
    | otherwise = (skind,dsubtaskc,dlambda) -- should not come here!
        
    where
        dkind=symKindDet gt -- see if it's K_S, K_Q or K_B
        dlambda = inLambda ctxt
        dletc = (letc ctxt)
        dsubtaskc = (subtaskc ctxt)

{-
Try to determine the Kind in a deterministic way first
-}

symKindDet :: GannetToken -> GSymbolKind
symKindDet gt = case gt of 
    (GannetTokenS x) -> K_S
    (GannetTokenB x) -> K_B
    (GannetTokenQ x) -> K_Q
    (GannetTokenL (GannetLabelS x)) -> 
        if
            (Hash.member x services)||(Hash.member x aliases)||(Hash.member x alunames)
            then
                K_S
            else
                K_Unknown        
{-
Rules for datatypes
Actually, we could do a complete type inference thing here, but that seems way over the top.
The reason for knowing the type is know what to do with a byte, essentially.
So we distinguish between String (ASCII for the moment), Int (signed! watch out!) and Float
-}        
symDatatype :: GannetToken -> GDataType
symDatatype gt = case gt of
    (GannetTokenB b) -> case b of
        (GannetBuiltinI i) -> T_i
        (GannetBuiltinF f) -> T_f
        (GannetBuiltinS s) -> T_s
    (GannetTokenQ q) ->  T_q
    (GannetTokenL d) ->  T_x
    (GannetTokenS s) ->  T_d

symExt :: GannetToken -> Int
symExt gt = case gt of
    (GannetTokenB b) -> case b of
-- let's sort this out: We have actually 24 bits out of 32 or 48 out of 64.     
--        (GannetBuiltinI i) -> if ((i>32767)||(i< -32766)) then 1 else 0 -- FIXME: only for 64-bit!
        (GannetBuiltinI i) -> if ((i>c_NBITS_NOT_EXT)||(i< 1-c_NBITS_NOT_EXT)) then 1 else 0 -- FIXME: only for 64-bit!
        (GannetBuiltinF f) -> 1
        (GannetBuiltinS s) -> 1
    (GannetTokenQ q) ->  0
    (GannetTokenL d) ->  0
    (GannetTokenS s) ->  0        
    
symCount :: [TokenTree] -> GSymbolKind -> Integer
{-                                              
symCount tl@(t:_) kind 
    | (kind==K_S) && ((stringInToken t)/="data") = toInteger (length tl)
    | (kind==K_S) && ((stringInToken t)=="data") = toInteger  ((length tl) - 1) -- because we skip the value
    | kind==K_B = toInteger (length tl) -- rather 'ad hoc' but should do the job
    | otherwise = 1
-}         
symCount [] kind = 0         
symCount tl@([t]) kind 
    | (kind==K_S) && ((stringInToken t)/="data") = toInteger (length tl)
    | (kind==K_S) && ((stringInToken t)=="data") = toInteger  ((length tl) - 1) -- because we skip the value
    | kind==K_B = toInteger (length tl) -- rather 'ad hoc' but should do the job
    | otherwise = 1              
    where
		t=head tl                    
symCount tl@(t:_) kind 
    | (kind==K_S) && ((stringInToken t)/="data") = toInteger (length tl)
    | (kind==K_S) && ((stringInToken t)=="data") = toInteger  ((length tl) - 1) -- because we skip the value
    | kind==K_B = toInteger (length tl) -- rather 'ad hoc' but should do the job
    | otherwise = 1
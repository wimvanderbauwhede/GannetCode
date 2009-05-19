-- |The symbolizer transforms a TokenTree into a SymbolTree. 
-- This is the core of the compiler.
-- Gannet symbol Kinds are identified based on the token type, position in the expression or from the context.
module Gannet.Symbolizer (
    symbolize
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.Symbolizer.InferTypes
import Gannet.State.SymbolTree
import Gannet.State.Context
import Gannet.State.Scope
import Gannet.Numerifier
import Gannet.Symbolizer.Internals

import Control.Monad.State
import System.IO
import qualified Data.Map as Hash
import Text.ParserCombinators.Parsec hiding (State)

{-
How to do 'taint checking'. i.e. detect which blocks should be stored at APPLY?
T1-if a block contains K_A's
T2-or it contains blocks containing K_A's
T3-or it contains K_L's binding blocks containing K_A's

(lambda 'x 'y (let 
                (assign 'v (+ x y :T1) :T2)
                (+ v 1 :T3)
                :T2)
)

the T3 rule requires the v to be tagged as tainted. As the whole taint check 
is only used inside the compiler, it would be better to add a 'taint' field 
to the GannetSymbol and to the SymbolListHeader. Call it 'lambda' :-)
So if we have an assign of which the value is tainted, 
the variable should be tainted as well 
-}

{-
Apart from that, with the view of supporting closures, K_L lexicals 
also come in 2 groups:
-Those that are called inside a nested lambda
(lambda 'x '(let (assign 'v 4) (lambda 'y '(+ v x y))))
-those that bind an expression containing lambda args

-If we encounter a v, and it's in a lambda (check the let_or_lambda context)
-if that lambda is the last expression in the let
OR it is bound to an f and that f is the last expression in the let
OR it is inside and IF

Clearly we need a rigorous type checker!
-}

-- | The Bool argument indicates if the symbols are numerified or not.
-- The returned Context contains the DataStore.        
symbolize :: TokenTree -> (SymbolTree,Context)
symbolize tt = (st,ctxt)
    where
        (st,ctxt) = (unwrapST (t2sm (tt,emptyC)) emptySL)

-- | A monadic tree walker to convert the TokenTree into a SymbolTree
t2sm :: (TokenTree,Context) -> State SymbolTree (TokenTree,Context)
t2sm (tt,ctxt)
    | ((length tl)==0) = do
        let
            ncurrent:ncallerstack= callerstack ctxt
            nletc:nouterc=
                if current ctxt =="let" || current ctxt == "lambda" 
                then (outerc ctxt)        
                else (letc ctxt):(outerc ctxt)
            ncurrentlambda:nlambdastack
                | currentlambda ctxt == subtaskc ctxt = lambdastack ctxt
                | otherwise = (currentlambda ctxt):(lambdastack ctxt)
            nctxt
                | head (callerstack ctxt) =="assign" = popAssignStack ctxt -- now this is really weird 
--                | (current ctxt) =="assign" = popAssignStack ctxt -- this is not the same!
                | otherwise = ctxt
            nnctxt=nctxt{ letc=nletc,
                        outerc=nouterc,
                        current=ncurrent,
                        callerstack=ncallerstack,
                        currentlambda=ncurrentlambda,
                        lambdastack=nlambdastack
                        }
        return (tt,nnctxt)    
    | isTokenList x = do
        let
            nsubtaskc= (subtaskc ctxt)+1
            TokenList (fx:xs)=x
            nx
                | (stringInToken fx) == "label" = TokenList xs                
                | otherwise = x
        tct <- case (stringInToken fx) of
			"stream" -> parseStream (TokenList xs) (tt2,ctxt)
			"get" -> parseGet (TokenList xs) (tt2,ctxt)         
			otherwise -> 
				let                
					(st2,nctxt) = (unwrapST (t2sm (x,ctxt{subtaskc=nsubtaskc})) emptySL) -- this creates a new ST based on the TT x
				in                        
					appendST st2 (tt2,nctxt) 
        t2sm tct
    | otherwise =         -- It's a token
        let
            (xsym2,nctxt) = createSymCtxt x tl ctxt
        in    
            do            
                case (stringInToken x) of
                    "label" -> do
                        tct3 <- parseLabel (tt2,ctxt)
                        t2sm tct3
                    "buf" -> do
                        tct3 <- parseBuf (tt2,ctxt)
                        t2sm tct3                            
--                    "OFFstream" -> do
--                        tct3 <- parseStream (tt2,ctxt)
--                        t2sm tct3                        
                    otherwise -> do
                        tct1 <- appendST xsym2 (tt2,nctxt)                
                        case  (stringInToken x) of 
                            "let" -> do -- WV 26/03/2008: what about LETTC?
                                let
                                    (tt,ctxt2)=tct1
                                    nouterc=(letc ctxt2):(outerc ctxt2)
                                    nletc=(subtaskc ctxt2)
                                    nscope=initScope nletc (letc ctxt2) (inLambda ctxt2) (scope ctxt2)
                                    tct3 = (tt,ctxt2{letc=nletc,outerc=nouterc,scope=nscope})
                                t2sm tct3
                            "assign" -> do
                                tct3 <- parseAssignArg tct1
                                t2sm tct3
                            "lambda" -> do
                                let
                                    (tt,ctxt2)=tct1
                                    nouterc=(letc ctxt2):(outerc ctxt2)
                                    nletc=(subtaskc ctxt2)
                                    tct2 = (tt,ctxt2{letc=nletc,outerc=nouterc})                    
                                tct3 <- parseLambdaArgs tct2
                                t2sm tct3                            
                            "data"        -> do
                                tct3 <- parseData tct1
                                t2sm tct3                                                                
                            otherwise    -> do 
                                t2sm tct1
    where
        (TokenList tl)=tt
        x:xs=tl
        tt2=(TokenList xs)

--------------------------------------------------------------------------------
-- Parsing of Language Services
--------------------------------------------------------------------------------
        
{- |
parsing Lambda arguments
-} 
parseLambdaArgs tct
    | (length xs)>2 = 
        do
            tct3 <- appendQSym tct K_A
            parseLambdaArgs tct3                
    | otherwise =
        do
            return tct
    where
        ((TokenList xs),ctxt)=tct
        ntct=((TokenList xs),ctxt)
        
-- | parsing Assign arguments
parseAssignArg tct =
    do            
        tct3 <- appendQSym tct K_L
        return tct3

-- | parse Data construct
-- values for Data are stored in the DataStore in the Context
-- labels for Data are stored on level 0 of the Scope     
parseData tct = appendQSym tct K_D

{-
Now this is rather dense. What happens is:
-tct is transformed into tct1; then a Quote is
appended to the SymbolTree, then the actual Symbol
tct1 contains nnctxt, this is nctxt with updated 
var count, datastore and scope
nctxt is ctxt with var pushed onto the AssignStack if it's a K_L
In fact, a lot of the complexity is caused by the K_D

Now, for Label support, what we do is return 
tct1=((TokenList xs2),nnctxt) like below, but no appendST calls
Furthermore, we add an entry for the Label in some lookup table in the context
-}

{-
(label L_i (S_j ...))
What we want is 
- remove the (label ... ) call from the AST
- identify label symbols
- replace occurences of label symbols with the corresponding references.
The first step is to create the reference R_j from S_j. 
Then we put this in the context, via a lookup table labels L_i=>R_j
A label could be identified by the symbol kind or we could have a
corresponding (call ...) or (goto ...) or (do ...)
Alternatively of course we could have the label tagged onto the service:
(let:L_i ...)
Our philosophy has always been that Gannet should have almost no syntax
and certainly no sugar. So I guess (label L_i (... L_i)) is best.

So we must transform ["label", "L_i", ["S_j",...]] into
["S_j",...] and a Hash "L_i" => R_j 

-}
parseLabel tct = 
    do
        return tct1
    where
        ((TokenList ltl),ctxt)=tct                
        lbl:[tl]=ltl
        Token glbl=lbl
        nctxt=ctxt{reflabel=glbl}
        tct1=(tl,nctxt)    
--        tct1=((TokenList tl),nctxt)    

appendQSym tct skind = 
    do
        tct2 <- appendST qsym tct1
        tct3 <- appendST xsym tct2
        return tct3
    where
        qsym=Symbol quoteGS
        ((TokenList xs),ctxt)=tct            
        nvarc=(varc ctxt)+1         
        (var,xs2,nletc,nouterc,nlambda,ndatastore)        
            | skind == K_D = parseD xs ctxt            
            | otherwise = parseLA xs ctxt
        Token gtvar=var 
        nctxt 
            | skind == K_L = pushAssignStack gtvar ctxt
            | otherwise = ctxt
        xgsym = compileSym var skind xs nctxt
        nscope=appendScope xgsym nvarc skind nletc nouterc nlambda (scope nctxt)
        nnctxt=nctxt{varc=nvarc,datastore=ndatastore,scope=nscope}
        tct1=((TokenList xs2),nnctxt)        
-- WV12082008: Attempt to postpone numerification until after packetization          
--        num=numeric ctxt
--        xsym=Symbol (numerify xgsym num nscope)
        xsym=Symbol xgsym 

-- | parse L or A Kind
-- This mainly extracts a list of items from the Context
parseLA :: [TokenTree] -> Context -> (TokenTree,[TokenTree],Integer,Integer,Integer,DataStore)
parseLA xs ctxt =
    let
        q:var:xs2=xs    
        ndatastore=datastore ctxt                
        nletc = letc ctxt
        nouterc:_= outerc ctxt
        nlambda=inLambda ctxt        
    in
        (var,xs2,nletc,nouterc,nlambda,ndatastore)            

-- | parse D Kind        
parseD xs ctxt =
    let            
        q:var:val:xs2=xs    
        (Token gt)=var            
        (Token (GannetTokenB gval))=val
        ndatastore=appendData gt gval (datastore ctxt)    
        nletc =0
        nouterc=0
        nlambda=0
    in
        (var,xs2,nletc,nouterc,nlambda,ndatastore)


--------------------------------------------------------------------------------
-- Gannet Symbol Manipulation        
--------------------------------------------------------------------------------

{-
This looks if the block bound to K_L is a LAMBDA.
If so, then what? Nothing realy, apart that the variable can be passed to APPLY
So it might be useful for error checking.
-}
bindsLambda :: [TokenTree] -> Context -> Bool
bindsLambda tl ctxt =
    let
        tt=(TokenList tl)
        (st2,_) = unwrapST (t2sm (tt,ctxt)) emptySL
    in
        case st2 of
            Symbol gs         -> (lambda gs)==1
            SymbolList (_,slh)     -> (rlambda slh)==1

--------------------------------------------------------------------------------
-- Buffer/Stream handling
--------------------------------------------------------------------------------
{-
(buf|var|cache|acc|stream V_i (S_j ...))
What we want is 
- remove the (buf|var|cache|acc ... ) call from the AST
- identify register/var symbols
- replace occurences of var symbols with the corresponding registers.
Presumably, we can defer that till numerification
At the very least bind the var symbol to a register value.
There are two cases: 
- (buf|var|cache|acc v (S ...)) result in the mode field 
of S being set to the corresponding type and the Register field set to the
(varname, regval) tuple. If the expression after the var token is not a
service expression or not present, that's a compile-time error.
- (stream v) results in a lookup of the register value and the service name
That means we must store (regval, service name) in some lookup table in the context
My idea is to have a lookup table like this:
Hash registers = {
servicename => { varname1 => regval1, varname2 => regval2 }
}
We immediately see a small problem: no scope! So ideally we should actually get unique varnames

The first step is to create the reference R_j from S_j. 
Then we put this in the context, via a lookup table labels L_i=>R_j
A label could be identified by the symbol kind or we could have a
corresponding (call ...) or (goto ...) or (do ...)
Alternatively of course we could have the label tagged onto the service:
(let:L_i ...)
Our philosophy has always been that Gannet should have almost no syntax
and certainly no sugar. So I guess (label L_i (... L_i)) is best.

So we must transform ["label", "L_i", ["S_j",...]] into
["S_j",...] and a Hash "L_i" => R_j 

-}

-- (stream ' b1)
parseStream tt tct = 
    do
        tct2 <- appendST vsym tct
        return tct2
    where
        (TokenList xs) = tt
        (tt2,ctxt)=tct                   
        q:var_token:xs2=xs -- so obviously xs2 is empty
        Token var=var_token         
        gvsym=compileStreamVarSym var ctxt     
        vsym = Symbol gvsym
--        tct1=(tt2,ctxt)

-- | parse Stream register var
parseStreamVar xs =
    let            
        q:var:xs2=xs    
        (Token gt)=var            
    in
        (var,xs2)


compileStreamVarSym :: GannetToken -> Context -> GannetSymbol
compileStreamVarSym var_token ctxt =
    let
        reg_info = case Hash.lookup var_token (regvartable ctxt)  of
                    Just reg_info -> reg_info
                    Nothing -> error $ "The variable "++(show var_token)++" is not bound to any service\n"
        sid = service_id reg_info
        task=taskc ctxt     
        greg = (var_token, (register reg_info)) 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 (GannetTokenL $ GannetLabelI sid) 0 0 M_buf greg
    in        
        gvsym
        
-- (get ' b1)
parseGet tt tct = 
    do
        tct2 <- appendST vsym tct
        return tct2
    where
        (TokenList xs) = tt
        (tt2,ctxt)=tct                   
        q:var_token:xs2=xs -- so obviously xs2 is empty
        Token var=var_token         
        gvsym=compileGetVarSym var ctxt     
        vsym = Symbol gvsym
--        tct1=(tt2,ctxt)

-- | parse Get register var -- UNUSED?
parseGetVar xs =
    let            
        q:var:xs2=xs    
        (Token gt)=var            
    in
        (var,xs2)

-- Note : identical to Stream apart from M_var so refactor!
compileGetVarSym :: GannetToken -> Context -> GannetSymbol
compileGetVarSym var_token ctxt =
    let
        reg_info = case Hash.lookup var_token (regvartable ctxt)  of
                    Just reg_info -> reg_info
                    Nothing -> error $ "The variable "++(show var_token)++" is not bound to any service\n"
        sid = service_id reg_info
        task=taskc ctxt     
        greg = (var_token, (register reg_info)) 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 (GannetTokenL $ GannetLabelI sid) 0 0 M_var greg
    in        
        gvsym        


parseBuf tct = 
    do
        tct2 <- appendST ssym tct1
        return tct2
    where
        ((TokenList xs),ctxt)=tct                   
        (var,xs2,nletc,nouterc,nlambda) = parseBufVar xs ctxt            
        nctxt=compileVarSym var M_buf xs2 ctxt     
        TokenList xs2l = xs2
        sgt:xs3=xs2l
        (ssym, nnctxt) = createSymCtxt sgt xs3 nctxt 
        tct1=((TokenList xs3),nnctxt)        

-- | parse Buffer register var
parseBufVar xs ctxt =
    let            
        q:var:[xs2]=xs    
        (Token gt)=var            
        nletc =0
        nouterc=0
        nlambda=0
    in
        (var,xs2,nletc,nouterc,nlambda)
            
-- BufVarSymbol is K_D:*:*:*:task:Mode|Reg:name
compileVarSym :: TokenTree -> GMode -> TokenTree -> Context -> Context
compileVarSym var_token mode tt ctxt =
    let
        TokenList tl = tt
        (Token service_token):tl2=tl
        service_name = lookupServiceName service_token
        task=taskc ctxt     
        Token var =var_token
        (reg,nctxt) = assignReg service_name var ctxt 
        gvsym=MkGannetSymbol K_D T_x 0 0 task 0 service_name 0 0 mode reg
    in        
        nctxt{prevsym=gvsym}
                

assignReg :: GannetToken -> GannetToken -> Context -> (GRegister,Context)
assignReg service_name vartoken ctxt =
    let
        register_table = registers ctxt
        cregvartable = regvartable ctxt        
        (greg,nctxt) = --if Hash.member cregvartable vartoken
                       -- then
                       --     error "The variable "++(show vartoken)++" is already in the register table for "++(show service_name)++"\n"
                       -- else
                            let
                               sid = lookupServiceId service_name
                               (creg,nctxt) = case Hash.lookup sid register_table of
                                    Just creg -> 
                                        let
                                            nregister_table = Hash.insert sid (creg-1) register_table
                                        in
                                            (creg,ctxt{registers=nregister_table})
                                    Nothing -> error $ "No entry for service "++(show service_name)++" in register table\n"                               
                               reg = (MkRegInfo creg sid)    
                               greg=(vartoken, creg)
                               nregvartable=Hash.insert vartoken reg cregvartable
                            in
                                (greg,nctxt{regvartable=nregvartable})
    in
       (greg,nctxt)       

--------------------------------------------------------------------------------
-- Assign Stack manipulation
--------------------------------------------------------------------------------


-- use currentassign to store the GannetToken.
-- On entering an ASSIGN block: 
pushAssignStack :: GannetToken -> Context -> Context
pushAssignStack var ctxt =
    let
        currentassignvar=currentassign ctxt
        nassignstack
            |currentassignvar==emptyGT =assignstack ctxt -- this starts out as []. 
            |otherwise = (currentassign ctxt):(assignstack ctxt) -- this starts out as []
        ncurrentassign= var
        assignvarstack = case Hash.lookup var (assignvarstacks ctxt) of
                        Just st -> st
                        Nothing -> []
        nassignvarstacks = Hash.insert var ((letc ctxt):assignvarstack) (assignvarstacks ctxt)
    in
        ctxt{currentassign=ncurrentassign,assignstack=nassignstack,assignvarstacks=nassignvarstacks}

-- on leaving an ASSIGN block:
popAssignStack :: Context -> Context
popAssignStack ctxt =         
    let
        currentassignvar=currentassign ctxt
        ncurrentassign:nassignstack 
            | length (assignstack ctxt)>0 = assignstack ctxt
            | otherwise = (currentassign ctxt):[]
        nassignvarstack = 
            case Hash.lookup currentassignvar (assignvarstacks ctxt) of
                Just st -> st 
                Nothing -> []    
        nnassignvarstack
            | length nassignvarstack==0 = []
            | otherwise = tail nassignvarstack
        nassignvarstacks=Hash.insert currentassignvar nnassignvarstack (assignvarstacks ctxt)
        nmon=((head (callerstack ctxt))++":"++(current ctxt)):(mon ctxt)
        nctxt = ctxt{currentassign=ncurrentassign,assignstack=nassignstack,assignvarstacks=nassignvarstacks,mon=nmon}
    in        
        nctxt            
        
module Gannet.State.SymbolTree (
    unwrapST,
    appendST
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Scope
import Gannet.State.Context
import Gannet.Numerifier
import Gannet.Symbolizer.InferTypes

import Control.Monad.State 
import qualified Data.Map as Hash
--import Data.Bits

{-
So the clue is:
NOT so much the actual Monad, but
the wrapping/unwrapping functions that go with it!
After creating my own TState Monad, I now see I can just use the ordinary State Monad
-}

unwrapST :: State SymbolTree (TokenTree,Context) -> SymbolTree -> (SymbolTree,Context)
unwrapST lst st0 =
    let
        (State lf)=lst
        ((tt,ctxt),st) = lf st0
    in 
        (st,ctxt)

{-
I'd like to modify the SLH here: 
If ms is a symbol, do this; if it's a symbollist, do that

-}
updateSLH :: SymbolTree -> SymbolTree -> Context -> (SymbolTree,Context) -- looks like we could use a monad here
updateSLH st ms ctxt =
    let
        nsl= getSL st         
        slh = getSLH st
        slhl = label slh
--        num=numeric ctxt
        (nslh,ctxt1)= case ms of    
            Symbol gs -> 
                let 
                    nlambda
                        | kind gs == K_A = lambda gs
                        | otherwise = newLambda (lambda gs) (rlambda slh)                    
                in
                if (kind gs)==K_S 
                    then
                        let
                -- set to to current from the context in (name st2)
--                WV12082008: try to postpone numerification until Packetizer
--                            to=numService (aliasedTo (GannetLabelS (current ctxt))) num
                            to=aliasedTo (GannetLabelS (current ctxt))
                -- set return_to to the first elt in the callerstack:
                            _:caller:_=(callerstack ctxt)
--                WV12082008: try to postpone numerification until Packetizer
--                            from=numService (GannetLabelS caller) num
                            from=GannetLabelS caller
--                            name2 = gsMaybeAliased (name gs)
                            ntagged
                                | (reflabel ctxt) /= emptyGT = True
                                | otherwise = False
                            nretval = inferTypeBySymbol (name gs) (kind gs) (datatype gs)                            
                            getDT (GSV (gk,gdt)) = gdt
                            getDT  _ = T_d
                            ndatatype = reconcileTypes (datatype slhl) (getDT nretval)
                            nlabel = gs{kind=K_R,datatype=ndatatype,name=(GannetTokenL to)}
                        in
                            (slh{lto=to,lfrom=from,label=nlabel,rlambda=nlambda,tagged=ntagged,retval=nretval},ctxt)
                    else
                        let
--                WV12082008: try to postpone numerification until Packetizer
--                            notlambda = (lto slh) /= numService (GannetLabelS "lambda") num
                            notlambda = (lto slh) /= GannetLabelS "lambda"
                            nlabel=label slh
                            ndatatype = reconcileTypes (datatype slhl) (inferGDTbyArgs st ms)
                        in
                            if (kind gs)==K_A && notlambda -- this is a proper subtask with a lambda var, not a lambda expr
                                then
                                    (slh{rlambda=nlambda,label=nlabel{count=nlambda,datatype=ndatatype},to_apply=True},ctxt)
                                else
                                    (slh{rlambda=nlambda,label=nlabel{count=nlambda,datatype=ndatatype}},ctxt)       
            SymbolList (_,gslh) ->
                let
--                WV12082008: try to postpone numerification until Packetizer
--                    notlambda = (lto slh) /= numService (GannetLabelS "lambda") num
                    notlambda = (lto slh) /= GannetLabelS "lambda"
                    nlambda
                     | notlambda = newLambda (rlambda gslh) (rlambda slh)
                     | otherwise = 0
                    maybe_to_apply
                      | notlambda = (to_apply gslh) || (to_apply slh)
                      | otherwise = False                                             
                in
                    let
                        nlabel=label slh
                        ndatatype = reconcileTypes (datatype slhl) (inferGDTbyArgs st ms)
                    in
                        (slh{rlambda=nlambda,label=nlabel{count=nlambda,datatype=ndatatype},to_apply=maybe_to_apply},ctxt)

        (nsl2,nslh2,ms2,ctxt2) -- = case True of
            |(testService nslh "assign") && ((length nsl==3)||(length nsl==4)) && (notQuote ms) = 
                let
                    assign:quote:var:mq = nsl
                    Symbol varsym =var
                    ndatatype=getGDT ms                    
                    nnsl = assign:quote:(Symbol varsym{datatype=ndatatype}):mq
                    nctxt = updateVar ctxt varsym ms                    
                in
                    (nnsl,nslh,ms,nctxt)
            |(testService nslh "read") && (length nsl==2) =
                let
                    Symbol varsym = ms                   
                    scopes=scope ctxt
                    ngs = lookupGSinScope scopes varsym
                    clabel = label nslh
                    nlabel = clabel{datatype=(datatype ngs)}                                   
                in
                    (nsl,slh{label=nlabel},Symbol ngs,ctxt)
            |((testService nslh "apply")||(testService nslh "applytc")) && (length nsl>1) =
                let    
                    nctxt = bindLambdaArg ctxt (SymbolList (nsl,nslh)) ms
                {-
                   -- get the 2nd elt of the SL (the first is K_S)
                   s:lfst:_ = nsl 
                   largs = case lfst of
                        SymbolList (lfsl,lfslh) ->                                     
                            if (testService (getSLH lfst) "lambda" num) -- presumably it must be a "read" otherwise
                                then 
                                    getLambdaArgs lfst num
                                else if (testService (getSLH lfst) "read" num)
                                    then
                                        -- get the LAMBDA via the K_L inside the READ
                                        -- (apply (read 'v)
                                        --unfortunately, it can be (apply (read x))
                                        -- In that case, we need to lookup the K_L bound to the K_A
                                        -- so we lookup K_A, and this should be allowed to fail
                                        -- then we get the K_L (which is all it can be, or not?)
                                        -- then we lookup the expr bound to the K_L as below
                                        let
                                            lfv = case getSL lfst of
                                                      s:q:(Symbol lfv):args -> lfv
                                                      s:(Symbol lfv):args -> lfv
                                            varsym = if kind lfv == K_A
                                                        then 
                                                            let
                                                                vst = lookupVar ctxt lfv 
                                                                vs = case vst of
                                                                        Symbol tvs -> tvs
                                                                        _ -> emptyGS
                                                            in
                                                                vs 
                                                        else 
                                                            lfv                                                        
                                            lfargs = getLambdaArgs (lookupVar ctxt varsym) num
--                                            s:q:(Symbol lfv):args = getSL lfst
--                                            lfargs = case Hash.lookup (symbolToGVarName lfv) (varbindings ctxt) of
--                                                Just lfst -> getLambdaArgs lfst
--                                                _   -> error $ "The variable " ++ (show lfv) ++ " is not bound to a LAMBDA expr."
                                        in
                                            lfargs
                                    else
                                        error "First argument of APPLY must be LAMBDA, READ or K_L"
                        -- in case the APPLY arg is a bare K_L        
                        Symbol lfv -> getLambdaArgs (lookupVar ctxt lfv) num
--                        Symbol lfv -> case Hash.lookup (symbolToGVarName lfv) (varbindings ctxt) of
--                            Just lfst -> getLambdaArgs lfst
--                            _   -> error $ "The variable " ++ (show lfv) ++ " is not bound to a LAMBDA expr."
                   -- So now we must store ms at the corresponing argval
                   eltindex=(length nsl)-2
                   argsym=largs !! eltindex
                   argv = symbolToGVarName argsym
                   cvarbindings=varbindings ctxt
                   nvarbindings= Hash.insert argv ms cvarbindings
                   
                   ndatatype = getGDT ms
                   cscopes = scope ctxt
                   nscopes = updateGDTinScope cscopes argsym ndatatype
                   nctxt=ctxt{varbindings=nvarbindings,scope=nscopes}
                -}
                in
--                    if (testService nslh "apply" num) && (length nsl>6)
--                        then error $ "APPLY:" ++ (show (nsl,nslh,ms,Hash.keys (varbindings nctxt), scope nctxt))
--                        else 
                    (nsl,nslh,ms,nctxt)
            | otherwise = (nsl,nslh,ms,ctxt)
{-            
        (nsl2,ctxt2) = if (testService nslh "assign" num) && ((length nsl==3)||(length nsl==4)) && notQuote ms 
            then
--                error $ "ASSIGN:" ++ (show nslh) -- OK, we come here
                -- (assign 'v expr)
                -- the problem is that the expr can be quoted, in which case we need to get rid of the quote
                let
                    assign:quote:var:mq = nsl
                    Symbol varsym =var
                    -- here we reconcile the datatype of the "assign" expression with that of the K_L
                    -- I think this is useless: we should use the type of ms!
                    -- There is no reason whatsoever why a K_L or an ASSIGN expre should _not_ get the type of the expr.
--                    ndatatype=reconcileTypes (datatype varsym) (datatype (label nslh))
                    ndatatype=getGDT ms                    
                    nnsl = assign:quote:(Symbol varsym{datatype=ndatatype}):mq
                    nctxt = updateVar ctxt varsym ms                    
--                    scopes=scope ctxt
--                    nscopes = updateGDTinScope scopes varsym ndatatype
--
--                    -- Here we update the VarBindings in the context.
--                    cvarbindings=varbindings ctxt
--                    nvarbindings = Hash.insert (symbolToGVarName varsym) ms cvarbindings
--                    nctxt=ctxt{scope=nscopes,varbindings=nvarbindings}   
                in
                    (nnsl,nctxt)
            else                        
                (nsl,ctxt) 
        -- purely for simplicity over efficiency, we deal with READ here
        -- (read 'v)
        (nslh2,ms2) =if (testService nslh "read" num) && (length nsl==2)
            then
--                error $ "READ:" ++ (show nslh) -- OK, we come here too
                let
                    Symbol varsym = ms                   
                    scopes=scope ctxt
                    ngs = lookupGSinScope scopes varsym
                    clabel = label nslh
                    nlabel = clabel{datatype=(datatype ngs)}                                   
                in
                    (slh{label=nlabel},Symbol ngs)
            else
                 (nslh,ms)
        -- Now deal with APPLY                 
        -- (apply (read 'v) exp1 ... expn)
        -- what we want to do is set the K_A's type from the exprs, an easy way is to
        -- add them to the VarBindings. So for every expr we need to get the LAMBDA,
        -- surely that's very inefficient. It would be better to do it for all
        -- when we reach the last expr. But "last" depends on the LAMBA
        -- so unless we can store the lambda in the SLH we have no chance
        -- well, let's do the inefficient way.
        ctxt3 = if ((testService nslh "apply" num)||(testService nslh "applytc" num)) && (length nsl>1) -- means ms must be an arg expr         
            then
--                error $ "APPLY:" ++ (show nslh)
                let                
                   -- get the 2nd elt of the SL (the first is K_S)
                   s:lfst:_ = nsl 
                   largs = case lfst of
                        SymbolList (lfsl,lfslh) ->                                     
                            if (testService (getSLH lfst) "lambda" num) -- presumably it must be a "read" otherwise
                            then 
                                getLambdaArgs lfst
                            else if (testService (getSLH lfst) "read" num)
                            then
                                -- get the LAMBDA via the K_L inside the READ
                                let
                                    s:q:(Symbol lfv):[] = getSL lfst
                                    lfargs = case Hash.lookup (symbolToGVarName lfv) (varbindings ctxt2) of
                                        Just lfst -> getLambdaArgs lfst
                                        _   -> error $ "The variable " ++ (show lfv) ++ " is not bound to a LAMBDA expr."
                                in
                                    lfargs
                            else
                                error "First argument of APPLY must be LAMBDA, READ or K_L"
                        -- in case the APPLY arg is a bare K_L        
                        Symbol lfv -> case Hash.lookup (symbolToGVarName lfv) (varbindings ctxt2) of
                            Just lfst -> getLambdaArgs lfst
                            _   -> error $ "The variable " ++ (show lfv) ++ " is not bound to a LAMBDA expr."
                   -- So now we must store ms at the corresponing argval
                   eltindex=(length nsl)-2
                   argsym=largs !! eltindex
                   argv = symbolToGVarName argsym
                   cvarbindings=varbindings ctxt2
                   nvarbindings= Hash.insert argv ms cvarbindings
                   ndatatype = getGDT ms
                   cscopes = scope ctxt2
                   nscopes = updateGDTinScope cscopes argsym ndatatype
                   nctxt=ctxt2{varbindings=nvarbindings,scope=nscopes}
                in
                   nctxt
            else
                ctxt2
-}                
    in
        (SymbolList (nsl2++[ms2],nslh2),ctxt2)

        
{-

tct2 <- appendST qsym tct1

appendST appends a SymbolTree (i.e. either a Symbol or a list of Symbols)
to an (invisible ) SymbolTree. You pass it a SymbolTree and
a (TokenTree,Context) tuple. It will modify the Context as required;
appendST does not modify the TokenTree, that happens in t2sm. 

It's just a convenient way of passing the TokenTree around in the monad.


-}

        
appendST :: SymbolTree -> (TokenTree,Context) -> State SymbolTree (TokenTree,Context)
--appendST st2 (tt,ctxt) = State (\st->((tt,ctxt{reflabel=emptyGT}),(updateSLH st st2 ctxt)))


appendST st2 (tt,ctxt) = State (\st ->
        let
            (st3,ctxt2)=updateSLH st st2 ctxt
            ctxt3=ctxt2{reflabel=emptyGT}
        in
            ((tt,ctxt3),st3)    
    )

{-
if one of them is 0, take the other.
if one is 11 and the other 12:
e.g. (lambda:11 (s:12... (... (lambda:16 ... (s:17...
-}

newLambda :: Integer -> Integer -> Integer
newLambda l1 l2 
    | (l1 == l2) && (l1==0) = 1 -- Ad Hoc 
    | l1 == 0 = l2
    | l2 == 0 = l1
    | l1 <= l2 = l1 -- dubious
    | l1 > l2 = l2
    
aliasedTo :: GannetLabel -> GannetLabel
aliasedTo gls =
    case Hash.lookup sname alunames of
        Just alias -> case Hash.lookup alias aliases of
                Just (agls,_,_) -> GannetLabelS agls
                Nothing -> gls
        Nothing -> gls
    where
        GannetLabelS sname = gls

   
{-
-- What totally baffled me is that the state MUST be the first argument
-- I guess the "TState st" below is partial application, whereas I thougt it refered to the lambda
-- I tried to remove the "tt" from this, but then I get a "kind error".
newtype TState st tt = TState (st -> (tt,st))
                        
instance Monad (TState st) where
    return tt = TState (\st -> (tt,st))
    (TState fr) >>= g = TState $ \st1 -> 
                    let
                        (tt1,st2) =fr st1
                        (TState gr) = g tt1                            
                    in      
                        gr st2                  


unwrapST :: TState SymbolTree TokenTree -> SymbolTree -> SymbolTree
unwrapST lst st0 =
            let
                (TState lf)=lst
                (tt,st) = lf st0
            in 
                st      
                
appendST :: SymbolTree-> TokenTree -> TState SymbolTree TokenTree
appendST st2 tt= TState (\st->(tt,(SymbolList ((getSL st)++[st2]))))                                   
-}

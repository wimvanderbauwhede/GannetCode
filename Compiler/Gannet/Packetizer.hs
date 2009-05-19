-- | Functions for turning a SymbolTree into a Gannet PacketList.
-- Functions for emitting human-readable and bytecode strings.
module Gannet.Packetizer (
    packetize,
    gplToWords,
    writeData    
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.Numerifier
import Gannet.Bytecodizer
import Gannet.State.Scope
import Gannet.State.Context

import Control.Monad.State 
import qualified Data.Map as Hash

packetize :: SymbolTree -> Context -> Bool -> PacketList
packetize st ctxt num= pl_remapped -- pl_lrefs
    where
        stw=SymbolList ([st],emptySLH)
        st_r=(stw,emptyGS)
        scopes = scope ctxt
        (pl,lrefs) = unwrapPL (st2plm st_r scopes Hash.empty num) emptyGPL
        pl_lrefs = appendLambdaRefs pl lrefs num
        pl_remapped = Hash.fromList $ remapSubtasksPL (Hash.toList pl_lrefs) ctxt emptySM []
        

type LambdaRefs = Hash.Map Integer [GannetSymbol]
        
appendLambdaRefs :: PacketList -> LambdaRefs -> Bool -> PacketList
appendLambdaRefs pl lrefs num = Hash.fromList (map (\(r,p)->appendIfLambda r p lrefs num) (Hash.toList pl))

appendIfLambda :: GannetSymbol ->  GannetPacket -> LambdaRefs -> Bool -> (GannetSymbol,GannetPacket)
appendIfLambda pref p lrefs num
    | name pref /= numServiceGT "lambda" num = (pref,p)
--    | otherwise = error $ "LREFS:"++(show lrefs)++";\nPREF:"++(show pref)++"\n"
    | otherwise = case Hash.lookup (subtask pref) lrefs of
            Just reflist -> (pref,appendRefs p reflist) -- WV17122008: never comes here so (subtask pref) is never in lrefs 
            _           -> (pref,p)

appendRefs :: GannetPacket -> [GannetSymbol] -> GannetPacket
appendRefs p reflist =
    let
        (gph,gppl) = p
        rootref=head $ reverse gppl
        reflist_norootref = filter (\gs->(gs{quoted=0}/=rootref{quoted=0})) reflist      
    in
        (gph{plength=(plength gph)+(length reflist_norootref)},gppl++reflist_norootref)      
        


unwrapPL :: State PacketList LambdaRefs -> PacketList -> (PacketList,LambdaRefs)
unwrapPL lst pl0 =
    let
        (State lf)=lst
        (lrefs,pl) = lf pl0
    in 
        (pl,lrefs)

updatePL :: PacketList -> SymbolTree -> GannetSymbol -> ScopeTable -> LambdaRefs -> Bool -> (LambdaRefs,PacketList)
updatePL pl st r scopes lrefs num = 
    let
        slh = getSLH st
        ref = label slh
        nref 
            | (rlambda slh >1) =ref{ext=1,lambda=rlambda slh} --WV: ext=1 indicates it's "tainted". But that should be redundant
            | otherwise = ref
        nnref=numerify nref num scopes
        lrefs1
            | to_apply slh /= False = appendLRef nnref lrefs
            | otherwise = lrefs
        p_to 
            | to_apply slh /= False = numServiceGL "apply" num
            | otherwise = numService (lto slh) num
        pn = case Hash.lookup r pl of
            Just p -> 
                let 
                    (nph,nppl) = p 
                    (nppl2,nref2,plen2)
                        | nppl == [] = ([],nnref,0)
                        | kind (last nppl)==K_Q = 
                            let
                                gss:rest=nppl
                                nppl1=gss{count=(count gss)-1}:rest
                            in
                                (init nppl1,nnref{quoted=1},(plength nph)-1)
                        | otherwise = (nppl,nnref,plength nph)
                in 
                    if last nppl2 /= nref2{quoted=1} 
                        then
                            (nph{plength=(plen2+1)},nppl2++[nref2])
                        else
                            (nph{plength=plen2},nppl2)
            Nothing -> (emptyGH,[nnref])
        pl2 = Hash.insert r pn pl
        p_ret = numServiceGL "gateway" num 
        -- WV12082008: this works, but why does numerify not work?
        refnamestr = (\ref -> let (GannetTokenL (GannetLabelS namestr)) = name ref in namestr) nref
--        refnamestr=show $ name nref
        nnref2=nref{name=numServiceGT refnamestr num}
        ph = MkGannetHeader P_code 0 0 0 p_to p_ret nullGS nnref        
    in
        (lrefs1,Hash.insert nnref (ph,[]) pl2)

appendPL :: SymbolTree -> GannetSymbol -> ScopeTable -> LambdaRefs -> Bool -> State PacketList LambdaRefs 
appendPL x r scopes lrefs num = State (\pl->(updatePL pl x r scopes lrefs num))        
        
{-
This one takes a PacketList and a key, and a GannetSymbol to be added at the
end of the packet payload
-}
updateP :: PacketList ->  SymbolTree -> GannetSymbol -> PacketList
updateP pl x r =
    let
        Symbol gs = x 
        pn= case Hash.lookup r pl of
                Just p -> 
                    let 
                        (nph,nppl) = p 
                        (nppl2,ngs,plen2)
                            | nppl == [] = ([],gs,0)
                            | kind (last nppl)==K_Q = 
                                let
                                    gss:rest=nppl
                                    nppl1=gss{count=(count gss)-1}:rest
                                in
                                    (init nppl1,gs{quoted=1},(plength nph)-1)                            
                            | otherwise = (nppl,gs,plength nph)
                        nppl3
                            | (kind ngs)==K_R && (ext ngs)==1 && (quoted ngs)==0 && (count ngs)==(subtask r) =
                                let
                                    gss:rest=nppl2                                    
                                in
                                    gss{count=(count gss)+1}:rest
                            | otherwise = nppl2
                        egs=extendGS ngs
                    in 
                        (nph{plength=(plen2+(length egs))},nppl3++egs)
                Nothing -> (emptyGH,extendGS gs)
    in
        Hash.insert r pn pl    

appendP :: SymbolTree -> GannetSymbol -> LambdaRefs -> Bool -> State PacketList LambdaRefs 
appendP x r lrefs num = State (\pl->(updateLRefs x r lrefs num,updateP pl x r))

updateLRefs ::  SymbolTree -> GannetSymbol -> LambdaRefs -> Bool -> LambdaRefs
updateLRefs  x r lrefs num
--    | kind gs == K_S && name gs == numServiceGT "lambda" num = Hash.insert (subtask r) r lrefs
     | kind gs == K_S && name gs == numServiceGT "lambda" num = Hash.insert (subtask gs) [] lrefs
     | otherwise = lrefs 
    where
        Symbol gs = x


appendLRef :: GannetSymbol -> LambdaRefs -> LambdaRefs      
appendLRef nref lrefs =
    case Hash.lookup (count nref) lrefs of -- looks like we use count to determine a lambda ref. But the .insert uses subtask
        Just reflist -> Hash.update (\v->Just (v++[nref{quoted=1}])) (count nref) lrefs
        Nothing ->  Hash.insert (count nref) [nref{quoted=1}] lrefs -- I don't like this: the entry should have been created already!

-- transform Symbol Tree into Packet List Monad
-- GannetSymbol (r in st_r) is the key in the PacketList hashtable 
                      
st2plm :: (SymbolTree,GannetSymbol) -> ScopeTable -> LambdaRefs -> Bool -> State PacketList LambdaRefs -- (SymbolTree,GannetSymbol)
st2plm st_r scopes lambdarefs num
    | length sl==0 = do return lambdarefs -- st_r
    | isSL x = -- it's a list of symbols
        do
            let
                rn = (numerify r num scopes)
            lrefs1 <- appendPL x rn scopes lambdarefs num
            lrefs2 <- st2plm (x,rn) scopes lrefs1 num
            st2plm st_r1 scopes lrefs2 num
    | otherwise = -- it's a symbol
        do                
            let 
                Symbol xgs = x 
            lrefs1 <- appendP (Symbol (numerify xgs num scopes)) r1l lambdarefs num
            st2plm st_r1 scopes lrefs1 num
    where
        (SymbolList st,r)=st_r
        (sl,slh)=st
        x:xs=sl
        st1=SymbolList (xs,slh)
        -- create a reference for the packet. 
        r1=numerify (label slh) num scopes
        r1l 
            | (rlambda slh >1) = r1{ext=1,lambda=rlambda slh}
            | otherwise = r1
        st_r1=(st1,r1l)

-- | Either turn a PacketList into a pretty-print string or into a bytecode string for writing to a .tdc file.        
gplToWords :: PacketList -> Bool -> String
gplToWords gpl numeric = 
    let 
        rootref = case Hash.lookup emptyGS gpl of 
            Just (ph,ppl) -> head ppl
            Nothing -> error $ "No rootref in "++show gpl
        (rph,rppl) = case Hash.lookup rootref gpl of 
            Just (ph,ppl) -> (ph,ppl)         
            Nothing -> error $ "Rootref "++show rootref++"not in "++show (Hash.toList gpl)
        rph2=rph{ptype=P_subtask}
        gpl1    =Hash.insert rootref (rph2,rppl) gpl    
        gpl2=Hash.delete emptyGS gpl1
        labels = Hash.keys gpl2
        npackets
            | numeric==True = intToBytes $ toInteger (length labels)
            | otherwise = "NPackets: "++show (length labels)
        strlist=    map (\label -> (gpToWords ((\label gpl2 -> case Hash.lookup label gpl2 of Just p -> p) label gpl2) numeric)) labels        
    in    
        npackets++(concat (concat strlist))
        
gpToWords :: GannetPacket -> Bool -> [String]
gpToWords gp numeric
    | numeric==True = bytecodize gp
    | otherwise = 
        let 
            (gph,gppl) = gp 
        in
             (show gph):(map addNL gppl)
--gpToWords (gph,gppl) = map gsToBC gppl
--gpToWords gp = bytecodize gp


addNL :: GannetSymbol -> String
addNL gs = (show gs)++"\n"

--gsToBC :: GannetSymbol -> [Char]
--gsToBC gs = bytecodize gs

{-
what we need to do is
-get value for label: numvals = map (\gt->(numData gt num scopes) labels
-convert label to numeric
-combine into string
-}
-- | Either turn the contents of the DataStore into a pretty-print string or into a string for writing to a .data file.
writeData :: Context -> String
writeData ctxt = 
    let
        datalist = datastore ctxt
        labels = Hash.keys datalist
        ndata = length labels        
        num = numeric ctxt
        scopes = scope ctxt
        numkeys = map (\gt->(numData gt num scopes)) labels
        datavals = map (\lb->(case Hash.lookup lb datalist of Just v -> v)) labels
        datapairs = zip numkeys datavals
        datapairstr 
            | num = foldl (\x y ->(x ++ (pair2str y))) ((show ndata)++"\n") datapairs
            | otherwise = show datalist
    in
        datapairstr


pair2str     :: (Integer,GannetBuiltin) -> String
pair2str (k,v)= (show k)++" "++(show v)++"\n"

{-
Strategy for emitting C from PacketList:
1. Emit function declarations
2. Emit the main function, i.r. run()
3. Emit the function definitions.

1. Function declarations:
If untainted, it's simply type Rx(); with type based on the Datatype field
If tainted, it's more difficult. What we need to do is find the parent lambda
and determine its signature. How? A possible way is via the apply call to the
lambda. But that might fail, as it might not end on leaf nodes.


-}


{-
 type PacketList = Hash.Map GannetSymbol GannetPacket
 type GannetPacket = (GannetHeader,[GannetSymbol])
for every packet in the list:
1. remap the reference
2. remap the return_as
3. remap and refs in the payload 
 
We need a datastructure Subtask->new subtask which we update on the fly . Should put it in Context?
-}
-- map old_subtask => (sid,new_subtask)
type SubtaskMap = Hash.Map Integer (Integer,Integer)

emptySM :: SubtaskMap
emptySM = Hash.empty

remapSubtasksPL :: PacketRefList -> Context -> SubtaskMap -> PacketRefList -> PacketRefList
remapSubtasksPL prl ctxt subtaskmap prl_r
    | length prl ==0 = prl_r
    | otherwise =
        let
            pp:prl2 = prl 
            (pp_r,ctxt2,subtaskmap2)=remapSubtaskP pp ctxt subtaskmap
        in
            remapSubtasksPL prl2 ctxt2 subtaskmap2 (pp_r:prl_r)


remapSubtaskP :: (GannetSymbol,GannetPacket) -> Context -> SubtaskMap -> ((GannetSymbol,GannetPacket),Context,SubtaskMap)
remapSubtaskP pp ctxt subtaskmap =
    let
        (pref,(ph,ppl))=pp
        (pref_r,ctxt2,subtaskmap2) = remapSubtask pref ctxt subtaskmap
        r_as = return_as ph        
        (r_as_r,ctxt3,subtaskmap3) = remapSubtask r_as ctxt2 subtaskmap2
        ph_r=ph{return_as=r_as_r}
        (ppl_r,nctxt,nsubtaskmap) = remapSubtaskPPL ppl ctxt3 subtaskmap3 []
        pp_r=(pref_r,(ph_r,ppl_r))
--        (pp_r,nctxt,nsubtaskmap)= ((pref_r,(ph,ppl)),ctxt2,subtaskmap2)
    in
        (pp_r,nctxt,nsubtaskmap)                                 

remapSubtaskPPL :: [GannetSymbol] -> Context -> SubtaskMap -> [GannetSymbol] -> ([GannetSymbol],Context,SubtaskMap)
remapSubtaskPPL ppl ctxt subtaskmap ppl_r
    | length ppl ==0 = (ppl_r,ctxt,subtaskmap)
    | otherwise =
        let
            p:ppl2 = ppl
            (p_r,ctxt2,subtaskmap2)            
                | kind p == K_R || kind p == K_C  = remapSubtask p ctxt subtaskmap
                | otherwise = (p,ctxt,subtaskmap)
        in
            remapSubtaskPPL ppl2 ctxt2 subtaskmap2 (ppl_r++[p_r])


-- remap the subtask using per-service address counters
                
remapSubtask :: GannetSymbol -> Context -> SubtaskMap -> (GannetSymbol,Context,SubtaskMap)
remapSubtask gs ctxt subtaskmap =
    if gs==emptyGS
    then (gs,ctxt,subtaskmap)
    else
        case Hash.lookup (subtask gs) subtaskmap of
            Just (sid,st_r) -> (gs{subtask=st_r},ctxt,subtaskmap)
            Nothing -> 
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
                    nsubtaskmap = Hash.insert (subtask gs) (sid,addr) subtaskmap
                in
                    (gs{subtask=addr},nctxt,nsubtaskmap)
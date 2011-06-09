{-# LANGUAGE CPP #-}

{- | The SystemConfiguration is the description of all the Services in the Gannet
System-on-Chip. Every service core can provide multiple services, these are accessed
via the Aliases.

WV08122010
The new approach is to use an object notation and model for the services. So instead of having aliases,
we now consider every service instance to be of some type Service, which is a kind of interface
that provides a number of methods. 

So, instead of (or rather, in addition to) looking up a "service" in the aliases, if the service instance name contains a
dot '.' we look up its type in the service instance record and use that to look up the method in the "Services" entry of the YML file


The SystemConfiguration YAML file contains the names and the numerical identifiers for all services.  
-}

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.SBA.SystemConfiguration (

newservices,
serviceids,
newaliases,
newinterfaces,
isFQN,
isTQN,
isAlias,
isServiceNode,
splitFQN,
splitTQN,
lookupFQN,
lookupFQNStrs,
lookupPQN,
cmpFQN,
lookupFQNIds,
--lookupNodeId,
lookupNodeIdFromFQN,
--lookupClasses,
--checkClass,
--lookupClassId,
--lookupMethodId, 
qualifyVarName,


) where
import qualified Data.Map as Hash
import Data.List
import Data.Maybe
--import Gannet.SBA.Constants 
import Gannet.SBA.Types
import Gannet.SBA.ConfigReader
import {-# SOURCE #-} Main ( ymlFileName )

type ServiceNodes = Hash.Map String (Integer,[String])
type ServicesNew = Hash.Map String (Integer,Integer)
type AliasesNew = Hash.Map String String
type InterfacesNew = Hash.Map String [String]


{-
With the new format for SystemConfiguration, we need a different lookup.
What do we need?
- the FQN for aliases, needed by Gannet/Symbolizer.hs
- The Service Node Id, needed for the packets, i.e for the packet header (to, return-to, return-as)
- The Service Class Id and the Opcode, needed for the service symbols
- check if a Service Node does provide an given Service Class
- check if a Service Class does provide an given Method

- For the 64-bit case, we need the FQN in the Name field!

Numerifier.hs
Symbolizer/Internals.hs
Symbolizer/InferTypes.hs
State/SymbolTree.hs
-}

isFQN mfqn = length (filter (\c -> (c == '.')) mfqn) == 2 
-- "Totally Qualified Name", i.e. including the library name
isTQN mtqn = length (filter (\c -> (c == '.')) mtqn) == 3

splitFQN mfqn  
    | isFQN mfqn = split '.' mfqn
    | otherwise = error $ "splitFQN: "++(show mfqn)++" is not an FQN"
    
splitTQN mfqn  
    | isTQN mfqn = split '.' mfqn
    | otherwise = error $ "splitTQN: "++(show mfqn)++" is not a TQN"
    
-- if it's not an FQN, it must be an alias
isAlias ma = Hash.member ma newaliases

lookupTQN = lookupFQN
lookupFQN name
    | isFQN name = 
        let 
            (snn,scln,scn,mn)=lookupFQNStrs name False 
        in
            snn++"."++scln++"."++scn++"."++mn
	| isTQN name = name
	| otherwise = lookupAlias name True
    
lookupAlias name noerror
    | name == "_" = "_" -- for empty symbols
    | otherwise =
        case Hash.lookup name newaliases of
            Just fqn -> fqn
            Nothing -> if noerror then "_._._._" else error $ "lookupAlias: Could not find a TQN for "++name++"\n"

lookupFQNStrs name noerror
    | name == "_" = ("_","_","_","_")
    | isFQN name = 
        let 
            snn:scn:mn:[] = splitFQN name 
            scln = lookupClassLibrary snn scn 
        in (snn,scln,scn,mn)
    | isTQN name = 
        let 
            snn:scln:scn:mn:[] = splitTQN name  
        in (snn,scln,scn,mn)
    | otherwise =
    	case Hash.lookup name newaliases of
            Just tqn -> 
                let
                    snn:scln:scn:mn:[] = splitTQN tqn  
                in 
                    (snn,scln,scn,mn)                 
            Nothing -> if noerror then ("_","_","_","_") else error $ "lookupFQNStrs: Could not find a FQN for <"++name++">\n"

-- this function takes an FQN and a string, and if they match, returns the string
lookupPQN mfqn noerror = 
    let
        (_,_,c,m)=lookupFQNStrs mfqn noerror 
    in
        (c,m)

-- for let, we must check if the fqn is of class LET and method let, and similar for lambda 
-- If we can't find the fqn, it's false
cmpFQN :: String -> String -> Bool
cmpFQN "'" alias = False
cmpFQN fqn alias =
    let
        (_,l1,c1,m1)=lookupFQNStrs fqn True
        (_,l2,c2,m2)=lookupFQNStrs alias True
    in
        l1==l2 && c1==c2 && m1==m2
                
isServiceNode fqn = Hash.member snn servicenodes where (snn,_,_,_) = lookupFQNStrs fqn True
  
lookupFQNIds name =
    let
        (nname,lname,scname,mname) = lookupFQNStrs name True 
    in
        if (nname,lname,scname,mname) ==  ("_","_","_","_")       
            then
                (0,0,0,0)
            else
                let                
                    snid =lookupNodeId nname
                    sclid=lookupSCLibId lname
                    scid =lookupClassId lname scname -- should be qualified with library name
                    opcode = lookupMethodId lname scname mname
                in
                    (snid,sclid,scid,opcode)

lookupNodeIdFromFQN name =
	let
		tqn
			| isTQN name = name
			| otherwise = lookupFQN name
		nname:_ = splitTQN tqn
	in
		lookupNodeId nname
		
lookupNodeId sn 
    | sn == "_" = 0
    | otherwise =
    	case Hash.lookup sn servicenodes of
    		Just (snid,_) -> snid
    		Nothing -> error $ "lookupNodeId: Could not find a Service Node for "++sn++"\n"
            
lookupClassLibrary sn scn 
    | sn == "_" = "NONE"
    | otherwise =
        let
            sclns = case Hash.lookup sn servicenodes of
                Just (snid,scls) -> scls
                Nothing -> error $ "lookupClassLibrary: Could not find a Service Node for "++sn++"\n"
            tmpl = filter (\t -> head t == scn ) (map (\scln -> split '.' scln) sclns)
            sclib = head (head tmpl)
        in
            sclib       
    

--lookupClasses sn 
--    | sn == "_" = []
--    | otherwise =
--    	case Hash.lookup sn servicenodes of
--    		Just (_,classes) -> classes
--    		Nothing -> error $ "lookupClasses: Could not find a Service Node for "++sn++"\n"
--		
--checkClass sn classname = classname `elem` (lookupClasses sn ) 
		
lookupClassId libname classname  
    | libname == "_" = 0
    | classname == "_" = 0
    | otherwise =
--    	case Hash.lookup classname newservices of
--    		Just (scid,ctrl) -> scid
--    		Nothing -> error $ "lookupClassId: Could not find a Service Class Id for "++classname++"\n"
        case Hash.lookup libname newservices of
            Just classes -> case Hash.lookup classname classes of
                    Just (scid,ctrl) -> scid
                    Nothing -> error $ "lookupClassId: No such classname "++classname++" in Interfaces\n"       
            Nothing -> error $ "lookupClassId: No such library name "++libname++" in Interfaces\n" 
			
lookupMethodId libname classname method  = 
	case lookup method (zip methods methodids) of
		Just mid -> mid
		Nothing -> error $ "No such method: "++method++" for Service Class "++classname++"\n"
	where
--		methods = case Hash.lookup classname newinterfaces of
--			Just ml -> ml
--			Nothing -> error $ "No such classname "++classname++" in Interfaces\n"
        methods = case Hash.lookup libname newinterfaces of
            Just classes->case Hash.lookup classname classes of
                    Just ml -> ml
                    Nothing -> error $ "lookupMethodId: No such classname "++classname++" in Interfaces\n"       
            Nothing -> error $ "lookupMethodId: No such library name "++libname++" in Interfaces\n" 
        methodids = take (length methods) (iterate (+1) 1) -- methods start at 1, so any 0 is an error
        
-- The danger with this is that it might try to qualify not only variables but also labels and buffers		
qualifyVarName :: GannetSymbol -> GannetSymbol
qualifyVarName v 
	| isFQN (getGSNameStr v) = v
	| otherwise =
		let
			vname = getGSNameStr v
			lettqn = lookupTQN "let"
			nname:sclname:scname:_ = splitTQN lettqn
			fqname=GannetTokenS (GannetLabelS (nname++"."++sclname++"."++scname++"."++vname))
		in
			v{name=fqname}		

servicenodes :: ServiceNodes
servicenodes = Hash.fromList servicenodelist
--(String, (Integer,[String]))
serviceids :: [Integer]
serviceids = map (\(sn,(snid,scl))->snid) servicenodelist

lookupSCLibId libname =
    case lookup libname (zip libraries sclibids) of
        Just sclid -> sclid
        Nothing -> error $ "No such library: "++libname++"\n"
    where
        sclibids = take (length libraries) (iterate (+1) 1)


newaliases = Hash.fromList newaliaslist -- [(alias,fqn)]
newservices = Hash.fromList (map (\t -> (fst t, Hash.fromList (snd t))) newservicelist)
newinterfaces = Hash.fromList (map (\t -> (fst t, Hash.fromList (snd t))) newinterfacelist)

--newservices = Hash.fromList newservicelist -- [ (libname,[(serviceclass, (serviceclassid,ctrl))]) ]
--newinterfaces = Hash.fromList newinterfacelist -- [ (libname,[(serviceclass,[method])]) ]

--alunames :: ALUNames
--alunames = Hash.empty





(libraries, servicenodelist, newaliaslist, newservicelist, newinterfacelist)= readSBAConfigNew ymlFileName -- defaults to SBA.yml

split :: Char -> [Char] -> [[Char]]
split delim str = words (map (\c-> (if delim==c then ' ' else c)) str)

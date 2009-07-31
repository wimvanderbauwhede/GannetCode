-- |This module provides integer transformations for various types.
-- a Bool flag is used throughout to toggle numerification.
module Gannet.Numerifier (
	numerify,
	numService,
	numServiceGL,
	numServiceGT,
	numGSK,
	numData,
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Scope
import qualified Data.Map as Hash

-- | Numerify the Name field of a GannetSymbol. 
numerify :: GannetSymbol -> Bool -> ScopeTable -> GannetSymbol
numerify gs num scopes 
	| num == False = gs
	| kind gs == K_L || kind gs == K_A || kind  gs == K_D =      
        if isNumeric (name gs)
            then
                gs
            else
        		let            
        			numvar =
        				case Hash.lookup (subtask gs) scopes of
        					Just scoperec -> 
        						case Hash.lookup (name gs) (varmap scoperec) of 
        							Just (_,n) -> n
        							Nothing -> 0 --  ad hoc!
        					Nothing -> 0
        			numname = 	(GannetTokenL (GannetLabelI numvar))
        		in
        			gs{name=numname}
	| kind gs == K_S = 
		let
--			GannetTokenS sname = name gs

--			numvar = case Hash.lookup sname services of
--				Just snum -> snum
--				Nothing -> case Hash.lookup sname alunames of
--					Just alias -> case Hash.lookup alias aliases of
--						Just (_,snum) -> snum
--						Nothing -> -2
--					Nothing -> -1
--			numname = 	(GannetTokenL (GannetLabelI numvar))		
--			numname = GannetTokenS (numServiceS sname num)
            numname = testGT (name gs)
		in
			gs{name=numname} 			
    | kind gs == K_R =
		let
--			GannetTokenL sname = name gs
--			numname = GannetTokenL (numService sname num)
            numname = testGTR (name gs)
		in
			gs{name=numname} 			
	| otherwise = gs


isNumeric :: GannetToken -> Bool
isNumeric numname@(GannetTokenL (GannetLabelI sid)) = True
isNumeric name = False

testGT :: GannetToken -> GannetToken
testGT gl@(GannetTokenS sname) = GannetTokenS (numServiceS sname True)
testGT gl@(GannetTokenL sname) = GannetTokenL (numServiceS sname True)
--testGT gl@(GannetTokenB sname) = gl
--testGT gl@(GannetTokenQ sname) = gl

testGTR :: GannetToken -> GannetToken
testGTR gl@(GannetTokenS sname) = GannetTokenS (numServiceR sname True)
testGTR gl@(GannetTokenL sname) = GannetTokenL (numServiceR sname True)
--testGTR gl@(GannetTokenB sname) = gl
--testGTR gl@(GannetTokenQ sname) = gl


-- | Helper for numerification of Services	
-- Returns the service id if no aliases, the opcode otherwise.

{- This is the numerifier for K_S symbols
   So this one needs to be modified for Service Core ID and Opcode
   Basically:
    - if the name matches the service, opcode=0, scid from field
    - if the name does not match the service, then the opcode is taken from the alias and the scid from the corresponding field
-}
    
numServiceS :: GannetLabel -> Bool -> GannetLabel
numServiceS gl num
	| num == False = gl
	|otherwise =
		let
			GannetLabelS sname = gl
			numvar = case Hash.lookup sname services of
				Just (snum,scid) -> 0 + scid*c_SHIFT_SCId -- snum -- a service, not an alias
				Nothing -> case Hash.lookup sname alunames of
					Just alias -> case Hash.lookup alias aliases of
						Just (sname,sid,snum) -> numAlias sname snum -- snum -- WV09122008: must lookup scid and combine with snum!
						Nothing -> error $ "numServiceS: ALUname but no alias for " ++ (show gl) ++"\n"
					Nothing -> case Hash.lookup sname aliases of
							Just (aname,sid,snum) -> numAlias aname snum -- WV09122008: must lookup scid and combine with snum!
							Nothing -> error $ "numServiceS: No alias for" ++ (show gl) ++"\n"
		in
			GannetLabelI numvar

numAlias sname snum = 
    case Hash.lookup sname services of
        Just (_,scid) -> let opcode = snum + scid*c_SHIFT_SCId in opcode -- error $ "Opcode:"++(show opcode)++"\n"
        Nothing -> error $ "numAlias: No parent service " ++ (show sname) ++ " for alias "++ (show snum) ++ "\n"

-- | Helper for numerification of Services, for K_R  
-- Returns the Service ID. The difference with numService is that it does not test against GATEWAY 
numServiceR :: GannetLabel -> Bool -> GannetLabel
numServiceR gl@(GannetLabelI _) _ = gl
numServiceR gl num
    | num == False = gl
    |otherwise =
        let
            GannetLabelS sname = gl
            numvar = case Hash.lookup sname services of
                Just (snum,scid) -> snum -- a service, not an alias
                Nothing -> case Hash.lookup sname alunames of -- look if it's an ALU symbol
                    Just alias -> case Hash.lookup alias aliases of -- if so, return the "parent" service & look up
                        Just (sname,_,_) -> case Hash.lookup sname services of
                            Just (snum,scid) -> snum
                            Nothing -> error $ "numServiceR: Could not find the parent service for alias "++sname++"\n"
                        Nothing -> error $ "numServiceR: ALUname but no alias for " ++ (show gl) ++"\n"
                    Nothing -> case Hash.lookup sname aliases of -- it's not an ALU symbol. Is it an alias?
                            Just (sname,_,_) -> case Hash.lookup sname services of
                                Just (snum,scid) -> snum
                                Nothing -> error $ "numServiceR: Could not find the parent service for alias "++sname++"\n"
                            Nothing -> error $ "numServiceR: No alias for " ++ (show gl) ++ "\n" ++ (show services)
        in
            GannetLabelI numvar

	
-- | Helper for numerification of Services	
-- Returns the Service ID; returns "0" for the GATEWAY whatever the actuall ID

-- FIXME: returns the opcode for ALU symbols but the service for non-ALU aliases!?! 
-- It doesn't seem to matter as we use numServiceS for the opcode
numService :: GannetLabel -> Bool -> GannetLabel
numService gl@(GannetLabelI _) _ = gl
numService gl@(GannetLabelS _) num
	| num == False = gl
	|otherwise =
		let
            GannetLabelS sname = gl
            numvar 
                | sname=="GATEWAY" = 0
                | otherwise = case Hash.lookup sname services of
                    Just (snum,scid) -> snum -- a service, not an alias
                    Nothing -> case Hash.lookup sname alunames of -- ALU symbol?
                        Just alias -> case Hash.lookup alias aliases of 
                        	Just (_,_,snum) -> snum
                        	Nothing -> error $ "numService: ALUname but no alias for " ++ (show gl) ++"\n"
                        Nothing -> case Hash.lookup sname aliases of
                        		Just (sname,_,_) -> case Hash.lookup sname services of
                        			Just (snum,scid) -> snum -- a service, not an alias
                        			Nothing -> 	error $ "numService: Alias but no service for " ++ (show gl) ++"\n"						
                        		Nothing -> error $ "numService: No alias for " ++ (show gl) ++"\n"
		in
			GannetLabelI numvar

-- | Turn a Service name string into a GannetLabel
numServiceGL :: String -> Bool -> GannetLabel
numServiceGL s num=  numService (GannetLabelS s) num

-- | Turn a Service name string into a GannetToken
numServiceGT :: String -> Bool -> GannetToken
numServiceGT s num = GannetTokenL (numServiceGL s num)

-- | Numerify a Gannet symbol Kind
numGSK :: GSymbolKind -> Bool -> String
numGSK gsk num
	| num = show $ fromEnum gsk 
	| otherwise = show gsk	

-- | Numerify a Data label
numData :: GannetToken -> Bool -> ScopeTable -> Integer
numData gt num scopes
	| num == False = 0 -- rubbish!
	| otherwise =
		case Hash.lookup 0 scopes of
			Just scoperec -> 
				case Hash.lookup gt (varmap scoperec) of 
					Just (_,n) -> n
					Nothing -> 0 --  ad hoc!
			Nothing -> 0

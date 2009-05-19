{-
This is module provides functions to read in the SBA.yml configuration file
and convert to the format used by SBA/ServiceConfiguration.hs
    0: { GATEWAY: [0, sba_GATEWAY, 1,[1,1]], Addr: 12 }
Services
    Int: { String: [Int, String, Int], "Addr": Int }
    Service Id: { Service Core name: [Service Core Id, Core Function name, Nthreads,[T_init,T_proc_val]], 'Addr': Service NoC Address }
    
transforms into:    
    servicelist = [ (Service Core name, (Service Id, Service Core Id)), ... ]
    
Aliases
    String: [String, Int, Int]
    Alias Name: [Service Core name, Service Id, Opcode,[T_init,T_proc_val]]
    
transforms into:
    aliaslist = [ (Alias name,(Service Core name,Opcode)),... ]
    
    to lookup: Service Core Name => Service Core Id
    
ALU_Names
    String: String
    '+': 'ALU:plus'
    
-}


module Gannet.SBA.ConfigReader (
    readSBAConfig,
    parseYamlFile,
    getSystemYMaps,
    serviceYMapToList,
    aliasYMapToList,
    namesYMapToList
) where

import Data.Yaml.Syck
import Data.Char (toLower)
import System.IO.Unsafe

readSBAConfig cfgfile =
    let
        yml = unsafePerformIO $ parseYamlFile cfgfile
        (sm,am,nm) = getSystemYMaps yml
    in
        (flattenServiceList (serviceYMapToList sm),(aliasYMapToList am),(namesYMapToList nm))

getSystemYMaps yml = 
    let 
	    EMap ltry = (n_elem yml)
	    (_,hv)   = head ltry
	    EMap [nServices,serviceYMap, aliasYMap,namesYMap] = (n_elem hv)
	    (_,serviceYMap_v)=serviceYMap
	    (_,aliasYMap_v)=aliasYMap
	    (_,namesYMap_v)=namesYMap
    in        
	    (serviceYMap_v,aliasYMap_v,namesYMap_v)

ymlMapToList helper m =	    
    let
        ym = getYMap m
    in
       map (\mv->helper mv ) ym                                    

    
serviceYMapToList = ymlMapToList serviceYMapToList_helper
serviceYMapToList_helper (mk,mv)  =  
    let
        scids=getServiceCoreIdsH mv
        snames=getServiceNamesH mv
    in        
        zip snames (zip (replicate (length scids) (read (getYStr mk)::Integer)) scids ) 

flattenServiceList = flattenServiceList_helper []

flattenServiceList_helper fsl sl = 
    if length sl==0
        then fsl
        else 
            let
                hsl:tsl=sl
                nfsl=fsl++hsl
            in
                flattenServiceList_helper nfsl tsl

-- was: 0: [GATEWAY, 10, sba_GATEWAY, 1, 12], i.e. mv is a list
-- is now: 0: { GATEWAY: [10, sba_GATEWAY, 1,[t1,t2]], GATEWAY2: [11, sba_GATEWAY2, 1,[t1,t2]], Addr: 12 }, i.e. mv is a map
-- transform into: (GATEWAY, (0, 10)), (GATEWAY2, (0,11)), ... ] ; timing info is ignored

getServiceNamesH l  =   
    let
        sm = getYMap l -- [ ( GATEWAY, [10, sba_GATEWAY, 1]), (GATEWAY2, [11, sba_GATEWAY2, 1]), (Addr, 12) ]        
        smf = filter (\kv -> let (k,v)=kv in (getYStr k)/="Addr") sm
        smks = map (\mkv -> getYMapKey mkv) smf
    in
        map (\mk -> map toLower (getYStr mk)) smks        

getServiceCoreIdsH l = 
    let
        sm = getYMap l
        smf = filter (\kv -> let (k,v)=kv in (getYStr k)/="Addr") sm
        smvs = map (\mkv -> getYMapVal mkv) smf
    in
        map (\mv -> read (getYStr (head (getYSeq mv)))::Integer) smvs


getServiceName l  =   
    let
        sl = getYSeq l
    in
        map toLower $ head $ map (\lv -> getYStr lv) sl

getServiceCoreId l = 
    let
        sl = getYSeq l
        sls = map (\lv -> getYStr lv) sl
    in
        read (sls !! 1)::Integer
   
aliasYMapToList = ymlMapToList aliasYMapToList_helper
aliasYMapToList_helper (mk,mv)  = (map toLower (getYStr mk), getAliasPair mv)

-- 'plus': [ALU, 7, 9,[t1,t2]] => ("plus",("alu",7,9)) ; timing info is ignored 
getAliasPair l =
	let
        sl = getYSeq l
        sls = map (\lv -> getYStr lv) sl
    in
        (map toLower (sls !! 0), read (sls !! 1)::Integer, read (sls !! 2)::Integer) 

namesYMapToList = ymlMapToList namesYMapToList_helper
namesYMapToList_helper (mk,mv)  = (map toLower (getYStr mk), map toLower (getYStr mv))

getYMapKey (mk,mv) = mk
getYMapVal (mk,mv) = mv

getYMap m = 
    let
        EMap sm = (n_elem m)
    in
        sm

getYSeq l =
    let
        ESeq sl = (n_elem l)
    in
        sl

getYStr mk = 
    let 
        EStr k = n_elem mk
    in
        unpackBuf k


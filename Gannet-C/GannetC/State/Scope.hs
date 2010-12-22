-- | Types for storing scope and functions for manipulating them.
module GannetC.State.Scope (
    VarMap,
    ScopeTable,
    ScopeRecord(..),
    initScope,
    getScope,
    getGCTypefromScope,
    appendScope,
    emptyScope,
    emptyVarMap
) where

import GannetC.AST
import qualified Data.Map as Hash

--------------------------------------------------------------------------------
-- Lexical Scope Table
--------------------------------------------------------------------------------
-- |ScopeTable keeps a list of all vars in a scope
type ScopeTable = Hash.Map Integer ScopeRecord

{- | ScopeRecord stores
 
>    * the enclosing scope
>    * a flag indicating the LET is in a LAMBDA
>    * a map from the symbol's name to the actual symbol and the corresponding number
 
-}    
data ScopeRecord = MkScopeRecord {     
                    enclosing :: Integer, -- ^ Points to the enclosing block
                    isinlambda :: Integer, -- ^ subtask count of enclosing LAMBDA
                    varmap :: VarMap -- a map from var name to type, contains all vars in this scope
                    }
instance Show ScopeRecord where show = showSR
showSR sr = (show (enclosing sr)) ++ " " ++ (show (varmap sr)) ++ "\n"
                   
initScopeRec :: Integer -> Integer -> ScopeRecord
initScopeRec enclosing isinlambda = MkScopeRecord enclosing isinlambda emptyVarMap
                                        
type VarMap = Hash.Map String GCType

emptyVarMap :: VarMap
emptyVarMap = Hash.empty

-- |Returns the type for a lexically scoped variable

getGCTypefromScope :: String -> Integer -> ScopeTable -> Integer -> GCType
getGCTypefromScope var currentscope scopes niters =
    case Hash.lookup currentscope scopes of
        Just scoperec -> 
            case Hash.lookup var (varmap scoperec) of 
                Just gct -> gct -- OK, found the var in this scope
                Nothing -> if enclosing scoperec == 0 -- at top level
                                then
                                    GCYada 
                                else -- recurse
                                if niters>0 then
                                        getGCTypefromScope var (enclosing scoperec) scopes (niters-1)
                                    else error $ "Deep recursion in scope table: "++(show scopes)++"\n" 
        Nothing -> error $ "Scope "++(show currentscope)++" not in scopes.\n" 

-- |Returns the actual scope of the var -- and the lambda of the var
getScope :: String -> Integer -> ScopeTable -> Integer -- (Integer,Integer)
getScope var currentscope scopes =
    case Hash.lookup currentscope scopes of
        Just scoperec -> 
            if (Hash.member var (varmap scoperec)) 
                then currentscope -- (currentscope, (isinlambda scoperec))
                else getScope var (enclosing scoperec) scopes
        Nothing -> 0 -- (0,0)

{- | The scope table must be initialized with an "empty" record to keep
track of the enclosing scopes. This is required for LET without ASSIGN
-}
initScope :: Integer -> Integer -> Integer -> ScopeTable -> ScopeTable
initScope currentscope enclosingscope inlambda scopes = 
    case Hash.lookup currentscope scopes of
            Just scoperec -> scopes
            Nothing -> 
                let
                    initscoperec= initScopeRec enclosingscope inlambda
                in
                    Hash.insert currentscope initscoperec scopes                        

{- | appendScope adds a ScopeRecord to the ScopeTable. 
It constructs the ScopeRecord from 
    String varname ,GCType vartype
    Integer currentscope
    Integer enclosingscope
    Integer inlambda    
and maps it to Integer currentscope
-}     

appendScope :: String -> GCType -> Integer -> Integer -> Integer -> ScopeTable -> ScopeTable
appendScope varname vartype current enclosing inlambda scopes = let
    cscoperec=
        case Hash.lookup current scopes of
            Just scoperec ->
                let
                    cvarmap= Hash.insert varname vartype (varmap scoperec)
                in                    
                      scoperec{varmap=cvarmap,isinlambda=inlambda}
            Nothing -> 
                let
                    cvarmap= Hash.singleton varname vartype
                in
                    MkScopeRecord enclosing inlambda cvarmap
    in
        Hash.insert current cscoperec scopes                        
        
emptyScope :: ScopeTable
emptyScope = Hash.empty 



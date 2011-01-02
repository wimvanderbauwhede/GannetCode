-- | Types for storing scope and functions for manipulating them.
module GannetPerl.State.Scope (
    VarMap,
    ScopeTable,
    ScopeRecord(..),
    initScope,
    getScope,
    getGPTypefromScope,
    getExprsFromScope,
    appendScope,
    emptyScope,
    emptyVarMap
) where

import GannetPerl.AST
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
-- WV27122010: for proper type inference, this should become
--type VarMap = Hash.Map String (GPType,[Expr])
-- with Expr the list of nodes bound to the var 
-- it is a list because vars are updateable                                          
type VarMap = Hash.Map String (GPType,[Expr])

emptyVarMap :: VarMap
emptyVarMap = Hash.empty

-- |Returns the type for a lexically scoped variable

getGPTypefromScope :: String -> Integer -> ScopeTable -> Integer -> GPType
getGPTypefromScope var currentscope scopes niters =
    case Hash.lookup currentscope scopes of
        Just scoperec -> 
            case Hash.lookup var (varmap scoperec) of 
                Just (gct,el) -> gct -- OK, found the var in this scope
                Nothing -> if enclosing scoperec == 0 -- at top level
                                then
                                    GPYada 
                                else -- recurse
                                if niters>0 then
                                        getGPTypefromScope var (enclosing scoperec) scopes (niters-1)
                                    else error $ "Deep recursion in scope table: "++(show scopes)++"\n" 
        Nothing -> error $ "Scope "++(show currentscope)++" not in scopes.\n" 

getExprsFromScope :: String -> Integer -> ScopeTable -> Integer -> [Expr]
getExprsFromScope var currentscope scopes niters =
    case Hash.lookup currentscope scopes of
        Just scoperec -> 
            case Hash.lookup var (varmap scoperec) of 
                Just (gct,el) -> el -- OK, found the var in this scope
                Nothing -> if enclosing scoperec == 0 -- at top level
                                then
                                    [] 
                                else -- recurse
                                if niters>0 then
                                        getExprsFromScope var (enclosing scoperec) scopes (niters-1)
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
    String varname ,GPType vartype
    Integer currentscope
    Integer enclosingscope
    Integer inlambda    
and maps it to Integer currentscope
-}     

appendScope :: String -> (GPType,[Expr]) -> Integer -> Integer -> Integer -> ScopeTable -> ScopeTable
appendScope varname (vartype,el) current enclosing inlambda scopes = let
    cscoperec=
        case Hash.lookup current scopes of
            Just scoperec ->
                let
                    cvarmap= Hash.insert varname (vartype,el) (varmap scoperec)
                in                    
                      scoperec{varmap=cvarmap,isinlambda=inlambda}
            Nothing -> 
                let
                    cvarmap= Hash.singleton varname (vartype,el)
                in
                    MkScopeRecord enclosing inlambda cvarmap
    in
        Hash.insert current cscoperec scopes                        
        
emptyScope :: ScopeTable
emptyScope = Hash.empty 



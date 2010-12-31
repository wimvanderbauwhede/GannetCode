-- | State information required for compilation
module GannetC.State.Context ( 
	Context(..), 
	currentScope,
	enclosingScope,
	emptyContext,
	emptyM
) where

import GannetC.State.Scope
import GannetC.AST

import qualified Data.Map as Hash

data Context = MkContext { 
						currentscope::Integer, -- ^ LET block subtask count
						scopestack::[Integer], -- ^ Stack of enclosing LETs
--						current::String, -- ^ Service name of current block
--						callerstack::[String], -- ^ Caller stack of current block 
--						currentLambda::Integer, -- ^ Subtask count of closest LAMBDA
--						lambdastack::[Integer], -- ^ Stack of enclosing LAMBDAs
--						scopetype::Int, -- ^ let=0,lambda=1
						scope::ScopeTable, -- ^ See "GannetC.State.Scope"
						isServiceDecl::Bool,
						currentService:: String,
						services::ServiceMap,						
						typeinfo::TypeInfoMap, -- Obsolete. For testing, we ignore the scope
						isConfigDecl::Bool,
						currentConfig::String,
						configs::[String],
						typedefs::TypeDefMap,
						configinstcount::Integer,
						configinsts::ConfigInstMap,
						handlecount::Integer,
						handles::HandleMap,
						count::Integer,
						stowedargs::StowedArgsMap						
						}						
-- rather than a field in the Context, use a special accessor
--enclosingScope ctxt = head (tail (scopestack ctxt))
enclosingScope ctxt = if length (scopestack ctxt) > 1 then head (tail (scopestack ctxt)) else 0
currentScope ctxt = if length (scopestack ctxt) > 0 then head (scopestack ctxt) else 0

-- | Initial values for Context 
emptyContext :: Context
emptyContext = MkContext 0 [0] emptyScope False "_" emptyS emptyTI False "_" [] emptyTD 1 emptyC 1 emptyH 0 emptySAM 
--emptyContext = (MkContext 0 [0] "_" ["GATEWAY"] 0 []  0 emptyScope False "_" emptyS emptyTI False "_" [] emptyTD 1 emptyC) 

instance Show Context where
	show ctxt =  
	   (showStack ctxt) ++ "\n\n" ++ (showScope ctxt) ++ "\n\n" ++ (showTypeInfo ctxt) ++ "\n\n" ++ (showTypeDefs ctxt)++ "\n\n" ++ (showServices ctxt)
	
showTypeInfo ctxt = "TypeInfo:\n"++(unlines $ map show (Hash.toList (typeinfo ctxt)))
showTypeDefs ctxt = "TypeDefs:\n"++(unlines $ map show (Hash.toList (typedefs ctxt)))
showStack ctxt = "Stack:\n"++(show (currentscope ctxt))++":"++(show (scopestack ctxt))	
showScope ctxt = "Scope:\n"++(unlines $ map show (Hash.toList (scope ctxt)))
showServices ctxt = "Services:\n"++(unlines $ map show (Hash.toList (services ctxt)))
-- to look up the "old type" based on the "new type"; the Bool indicates that the type is a Configuration
-- so Hash.insert (td_ntype td) ((td_otype td),(isConfigDecl ctxt)) (typedefs ctxt)
-- but also add the currentConfig to the namespace of the td_ntype, i.e. prepend to o_qtype

-- Services are currently global. Later, they might become local, if we introduces a VM service that can run other VM serices and so on
type ServiceMap = Hash.Map String MethodMap
emptyS :: ServiceMap
emptyS = Hash.empty

type MethodMap = Hash.Map String [GCType]
emptyM :: MethodMap
emptyM = Hash.empty

-- obsolete
type TypeInfoMap = Hash.Map String (GCType,Integer)
emptyTI :: TypeInfoMap
emptyTI = Hash.empty
 
-- Maybe typedefs should have local scope, so the TypeDefMap should be inside the Scope
-- Same for config instances and handles.
-- but just for now we keep them global
type TypeDefMap = Hash.Map GCType (GCType,Bool) 
emptyTD :: TypeDefMap
emptyTD = Hash.empty

type ConfigInstMap = Hash.Map String (GCType,Integer) 
emptyC :: ConfigInstMap
emptyC = Hash.empty

type HandleMap = Hash.Map String Integer 
emptyH :: HandleMap
emptyH = Hash.empty

type StowedArgsMap = Hash.Map Integer [ArgTup]
emptySAM :: StowedArgsMap
emptySAM = Hash.empty

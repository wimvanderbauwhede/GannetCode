{-# LANGUAGE Rank2Types #-}

module GannetC.TypeMapper (
    getTypeInfo,
    inferTypes,
    inferCtc
) where

import GannetC.AST
import GannetC.Emitter (emit)
import GannetC.State.Context
import GannetC.State.Scope

import qualified Data.Map as Hash
import Control.Monad.State
import Data.Generics 

-- Data.Generics does not provide a top-down everywhereM
everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x = do
						x' <- f x
						gmapM (everywhereM' f) x'

-- Data.Generics does not provide a depth-first everywhereM
everywhereM'' :: Monad m => GenericM m -> GenericM m -> GenericM m
everywhereM'' fi fo x = do
						x' <- fi x
						x'' <- gmapM (everywhereM'' fi fo) x'
						fo x''

getTypeInfo :: Program -> Context
getTypeInfo p =  execState (everywhereM'' (mkM getTypeInfoIn) (mkM getTypeInfoOut) p) emptyContext

inferTypes :: Program -> Context -> Program
inferTypes p ctxt = evalState (everywhereM' (mkM inferType) p) ctxt

{-
Inventory:

* Nodes that declare a typed variable:

data BindExpr=
	          BAssign AssignExpr => declares a variable
            | BFunDef FunDef => declares a variable bound to a function. If the argtype is Void or empty, it's a label

data DeclExpr = 
            | DInstDecl InstDecl => declares a service or config instance.

* Service and Configuration
              DConfigDecl ConfigDecl => sets a bit inConfigDecl and sets currentConfig on in*         
            | DServiceDecl ServiceDecl => sets a bit inServiceDecl and sets currentService on in*
* Typedefs:
            | BTypeDef TypeDef => adds to the typedefs

* Let block handling:
	inLet => increment counter, push on stack
	outLet => pop off stack

We can either do a single pass, filling the typeinfo table and looking up the types.
Or we can do the lookup in a separate pass. It is more generic, more flexible,
makes the code less cluttered - but it's slower
Still, I like it, as it means we only modify the Context in the first pass.
In the second pass we modify the AST.
-}
{--
Just like in SableCC, the In works on entering, the Out on leaving the node.
--}

getTypeInfoIn :: Expr -> State Context Expr
getTypeInfoIn x = do
	ctxt <-get			
	let
		nctxt = case x of
				BindE (BAssign a) -> inAssign a ctxt
				BindE (BFunDef f) -> inFunDef f ctxt
				DeclE (DInstDecl i) -> inInstDecl i ctxt
				PureE (PLet l) -> inLet ctxt
				DeclE (DConfigDecl cd) -> inConfigDecl cd ctxt
				DeclE (DServiceDecl sd) -> inServiceDecl sd ctxt
				DeclE (DOpDecl od) -> inOpDecl od ctxt
				BindE (BTypeDef td) -> inTypeDef td ctxt						
				otherwise -> ctxt
	put nctxt 
	return x
-- mainly we pop the stack here			
getTypeInfoOut :: Expr -> State Context Expr			
getTypeInfoOut x = do
	ctxt <- get
	let
		nctxt = case x of
				PureE (PLet l) -> outLet ctxt
				DeclE (DConfigDecl cd) -> outConfigDecl cd ctxt
				DeclE (DServiceDecl sd) -> outServiceDecl sd ctxt
				otherwise -> ctxt
	put nctxt 
	return x

-- Get types from declarations

addDecl:: String -> GCType -> Context -> Context
addDecl n t ctxt =
	let		
		current = currentscope ctxt
		enclosing = enclosingScope ctxt
		inlambda = 0
		ntypeinfo = Hash.insert n (t,current) (typeinfo ctxt)
		nscope = appendScope n t current enclosing inlambda (scope ctxt)
		nctxt=ctxt{typeinfo=ntypeinfo,scope=nscope}
	in
		nctxt			

inAssign :: AssignExpr -> Context -> Context
inAssign a ctxt = addDecl (a_name a) (a_type a) ctxt

inFunDef f ctxt =
	let
		fname = fd_name f
		rettype = fd_type f
		argtypes = case (fd_argstypes f) of
			[Void] -> [] 
			ts -> map (\(Arg x)->(at_type x)) ts 
		ftype = GCFunc $ MkFunc rettype argtypes
		nctxt=addDecl fname ftype ctxt		
	in
		nctxt
-- InstDecl can be Service or Configuration
inInstDecl i ctxt = 
	let
		instname = id_name i
		cfgname = case (id_type i) of
					GCObj (MkObj _ [cfgn,cfgm]) -> cfgn -- FIXME: later, namespaces can be nested, so this is not generic
					GCObj (MkObj _ [servicen]) -> servicen
					otherwise -> ""
		nctxt = if cfgname `elem` (configs ctxt) 
			then -- it's a configuration instance
				let
					nconfiginstcount = (configinstcount ctxt)+1
					nconfiginsts = Hash.insert instname ((id_type i),nconfiginstcount) (configinsts ctxt)
				in 
					ctxt -- FIXME! infinite loop here! { configinstcount=nconfiginstcount,configinsts=nconfiginsts}
			else --it's a service instance
				if cfgname/="IO"
					then ctxt
					else let
						nhandlecount=(handlecount ctxt) +1
						nhandles = Hash.insert instname nhandlecount (handles ctxt)	
					in
						ctxt{handlecount=nhandlecount,handles=nhandles}					
	in
		addDecl instname (id_type i) nctxt
						
-- Scope handling
-- Lambda is not handled at the moment

inLet ctxt = 
	let
		ncurr = (currentscope ctxt)+1
		nencl = head (scopestack ctxt)
		inlambda = 0
		nscope = initScope ncurr nencl inlambda (scope ctxt) 
		nscopestack = ncurr:(scopestack ctxt)
		nctxt = ctxt{currentscope=ncurr,scopestack=nscopestack,scope=nscope}
	in
		nctxt
						
outLet ctxt =
	let
		ncurrentscope:nscopestack = scopestack ctxt
		nctxt = ctxt{scopestack=nscopestack}		 
	in
		nctxt

-- Services, Configurations, typedefs

inConfigDecl cd ctxt = ctxt{isConfigDecl=True,currentConfig=(cd_name cd),configs=((cd_name cd):(configs ctxt))}
outConfigDecl cd ctxt = ctxt{isConfigDecl=False,currentConfig="_"}		

inServiceDecl sd ctxt =
	let
		nctxt = inLet ctxt
		nservices = Hash.insert (sd_name sd) emptyM (services nctxt)
	in	  
		nctxt{isServiceDecl=True,currentService=(sd_name sd),services=nservices} -- TODO: handle OpDecl!

outServiceDecl sd ctxt = 
	let
		nctxt = outLet ctxt
	in
		nctxt{isServiceDecl=False,currentService="_"}	
		
inOpDecl od ctxt = 
	let
		mmap = case Hash.lookup (currentService ctxt) (services ctxt) of 
					Just tmap -> tmap
					Nothing -> emptyM    
		nmmap = Hash.insertWith (++) (od_name od) [od_type od] mmap
		nservices = Hash.insert (currentService ctxt) nmmap (services ctxt)
	in
		ctxt{services=nservices}
				
inTypeDef td ctxt =
	let	 
		qntype 
			| isConfigDecl ctxt = addConfigNS (td_ntype td) (currentConfig ctxt)
			| otherwise = td_ntype td
		ntypedefs = Hash.insert qntype ((td_otype td),(isConfigDecl ctxt)) (typedefs ctxt)
		nctxt=ctxt{typedefs=ntypedefs}
	in
		nctxt		
-- we assume that a typedef in a Configuration has an GCObj or GCTemplObj type 
addConfigNS (GCObj (MkObj tq qt)) cfgns = GCObj $ MkObj tq (cfgns:qt)
addConfigNS (GCTemplObj (MkTemplObj tq qt ats)) cfgns = GCTemplObj $ MkTemplObj tq (cfgns:qt) ats
addConfigNS obj cfgns = obj

{-
Inferring types is particularly needed for variables, without it I think the
emitter can't work correctly. But of course it can also be used for type checking.

So we have:

PureE PVar
PureE  

In particular, I need to be able to emit a config call based on an UpdateExpr.
The UpdateExpr is a BindExpr. If the lhs is a service (and that is already tricky!)
and the rhs, which is a PureExpr, is a PVar, then we must look up the type of that var.
If that type is a typedef for something else and the isConfig bit is set, it is a Configuration
In that case we need to emit (config).
The important point here is that emit needs the Context
From the inferType point, all we need to do is populate the u_type field by lookup 
-}

inferType :: Expr -> State Context Expr
inferType  x = do
	ctxt <-get	
	let		
		x' = case x of
			BindE (BUpdate ue) -> inferUpdate ue ctxt 
			PureE (PVar v) -> inferVar v ctxt
			PureE (PFunAppl fa) -> inferFunAppl fa ctxt
--			PureE (PServiceCall MkServiceCall _ _ sc_args _ ) -> inferServiceCall sc ctxt
			_ -> x
	return x'

inferUpdate :: UpdateExpr -> Context -> Expr
inferUpdate ue ctxt = 
	let
		name = u_name ue
		niters = 100 -- Ad hoc!
		current = currentscope ctxt
		scopetable = scope ctxt
		-- look up the var name in the scope table
		ntype = getGCTypefromScope name current scopetable niters		
	in	
		BindE $ BUpdate $ MkUpdate (u_name ue) (u_rhs ue) ntype
		
inferVar :: Var	-> Context -> Expr
inferVar v ctxt =
	let
		name = v_name v
		niters = 100 -- Ad hoc!
		current = currentscope ctxt
		scopetable = scope ctxt
		-- look up the var name in the scope table
		ntype = getGCTypefromScope name current scopetable niters	
	in	
		PureE $ PVar $ MkVar (v_name v) ntype
		
--inferNothing x = PureE $ PVar $ MkVar "NOTHING" GCYada		
	 
inferFunAppl fa ctxt =	PureE $ PFunAppl fa 

--inferServiceCall sc ctxt = PureE $ PVar $ MkVar "SERVICE_CALL" GCYada

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Convert tree to annotate if a node is compile-time computable
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
* For every node, we need to figure out if it is CTC or not.
* Essentially, nodes that 
	- do not read data from the outside world
	- don't call rand()
	- and, crucially, don't contain (non-void) service calls!
	- So mostly this means no calls to ServiceCall IO.* and no calls to any declared services
* However, the main point of CTC is to be able to generate service instantiations and calls in loops,
e.g.

my @s; 
foreach my $i (1..$N) {
	my $s[$i] = new S($i);
}

We should not mark the foreach loop as non-CTC unless $i is non-CTC e.g. because $N is random
Of course, the problem is if folks do things like $i++ in the body, or they do break etc
So what we should do is basically for every update in the loop, check if the var being updated occurs in the Guard

- get the iterator from the guard
- check for updates (filter)
- check for updates 

Same for if(): if we can compute the condition at compile time, we can generate the correct branch
For while(), I think it is not possible as the predicate is definitely altered inside the loop

* If any child node is False, a node's CTC is False 
So, we go top-down; on In we set the node to True and descend; if it is False we should not descend;
on Out we check the child nodes

The problem here is that this will not work unless I change all relevant child nodes to Expr!

-}

inferCtc :: Program -> Context -> Program
inferCtc p ctxt = evalState (everywhereM'' (mkM inferCtcIn) (mkM inferCtcOut) p) ctxt

inferCtcIn :: Expr -> State Context Expr
inferCtcIn  (PureE x) = do
	ctxt <-get	
	let		
		ctc = case x of
			PServiceCall (MkServiceCall "IO" _ _ _ ) -> False
			PServiceCall (MkServiceCall "Math" "rand" _ _ ) -> False
			PServiceCall (MkServiceCall "Math" "srand" _ _ ) -> False
			PServiceCall (MkServiceCall srvc _ _ _ ) -> Hash.notMember srvc (services ctxt)
			otherwise -> True
	return (PureECtc x ctc)
inferCtcIn  (DeclE x) = return (DeclECtc x True)
inferCtcIn  (BindE x) = return (BindECtc x True) -- FIXME!

{- when climbing up, the decision is based on the CTC of the children.

I think the current version will not work as the CtcE wrapper operates on Expr, not on anything inside expr.

-}	
inferCtcOut :: Expr -> State Context Expr
inferCtcOut x' = do
	ctxt <- get	
	let 
		nctxt = case x' of
			BindECtc (BAssign (MkAssign vtype vname arhs)) _ -> 
				let
					cscope = currentscope ctxt
					scopet = scope ctxt 
					vscope = getScope vname cscope scopet			
					ctc = getCtc arhs
					nvtype = GCTypeCtc vtype ctc
					nscopet = appendScope vname nvtype cscope (enclosingScope ctxt) 0 scopet -- TODO: to be correct, inlambda should be extracted from the old record
				in
					ctxt{scope=nscopet}
			_ -> ctxt			
		ctc =
			let
				exprs = case x' of
				-- ASSIGN: set the CTC based on the RHS and store it
				-- in the scope table
					BindECtc (BAssign (MkAssign at an arhs)) _ -> [arhs] 
				-- LET: all exprs must be True
					PureECtc (PLet (MkLet _ lb _)) _ -> lb
				-- BEGIN: idem
					PureECtc (PBegin (MkBegin _ lb _)) _ -> lb
				-- IF: the predicate and both branches
					PureECtc (PCond (MkCond cp ct cf)) _ -> [cp,ct,cf]
				-- FOR: the guard and the body
					PureECtc (PFor (MkFor (MkGuard fi fc fm) fb)) _ ->  
						if hasIteratorUpdate x' then [PureECtc (PString "") False] else [fi,fc,fm]
				-- FOREACH: the value list, unless there's a update of the iterator. Then we pass a 		
					PureECtc (PForeach (MkForeach (PureE (PVar (MkVar itname ivt))) lv b)) _ -> 
						if hasIteratorUpdate x' then [PureECtc (PString "") False] else [lv]													
				-- WHILE: can never be CTC! 
					PureECtc (PWhile (MkWhile wp wb)) _ -> [PureECtc (PString "") False] -- [wp, wb]
				-- PFunAppl, PServiceCall: all args				
					PureECtc (PFunAppl (MkFunAppl _ fargs _)) _ -> fargs
					PureECtc (PServiceCall (MkServiceCall _ _ scargs _)) _ -> scargs
				-- PVar: Need to perform a lookup
					PureECtc (PVar (MkVar vname _)) _ ->
						let
							cscope = currentscope ctxt
							scopet = scope ctxt 
							vscope = getScope vname cscope scopet			
							vtype = getGCTypefromScope vname cscope scopet 500 -- ad hoc!							
							ctc = getCtcFromType vtype
						in
							[setCtc x' ctc]
					otherwise -> [] 					
			in			
				foldl (&&) True (map getCtc exprs)
	put nctxt				
	return (setCtc x' ctc)
	
getCtcFromType (GCTypeCtc gtype ctc) =ctc
getCtcFromType _ = False -- FIXME

getCtc (PureECtc x ctc) = ctc
getCtc _ = True

setCtc (PureECtc x ctc) ctc' = (PureECtc x ctc') 
setCtc x _ = x

-- Check for iterator updates in For and Foreach
hasIteratorUpdate :: Expr -> Bool
hasIteratorUpdate (PureE (PFor (MkFor (MkGuard fi fc fm) (PureE (PLet fb))))) = length (filter (\e -> (isIteratorUpdate (getIterator fi) e) ) (l_body fb)) > 0
hasIteratorUpdate (PureE (PForeach (MkForeach (PureE (PVar (MkVar itname ivt))) lv (PureE (PLet b))))) =  length (filter  (\e -> (isIteratorUpdate itname e)) (l_body b)) > 0
hasIteratorUpdate _ = False

getIterator :: Expr -> String
getIterator (BindE (BAssign (MkAssign ittype itname _))) = itname

isIteratorUpdate :: String -> Expr -> Bool
isIteratorUpdate itname (BindE (BUpdate (MkUpdate vname _ _))) = (vname == itname)
isIteratorUpdate itname (BindE (BOpUpdate (MkOpUpdate vname _ _ _))) = (vname == itname)
isIteratorUpdate _ _ = False
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Below here, old stuff, purely for reference
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

		
{-
NOT NEEDED?

walkAST :: Expr -> Context -> Context 
walkAST n ctxt = execState (everywhereM'' (mkM getTypeInfoIn) (mkM getTypeInfoOut) n) ctxt

getTypeInfoDesc :: Expr -> State Context Expr
getTypeInfoDesc x = do
					ctxt <-get
					let
						nctxt = case x of
--							PureE (PCond c) -> walkCond c ctxt
							PureE (PWhile c) -> walkWhile c ctxt
							PureE (PFor c) -> walkFor c ctxt
							_ -> ctxt
					put nctxt
					return xRotate
					
walkCond c ctxt = 
	let
		ctxt' = walkAST (PureE (c_pred c)) ctxt
		ctxt'' = walkAST (PureE (PLet (c_iftrue c))) ctxt'
		ctxt''' = walkAST (PureE (PLet (c_iffalse c))) ctxt''
	in
		ctxt'''	 
		
walkWhile w ctxt = ctxt
walkFor f ctxt = ctxt					
-}					
		
{--
The type mapper needs a data structure to keep track of all types.

map expression => type

Leaving lambda's for now, we need a table for variable name => type
and a table for scope -> variable name, i.e.:

On encountering a variable declaration (i.e. a Decl or Assign)
- determine scope
- every scope has a table variable => type. add an entry

On encountering a variable used in a Pure expression
- determine scope
- look up type
- annotate node with the type
=> This means every var node should have a type field

Once we know the type, we can typecheck the expressions; 
if that passes, we can use the type information in the emitter.
 
--}

-- this is not really useful as we have to walk the tree anyway
getDecls p = listify isDecl p 

isDecl (DeclE _) = True
isDecl _ = False


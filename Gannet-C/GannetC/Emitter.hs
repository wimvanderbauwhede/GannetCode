{-# LANGUAGE TemplateHaskell #-}
module GannetC.Emitter (
emit
) where

import Language.Haskell.TH
import GannetC.GenInstances (genInstances)
import GannetC.State.Context
import GannetC.State.Scope

import qualified Data.Map as Hash
import Control.Monad.State
import GannetC.AST
import GannetC.EmitG

-- the statement below generates instances for every node in the AST for the class and method defined in InstTypes
$(genInstances ''Program)

emitProgram :: Program -> State Context String
emitProgram (MkProg exprs) = do		
		estrs <- (mapM emit exprs) -- estrs::[String], should maybe be [[String]], then we concat and unlines
		return $ ";program\n" ++ (unlines (filter (\s->(s/="" && s/="'")) estrs))
-- instance EmitG Program where emit = emitProgram

emitExpr (BindE e) = emit e
emitExpr (PureE e) = emit e
emitExpr (DeclE e) = emit e
--emitExpr (PureECtc e _) = return . show 
--emitExpr (BindECtc e _) = return . show 
--emitExpr (DeclECtc e _) = return . show 
-- instance EmitG Expr where emit = emitExpr

--------------------------------------------------------------------------------
-- TODO

emitInstDecl = return . show
emitVarDecl = return . show  
emitInstAlloc = return . show 
emitServiceDecl = return . show
emitConfigDecl = return . show
emitTypeDef = return . show
emitOpDecl = return . show
-- for Perl
emitUseDecl = return . show
emitBool = return . show
--emitPair = return . show
-- MkPair {p_key:: Expr, p_value:: Expr, p_type::GCType}	deriving (Eq, Show, Typeable, Data)
{--
        ftype::GCType,
        fname::String,
        fargstypes::[FuncArg],
        fbody::Let
        
data FuncArg = Arg ArgTup
              | Void 
	deriving (Eq, Show, Typeable, Data)              
data ArgTup = MkArgTup 
    {
        at_type::GCType,
        at_name::String
    }              
	deriving (Eq, Show, Typeable, Data)
--}

emitFunDef fd 
	| (fd_argstypes fd) == [Void] = do
                estrs <- emit (fd_body fd)
                return $ emitCallQStr "label" [fd_name fd,estrs]
	| otherwise = do
                efargs <- (emitFuncArgs (fd_argstypes fd))
                efbody <- emit (fd_body fd)
                return $ emitCallQStr "label" [fd_name fd,emitCallBlockStr "lambda" [efargs, q efbody]]
					
emitFuncArgs argts = return $ unwords (map (\(Arg argt)->( "'"++ (at_name argt))) argts)

-- data LambdaDef = MkLambdaDef  { ld_type::GCType, ld_argstypes::[FuncArg], ld_body::[Expr] }
emitLambdaDef ld 
	| (ld_argstypes ld) == [Void] = do
                estrs <- emit (ld_body ld)
                return  estrs
	| otherwise = do
                efargs <- (emitLambdaArgs (ld_argstypes ld))
                efbody <- emit (ld_body ld)
                return $ emitCallStr "lambda" [efargs, q efbody]
					
emitLambdaArgs argts = return $ unwords (map (\(Arg argt)->( "'"++ (at_name argt))) argts)



emitFloat = return . show
emitInt = return . show

-- placeholders
emitOpUpdateExpr = return . show
{-
data For = MkFor { f_guard::Guard, f_body::Let }	
data Guard = MkGuard {f_init::Expr, f_cond::PureExpr, f_mod::Expr}

for (i=0;i<N;i++) {
[...]
}
-}

emitFor fl = do
	ctxt <- get
	f_init_str <- emit (f_init (f_guard fl))
	f_cond_str <- emit (f_cond (f_guard fl))
	f_mod_str <- emit (f_mod (f_guard fl))
	f_body_str <- emit (f_body fl)
	let
		unique_str = show (count ctxt)
		ncount = (count ctxt) + 1
		cond_true_str= emitCallBlockStr "let" [q f_body_str,q f_mod_str,emitCallQStr "return" [q ("FOR_L"++unique_str)]]
	put ctxt{count=ncount}
	return $ unlines $ -- ["(label FOR_L"++unique_str,"(if ",f_cond_str,"'"++cond_true_str,"'(return)","))"]
		emitCallBlock ("label FOR_L"++unique_str) $
			emitCallBlock "if" [f_cond_str, q cond_true_str, "'(return)"]
			
--  { w_pred::PureExpr, w_body::Let }	
emitWhile wl = do
	ctxt <- get	
	w_pred_str <- emit (w_pred wl)
	w_body_str <- emit (w_body wl)
	let
		unique_str = show (count ctxt)
		ncount = (count ctxt) + 1
		cond_true_str= unlines $ emitCallBlockQ ("return 'WHILE_L"++unique_str) [w_body_str] -- ["'(return 'WHILE_L"++unique_str,w_body_str,")"] -- 
	put ctxt{count=ncount}
	return $ unlines $
		emitCallBlock ("label WHILE_L"++unique_str) $
			emitCallBlock "if" [w_pred_str,cond_true_str,"'(return)"]
	
	
	
{-
How do we emit a foreach?

foreach my $i (1..$N) {
    $a+=$l[$i];
}

We have a separate service, called Range
Range:
    new
    done
    inc
    
(label foreachLXXX
(let
    '(assign 'foreachXXX (Range.new start stop))  
    '(if (Range.done foreachXXX) 
        '(return)
        '(let 
            '(assign 'i (Array.inc foreachXXX))
            '<body>, but strip any LET
            '(return 'foreachLXXX)
            )
     )
))

If the value list not a range, it must be an array; if we're smart we can even get rid of single-value loops
 
(label foreachLXXX
(let
    '(assign 'foreachXXX (Array.new <value list>))  
    '(if (Array.empty foreachXXX) 
        '(return)
        '(let 
            '(assign 'i (Array.shift foreachXXX))
            '<body>, but strip any LET
            '(return 'foreachLXXX)
            )
     )
))

MkForeach { fe_iterator::Expr, fe_list::Expr, fe_body::Expr }

fe_iterator is PureE $ PVal $ MkVal {v_name:: String, v_type:: GCType}

PServiceCall (MkServiceCall "Range" "new" rargs GCAny)
sc_name::String, sc_op::String, sc_args::[Expr], sc_type::GCType
-}	

emitForeach fl = do
	ctxt <- get
	body <- emit (fe_body fl)
	vl <-emit (fe_list fl)
	let
		iter = v_name ((\(PureE (PVar mkv))->mkv) (fe_iterator fl))		
		lbl = "foreach"++(show (count ctxt))
		srvc = sc_name (( \(PureE (PServiceCall mksrvc)) -> mksrvc ) (fe_list fl))	
		iterval
			| srvc == "Range" = emitCallStr "Range.inc" [lbl]
			| srvc == "Array" = emitCallStr "Array.shift" [lbl]
			| otherwise = error $ "Foreach list must either be Array or Range, not "++srvc
		itercond
			| srvc == "Range" = "Range.done"
			| srvc == "Array" = "Array.empty"
			| otherwise = error $ "Foreach list must either be Array or Range, not "++srvc	
		ncount = (count ctxt) + 1
	put ctxt{count=ncount}		
	return $ unlines $
		emitCallBlock ("label L_"++lbl) $ 
			emitCallBlock "let" (concat [							
					 emitCallQ "assign" [q lbl,vl] -- [,]  
					,emitCallBlockQ "if" (concat [
						 emitCall itercond [lbl] 
						,["'(return)"] -- emitCallQ "return" []						
						,emitCallBlockQ "let" (concat [ 
							 emitCallQ "assign" [q iter, iterval]
							,["'"++body]
							,emitCallQ "return" [q "L_"++lbl]
						])						
					])
				])
						

--------------------------------------------------------------------------------

emitPureExpr (PString s) = return $ show s
emitPureExpr (PRegex s) = return $ show s
emitPureExpr (PVar v) = emit v
emitPureExpr (PPair p) = emit p
emitPureExpr (PReturn rv) = emit rv
emitPureExpr (PNumber n) = emit n
emitPureExpr (PLet l) = emit l
emitPureExpr (PBegin b) = emit b
emitPureExpr (PCond c) = emit c
emitPureExpr (PFor fl) = emit fl
emitPureExpr (PForeach fl) = emit fl
emitPureExpr (PWhile wl) = emit wl
emitPureExpr (POpCall oc) = emit oc
emitPureExpr (PServiceCall sc) = emit sc
emitPureExpr (PFunAppl fa) = emit fa
emitPureExpr (PLambdaDef ld) = emit ld

emitPureExpr e = return $ "<pure " ++ show e ++">"
-- instance EmitG PureExpr where emit = emitPureExpr


emitLet l
	| length (l_body l) > 1 =
		let
        		maybequote 
        			| length (l_blocktype l) == 0 = ""
        			| head (l_blocktype l) == "seq" = "'"
        			| otherwise = ""	
		in 
			do
				elbody <- (mapM emit (l_body l))
				let
					eqlbody = map (\x->(maybequote++x)) elbody
				return $ emitCallBlockStr "let" (filter (\s->(s/="" && s/="'" && s/="';<decl>")) eqlbody) 
	| otherwise = emitMaybeBegin $ head (l_body l) -- but if this is a begin, it should become a let!
	
emitMaybeBegin e =	
	if (isBegin e) 
		then
			let
				PureE (PBegin be) = e 
				b2l = PureE $ PLet $ MkLet (bb_blocktype be) (bb_body be) (bb_type be)
			in
				emit b2l
		else
			emit e

isBegin (PureE (PBegin _)) = True
isBegin _ = False

emitBegin l
	| length (bb_body l) > 1 =
		let
        		maybequote 
        			| length (bb_blocktype l) == 0 = ""
        			| head (bb_blocktype l) == "seq" = "'"
        			| otherwise = ""	
		in 
			do
				elbody <- (mapM emit (bb_body l))
				let
					eqlbody = map (\x->(maybequote++x)) elbody
				return $ emitCallBlockStr "begin" (filter (\s->(s/="" && s/="'" && s/="';<decl>")) eqlbody ) 
	| otherwise = emit $ head (bb_body l)	
	
-- instance EmitG Let where emit = emitLet

emitCond c = do
        	epred <- (emit (c_pred c))
        	let
        		ift = (c_iftrue c)
        		iff = (c_iffalse c)
        	eiftrue <- (emit ift)
        	eiffalse <- (emit iff) 
        	return $ emitCallBlockStr "if" [epred ,(maybequote (emit_maybe_return ift eiftrue)) ,(maybequote (emit_maybe_return iff eiffalse))]
	where
		emit_maybe_return (PureE (PLet l)) el 
			| length (l_body l) ==1 && isVar (head (l_body l)) = emitCallStr "return" [el]
			| otherwise = el
		maybequote str@('\'':_) = str
		maybequote str = "'"++str   
-- instance EmitG Cond where emit = emitCond

emitOpCall oc  
	| (oc_name oc) /= "++" && (oc_name oc) /= "--" = do 
        	eocargs <- (mapM emit (oc_args oc))
        	return $ emitCallStr (oc_name oc) eocargs														
	| (oc_name oc) == "++" = do
		eocargs <- (emit (head (oc_args oc)))
		return $ emitCallStr "update" $ (q eocargs):(emitCall "+" [eocargs,"'1"])
	| (oc_name oc) == "--" = do 
		eocargs <- (emit (head (oc_args oc)))
		return $ emitCallStr "update" $ (q eocargs):(emitCall "-" [eocargs,"'1"])
-- instance EmitG OpCall where emit = emitOpCall

--emitFunAppl sc = return $ "(apply " ++(fa_fname sc)++" "++ args ++ ")"
--	where
--		args
--			| length (fa_args sc) > 0 =  (unwords (map emit (fa_args sc)) )
--			| otherwise = ""
			
emitFunAppl sc = do
	efaargs <- (mapM emit (fa_args sc))
	let			
		args
			| length (fa_args sc) > 0 =  (unwords efaargs )
			| otherwise = ""
	return $ emitCallStr "apply" [fa_fname sc,args]
-- instance EmitG FunAppl where emit = emitFunAppl

--emitServiceCall sc = return $ "(" ++(sc_name sc)++"."++ (sc_op sc) ++" "++ args ++ ")"
--	where
--		args
--			| length (sc_args sc) > 0 =  (unwords (map emit (sc_args sc)) )
--			| otherwise = ""

-- if the first arg is a Configuration, we must emit a confrun.
-- but maybe only if the method is "run"
--        (s1.confrun '3 '(config '3 '1)
--                    args
--                )
-- instead of
-- (s1.run args)
-- it's a configuration if it exists in the configinsts
-- 
-- for IO, test if the sc_name is in the handles map. If so, emit an IO call

emitServiceCall sc = do
	ctxt <- get	
	let
		mcstr = case length scargs of
			0 -> "_"
			otherwise -> let
							mc:oargs=(sc_args sc) -- does not work for empty arglist!
							mcstr =  case mc of
								PureE (PVar (MkVar vname vtype)) -> vname
								otherwise -> "_"
						in mcstr			
		maybeconf = case Hash.lookup mcstr (configinsts ctxt) of
			Just (t,c) -> q $ (show c)++(emitCallQStr "config" [q (show c),"'1"])
			Nothing -> ""
		meth
			| maybeconf == "" = (sc_op sc)
			| otherwise = "confrun "
		scargs 
			| maybeconf == "" = sc_args sc
			| otherwise = tail (sc_args sc)
	escargs <- mapM emit scargs
	let			
		args
			| length scargs > 0 = unwords escargs
			| otherwise = ""
		emitstr = case Hash.lookup (sc_name sc) (handles ctxt) of
					Just fh -> emitCallStr ("io." ++ (sc_op sc)) [q (show fh),args]				 
					Nothing -> emitCallStr ((sc_name sc)++"."++ meth) [maybeconf,args]
	return emitstr								
-- instance EmitG ServiceCall where emit = emitServiceCall

--emitVar = return . v_name
{-
If the var is of type Array or Hash, we must always emit
"(Array.get "++(v_name v)++")" 

-} 
emitVar v = do
	ctxt <- get
	let
		typestr = (show (v_type v))++":"
		evstr = case Hash.lookup (v_name v) (configinsts ctxt) of 
				Just (t,c) -> show c
				Nothing ->  if isBuf (v_type v)
						then "(get '"++(v_name v)++")" 
						else  emitVarByType (v_type v) (v_name v)
--							case (v_type v) of
--								scalarType -> (v_name v)
--								arrayType ->  "(Array.get "++(v_name v)++")" 
--								hashType -> "(Hash.get "++(v_name v)++")" 								
	return evstr
arrayType = GCTemplObj (MkTemplObj [] ("Array":[]) [TArgT GCAny])
hashType = GCTemplObj (MkTemplObj [] ["Hash"] [TArgT GCAny])
emitVarByType (GCTemplObj (MkTemplObj [] ("Array":[]) [TArgT GCAny])) vstr = "(Array.get "++vstr++")"
emitVarByType (GCTemplObj (MkTemplObj [] ("Hash":[]) [TArgT GCAny])) vstr = "(Hash.get "++vstr++")"
emitVarByType _ vstr = vstr 
						 
emitNumber (NInt i) = return $ "'"++(show i)
emitNumber (NFloat f) = return $ "'"++(show f) 
-- instance EmitG Number where emit = emitNumber


emitPair p = do
	k <- emit (p_key p)
	v <- emit (p_value p)
	return $ unwords [k,v]
--------------------------------------------------------------------------------

emitBindExpr (BAssign e) = emit e
emitBindExpr (BUpdate e) = emit e
emitBindExpr (BTypeDef e) = return $ ""
emitBindExpr (BFunDef e) = emit e
emitBindExpr e = return $ "<bind " ++ show e ++">"
-- instance EmitG BindExpr where emit = emitBindExpr


-- for my @a=@b, we must emit (assign 'a (Array.new (Array.get b) ) )
emitAssignExpr e = do
--		ctxt <- get
		eatype <- (emit $ a_type e)
		earhs <- (emit $ a_rhs e)
		let			
			assign_or_label 
				| isFuncType (a_type e) = "label"
				| isBuf (a_type e) = "buf"
				| otherwise = "assign"
			maybe_q str
				| isFuncType (a_type e) = str
				| isBuf (a_type e) = q str
				| otherwise = q str
			typestr = "" -- (show (a_type e)) ++":"				
		return $ emitCallStr (assign_or_label ++ typestr) [maybe_q $ a_name e, maybeCtor e earhs]

maybeCtor e earhs 
	| a_type e == arrayType && isRange (a_rhs e) = emitCallStr "Array.fromRange" [earhs]
	| a_type e == arrayType = emitCallStr "Array.new" [earhs]
	| a_type e == hashType = emitCallStr "Hash.new" [earhs]
	| otherwise = earhs

isRange (PureE (PServiceCall (MkServiceCall "Range" _ _ _))) = True
isRange _ = False	
-- instance EmitG AssignExpr where emit = emitAssignExpr

-- s1=rot; => (s1.reconf '3 (config '3 '1)) 
-- provided that rot maps to 3
-- so we must have a map of the configuration instances to numbers
emitUpdateExpr e = do
	ctxt <- get
	eurhs <- (emit $ u_rhs e)
	-- now look up the type of u_name
	let
		sinst= u_name e
		sname = case u_type e of
					GCObj (MkObj _ (s:[])) -> s
					otherwise -> "data"
		typestr=""									
	-- then check if it's a service
		op = case Hash.lookup sname (services ctxt) of
				Just s -> emitCallStr (sinst++".reconf") [ q eurhs, emitCallStr "config" [q eurhs,"'1"]]
				Nothing -> emitCallStr "update" [" '" ++ typestr ++sinst,eurhs]					
	return op
-- instance EmitG UpdateExpr where emit = emitUpdateExpr

--------------------------------------------------------------------------------
-- if the Decl is an IO service, it is considered an io.fopen()
-- so we must emit it
--emitDeclExpr (DInstDecl e) = emit e
emitDeclExpr _ = return $ ";<decl>"
-- instance EmitG DeclExpr where emit=emitDeclExpr

--------------------------------------------------------------------------------
emitGCType t = return $ "" -- ":"++(show t)    
instance EmitG GCType where emit = emitGCType


isVar (PureE (PVar _ )) = True
isVar (PureE (PReturn (PVar _ ))) = True
isVar _ = False

isFuncType (GCFunc _) = True
isFuncType _ = False		 

isBuf (GCTemplObj (MkTemplObj _ ["Buf"] _ )) = True
isBuf _ = False

-- a utility function to quote expression strings
q str = "'"++str
-- a utility function to emit a Gannet service call given the service name and a list of arguments
emitCall :: String -> [String] -> [String]
emitCall srvc args = [ "("++(unwords (srvc:args))++")" ]
emitCallQ :: String -> [String] -> [String]
emitCallQ srvc args = [q (head (emitCall srvc args))]
emitCallStr srvc args = head $ emitCall srvc args
emitCallQStr srvc args = head $ emitCallQ srvc args


--emitCallBlock srvc args = "("++srvc++"\n"++(unlines (map (\str->"\t"++str) args))++")"
emitCallBlock :: String -> [String] -> [String]
emitCallBlock srvc args = ["("++srvc]++(map (\str->("  "++str)) args)++[")"]

emitCallBlockQ :: String -> [String] -> [String]
emitCallBlockQ srvc args = (q (head (emitCallBlock srvc args))):(tail (emitCallBlock srvc args))

emitCallBlockStr srvc args = unlines $ emitCallBlock srvc args
emitCallBlockQStr srvc args = unlines $ emitCallBlockQ srvc args




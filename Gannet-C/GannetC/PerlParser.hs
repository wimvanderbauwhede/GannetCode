{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}

{--
TODO
----
How do I indicate that the end of a block indicates the end 
of an expression? 
update operators (+=, *= etc) not yet supported


--}

module GannetC.PerlParser (
	prettyGannetPerlFromFile,
	parseGannetPerl
) where

import GannetC.AST

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
--import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 

-- -- WV: Can't get this to work ...
--import Text.Parsec hiding (State)
--import Text.Parsec.Expr
----import Text.ParserCombinators.Parsec.Char
--import qualified Text.Parsec.Token as P
--import Text.Parsec.Language

prettyGannetPerlFromFile fname = do
	input <- readFile fname
--	-- here I could "patch" the input
--	let
--		input' = input	
	putStr input
	case parse program fname input of
		Left err -> do
			putStr "parse error at "
			print err            
		Right (MkProg x)  -> putStrLn $ unlines (map show x)
      
--patch input =
--let
--	srclines = lines input
--	srclines' = map patchIfMatch srclines   
--in
--  	unlines srclines'
--  	    
--patchIfMatch line =
--	let
--		line' = line		
--	in  	    
--		line'
		
parseGannetPerl input =  case parse program "" input of
           Left err -> MkProg [PureE $ PString $ "parse error at " ++ show err]
           Right val  -> val
            

-- type Parser a = GenParser Char () a
-- type CharParser st a = GenParser Char st a, i.e. a parser for character streams
-- with user state st.
-- TODO: For simplicity we assume a ';' -separated list; but semicolons should be optional
-- after blocks 
-- we do not allow empty programs
program :: Parser Program
program = whiteSpace >> semiSepEnd1 expr >>= \exprs -> return $ MkProg exprs
--program = many1 expr >>= \exprs -> return $ MkProg exprs

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

exprNoSep :: Parser Expr
exprNoSep  =   
            (try bindExpr)
		<|> (liftM DeclE (try declExpr))
--		<|> (liftM BindE (try bindExpr)) 
		<|> (liftM PureE pureExpr)
		<?> "expression"

--exprSep = do 
--			expr <- exprNoSep 
--			semi
--			return expr
			
--expr =  (try exprSep) <|> exprNoSep

expr =  exprNoSep
--------------------------------------------------------------------------------
--
-- PureExpr
--
--------------------------------------------------------------------------------

atomicPureE :: Parser PureExpr
atomicPureE = 	     		 
            numberExpr
        <|> boolExpr     
        <|> stringExpr 
        <|> returnExpr
        <|> try arrayIndex
        <|> try instAlloc
        <|> try serviceCall 
        <|> try funAppl    
		<|> try bareCommand    
        <|> liftM PLet letExpr
        <|> liftM PBegin beginExpr
        <|> condExpr
        <|> whileExpr 
        <|> forExpr
        <|> foreachExpr -- FIXME: allow "for" as "foreach"  
        <|> liftM PLambdaDef lambdaDef 
        <|> varExpr 
        <|> parens pureExpr
		<?> "atomicPureE"

instAllocArgs = do
        reserved "new"
        insttype <- oldType
        instargs <- parens (commaSep pureExpr)
        return $ PInstAlloc ( MkInstAlloc insttype (map PureE instargs))

instAllocNoArgs = do
        reserved "new"
        insttype <- oldType
        return $ PInstAlloc (MkInstAlloc insttype [])
    				
instAlloc = try instAllocArgs <|> instAllocNoArgs <?> "instAlloc"

funAppl = do
				fname <- identifier
				args <- parens (commaSep pureExpr)
				return $ if fname/="shift" -- TODO: make generic!
							then PFunAppl (MkFunAppl fname (map PureE args) GCAny)
							else PServiceCall (MkServiceCall "Array" fname (map PureE args) GCAny) 
				
bareCommand = do
				cname <- command
				return $ commandAppl cname				

bareCommandArgs = do
				cname <- command
				args <- commaSep pureExpr
				return $ commandAppl cname				

command =
				symbol "shift" 
			<|> symbol "pop"
			<|> symbol "scalar"
			<|> symbol "push"
			<|> symbol "unshift"
			<|> symbol "print"
			-- and many, many more!
			<?> "commands"			
{-
Commands belong mostly to special objects:
IO:
	print
Array:
	push
	pop
	shift
	unshift
	scalar	
Hash:
	keys
	exists
So we need to construct the corresponding object call, basically
by a reverse lookup of the object
	
-}												
commandAppl cname = PServiceCall (MkServiceCall cobj cname [cvar] GCAny)
	where
	-- FIXME: make this more generic!
		cobj="Array"
		cvar = PureE (PVar (MkVar "_" arrayType))
		  												
serviceCall = do
				inst <- identifier
				arrow
				meth <- identifier
				args <- parens (commaSep pureExpr)
				return $ PServiceCall (MkServiceCall inst meth (map PureE args) GCAny) 
-- $a[e1][e2]... -> (Array.at (Array.at a e1) e2)				
arrayIndex = do
			(vartype,varname) <- varIdentifierPair	
			idxs <- many1 (brackets pureExpr)
			let
				inst = parseArray (PVar (MkVar varname vartype)) idxs	
			return inst				

parseArray var idxs 
	| length idxs>1 = foldl (\a ie -> (PServiceCall (MkServiceCall "Array" "at" [PureE a, PureE ie] GCAny))) var idxs
	| otherwise = 	PServiceCall (MkServiceCall "Array" "at" [PureE var, PureE (head idxs)] GCAny)					


arrow = symbol "->"
        
blockType =  keyword "par" <|> keyword "seq"       
letExpr = do
			bt <- many blockType
			exprs <- braces exprList
			let
				exprs' = processSeq exprs []			
			return $ MkLet bt exprs' GCAny
			
processSeq el el' 
    | length el==0 = el'
    | otherwise =
        let
            x:xs=el        
            (x',xs') = if isSeq x
                then
                    let
                       (exprs,xs') = findNoSeq xs
                    in
                       (PureE (PBegin (MkBegin ["seq"] exprs GCAny)),xs')
                else (x,xs)
        in
            processSeq xs' (el'++[x'])
            
findNoSeq el = (takeWhile notNoSeq el,dropWhile notNoSeq el)

notNoSeq (DeclE (DUseDecl (MkUseDecl False "seq"))) = False
notNoSeq _ = True

isSeq  (DeclE (DUseDecl (MkUseDecl True "seq"))) = True
isSeq _ = False  			
				
beginExpr = do
			exprs <- parens (commaSep1 expr)
			return $ MkBegin [] exprs GCAny		

lambdaDef = do
			reserved "sub"
			fbody <- letExpr
--			exprs <- braces exprList
			let
				exprs = l_body fbody
				--(args,exprs') = getArgs exprs PVar
				args 
					| hasArgs exprs = [ Arg (MkArgTup arrayType "_" ) ]
					| otherwise = []
			return $ MkLambdaDef GCAny args (PureE (PLet fbody)) 						        

{-
I just realised this is silly: simply use an array as the lambda arg 
and it all works!

-
my @args = @_; OK 
(my $a1, my $a2) =  @_; needs support for list assignment
-> means a list should be allowed on LHS of a bind expression
-> the result should be a list of assignments, in order of occurence

my $a1 = shift(@_); OK 
my $a2 = $_[1]; OK
my @a2 = pop; needs support for bare commands, more or less OK
-

getArgs exprs = 
	let
		arg_exprs = filter isArgExpr exprs
		other_exprs = filter (not . isArgExpr) exprs
		args = processArgExprs arg_exprs
	in
		(args,other_exprs)
-}		
-- This assumes that folks first assign the implicit args. 
-- If they don't, the expression could be just about anything ...
-- Still, what is most likely, appart from assign? update, opupdate, servicecall, funappl

-- shift, shift(@_), pop, pop(@_) 
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PServiceCall (MkServiceCall "Array" "shift" [PureE (PVar (MkVar "_" arrayType))] _)))))) = True
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PFunAppl (MkFunAppl "shift" [PureE (PVar (MkVar "_" arrayType))] GCAny)) )))) = True
-- to support pop we need a temp stack of the pop'ed values. 
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PServiceCall (MkServiceCall "Array" "pop" [PureE (PVar (MkVar "_" arrayType))] _)) )))) = True
-- $_[...] but this will only work if the index expression is a constant
-- also, we need to deal with out-of-order indexing, by building a list of pairs and sorting them. 
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PServiceCall (MkServiceCall "Array" "at" [PureE (PVar (MkVar "_" arrayType)),_] _) ))))) = True
-- @_
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PVar (MkVar "_" arrayType))) ))) = True
isArgExpr _ = False

hasArgs exprs = length (filter isArgExpr exprs) > 0

{-
processArgExpr (BindE (BAssign (MkAssign vtype vname (PServiceCall (MkServiceCall "Array" "shift" [PureE (PVar (MkVar "_" arrayType))] _))))) idx = (vtype,vname,idx+1)
processArgExpr (BindE (BAssign (MkAssign vtype vname (PFunAppl (MkFunAppl "shift" [PVar (MkVar "_" arrayType)] GCAny)) ))) idx = (vtype,vname,idx+1)
--processArgExpr (BindE (BAssign (MkAssign vtype vname (PServiceCall (MkServiceCall "Array" "pop" [PureE (PVar (MkVar "_" arrayType))] _))))) = (vtype,vname,idx-1) 
--processArgExpr (BindE (BAssign (MkAssign vtype vname (PServiceCall (MkServiceCall "Array" "at" [PureE (PVar (MkVar "_" arrayType)),_] _))))) 
--	| idx==0 = (vtype,vname,idx)
--	| otherwise = error $ "Mixing shift and indexing for sub arguments not supported!\n"
processArgExpr (BindE (BAssign (MkAssign vtype vname (PVar (MkVar "_" arrayType)) ))) idx = (vtype,vname,0)

processArgExprs arg_exprs = processArgExprsH arg_exprs [] 0 
processArgExprsH arg_exprs args idx 
	| length arg_exprs ==0 = args
	| otherwise =
		let
			ae:aes = arg_exprs
			(vtype,vname,idx')=processArgExpr ae idx
			argtup = Arg (MkArgTup vtype vname)
			args' = args++[argtup] 
		in 
	 		processArgExprsH aes args' idx'
 -}
 
condExpr = do
		reserved "if"
		c <- parens pureExpr		
		t <- letExpr
		reserved "else"
		f <- letExpr
		return $ PCond (MkCond (PureE c) (PureE (PLet t)) (PureE (PLet f)))

whileExpr = try whileExprLet <|> whileExprNoLet

whileExprNoLet = do
                reserved "while"
                c <- parens pureExpr            
                return $ PWhile (MkWhile (PureE c) (PureE (PLet (MkLet [] [] GCAny))))

whileExprLet = do
		reserved "while"
		c <- parens pureExpr		
		b <- letExpr		
		return $ PWhile (MkWhile (PureE c) (PureE (PLet b)))

forExpr = do
		reserved "for"
		g <- parens forGuard		
		b <- letExpr		
		return $ PFor (MkFor g (PureE (PLet b)))
		
forGuard = do 
		vinit <- bindExpr
		semi
		vcond <- pureExpr
		semi 
		vmod <- expr
		return $ MkGuard vinit (PureE vcond) vmod 
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

-}
foreachExpr = do
		reserved "foreach"
		reserved "my"
		(ivt,ivn) <- varIdentifierPair
		lv <- listExpr -- problem is that Perl is somewhat unlogical here 		
		b <- letExpr		
		let
			lv' = case lv of
				PBegin (MkBegin _ body _) -> PServiceCall (MkServiceCall "Array" "new" body GCAny)
				otherwise -> lv
		return $ PForeach (MkForeach (PureE (PVar (MkVar ivn ivt))) (PureE lv') (PureE (PLet b)))
		
listExpr = commasepList <|> dotdotList  

commasepList = liftM PBegin beginExpr 
dotdotList= do
				symbol "("
				start_expr <- pureExpr
				symbol ".."
				stop_expr <- pureExpr
				symbol ")"
				return $ PServiceCall (MkServiceCall "Range" "new" [PureE start_expr,PureE stop_expr] GCAny)
											
returnExpr = reserved "return" >> pureExpr >>= \arg -> return $ PReturn arg
			 		
		
--exprList = do 
--			exprs <- sepEndBy1 expr semi
--			return exprs
			
--exprList = many1 expr
exprList = semiSepEnd1 expr			 
			 
-- -----------------------------------------------------------------------------
bindExpr :: Parser Expr
bindExpr = 
		try assignExpr 
	<|> liftM BindE (try updateExpr)
--	<|>	try opUpdateExpr
	<|> (liftM (BindE . BTypeDef) (try typeDef))
	<|> (liftM (BindE . BFunDef) funDef)
	<?> "bindExpr"
	
-- Problem: in Perl, we can have 
-- my $a; $a=1;
-- or
-- my $a=1;
-- In both cases this is not an update!	
--assignExpr :: Parser BindExpr
assignExpr = do
--		vartype <- oldType -- typeExpr
        reserved "my"
        (vartype,varname) <- varIdentifierPair
        reservedOp "="
        rhs <- pureExpr
        if isInstAlloc rhs 
            then
                return $ DeclE $ DInstDecl (MkInstDecl (ia_type ((\(PInstAlloc x)->x) rhs) ) varname  (ia_args ((\(PInstAlloc x)->x) rhs)) )
                --PServiceCall (MkServiceCall inst meth (map PureE args)                           
            else
            do
            	let
            		rhs' = transformList vartype rhs            		
                return $ BindE $ BAssign (MkAssign vartype varname rhs')
                
isInstAlloc (PInstAlloc ( MkInstAlloc insttype instargs)) = True
isInstAlloc _ = False

{-
If the LHS is an Array and the RHS is a "begin" then the RHS must become 
an Array constructor call. As there is only one, implicit, Array service, this is a ServiceCall:
(Array.new args), instead of (begin args)

MkBegin { -- b_ is taken by Basic
	bb_blocktype::[String] 
,	bb_body::[Expr]
,	bb_type::GCType

data ServiceCall = MkServiceCall
    {
         sc_name::String, sc_op::String, sc_args::[Expr], sc_type::GCType -- sc_args is  [PureExpr] but for Data.Generics, use Expr
    }
 

-}
transformList (GCTemplObj (MkTemplObj [] ["Array"] [TArgT GCAny])) (PBegin (MkBegin bt body _)) = PureE (PServiceCall (MkServiceCall "Array" "new" body GCAny))
transformList _ e = PureE e

varIdentifierPair = scalarIdentifierPair <|> arrayIdentifierPair <|> hashIdentifierPair <?> "varIdentifierPair"
scalarType = GCTemplObj $ MkTemplObj [] ["Scalar"] [TArgT GCAny]
scalarIdentifierPair = do
        s <- scalarIdentifier
        return (scalarType,s)
arrayType = GCTemplObj $ MkTemplObj [] ["Array"] [TArgT GCAny]        
arrayIdentifierPair = do
        s <- arrayIdentifier
        return (arrayType,s)
hashType = GCTemplObj $ MkTemplObj [] ["Hash"] [TArgT GCAny]        
hashIdentifierPair = do
        s <- hashIdentifier
        return (hashType,s)
        
updateExpr = do		
		(vartype,varname) <- varIdentifierPair
		reservedOp "="
		rhs <- pureExpr
		return $ BUpdate (MkUpdate varname (PureE rhs) GCAny)
		
opUpdateExpr = do		
		(vartype,varname) <- varIdentifierPair
		op <- resOps		
		symbol "="		
		rhs <- pureExpr
		return $ BOpUpdate (MkOpUpdate varname op (PureE rhs) GCAny)				

funDef = do
		reserved "sub"
--		ftype <- typeExpr
		fname <- identifier
--		fargs <- parens ((commaSep argExpr) <|> voidArgExpr)
		fbody <- letExpr
		let
			exprs = l_body fbody
			args
				| hasArgs exprs = [ Arg (MkArgTup arrayType "_" ) ]
				| otherwise = []		
		return $ MkFunDef GCAny fname args (PureE (PLet fbody))
		
argExpr = do
		argtype <- typeExpr
		argname <- identifier
		return $ Arg $ MkArgTup argtype argname
		
voidArgExpr = reserved "void" >> return [Void]

typeDef = do
		reserved "typedef"
		otype <- oldType
		ntype <- oldType -- typeExpr
		return $ MkTypeDef otype ntype 		

oldType = (try funcType) <|> (try templObjType) <|> (try objType) <|> basicType <|> yadaType <|> anyType
-- ------------------------------------------------------------

typeExpr = 
			yadaType
		<|> anyType	
		<|> try templObjType -- try to match an objType followed by <>			
		<|> try objType
		<|> try funcType
		<|> basicType
		<?> "typeExpr"
				
objType = do 
			tq <- many typeQual
			qobjtype <- sepBy1 identifier nsSep
			return $ GCObj (MkObj tq qobjtype)
--objType = objIdentifier >>= \objtype -> return (GCObj (MkObj [qobjtype]))	

-- Any type should presumably const? But I only enforce const (if I do at all)
-- for basic tyes and List<> types. 
-- I guess List without <> means List<data> or List<any>

templObjType = do
		tq <- many typeQual
		qobjtype <- sepBy1 objIdentifier nsSep
		templargs <- angles (commaSep templArg)
		return $ GCTemplObj (MkTemplObj tq qobjtype templargs)	

templArg = 
		-- (liftM TArgT (try typeExpr))
	-- <|>	(liftM TArgN signedInt)
	    (liftM TArgN signedInt)
	<|> (liftM TArgS strlit)		
    <|>	(liftM TArgT (try typeExpr))

nsSep = symbol "::"
		
yadaType = reserved "..." >> return GCYada
anyType = reserved "any" >> return GCAny

basicType = do
		tq <- many typeQual
		uqbt <- unQualBasicType
		return $ GCBasic $ MkBasic tq uqbt 

unQualBasicType :: Parser UnqualBasicType			
unQualBasicType = 
				liftM (OtherT . MkOtherType) otherType 
			<|> liftM NumberT numberType -- symbol "int" >> return (GCBasic (MkBasic [] (NumberT (SimpleT Int))))
--			<|> liftM (OtherT . MkOtherType) (try otherType) 
			<?> "unQualBasicType"

typeQual = keyword "local"
		 <|> keyword "const" 
		 <?> "typeQual"
		
numberType = (liftM (SimpleT . MkSimpleNumType) (try simpleNumType)) <|> (liftM CIntT cIntType) <?> "numberType"

simpleNumType = 
				keyword "bool"
--			<|> symbol "char"	
			<|> keyword "float"
			<|> keyword "double"
			<|> keyword "uint"
			<?> "simpleNumType"
otherType =
				keyword "string"
			<|> keyword "void"
			<|> keyword "word"
			<|> keyword "data"
			<|> keyword "symbol"
			<|> keyword "any"
			<?> "otherType"

cIntType = do
		csg <- many signQual
		csz <- many sizeQual
		return $ MkCIntType	csg csz
						
signQual = keyword "signed" <|> keyword "unsigned"
sizeQual = keyword "short" <|> keyword "long"	 <|> keyword "int" <|> keyword "char" <?> "C-style int type" -- <|> symbol "long long"					

funcType = do
			rettype <- retTypeExpr
			argtypes <- parens (commaSep retTypeExpr) 
			return $ GCFunc $ MkFunc rettype argtypes

retTypeExpr = do
			yadaType
		<|> try templObjType -- try to match an objType followed by <>			
		<|> try objType
		<|> basicType
		<?> "retTypeExpr"
		
-- -----------------------------------------------------------------------------					  				
declExpr :: Parser DeclExpr  
declExpr = 
                (liftM DServiceDecl (try serviceDecl))
			<|> (liftM DUseDecl (try useDecl))
			<|> (liftM DUseDecl noDecl)                
            <|> (liftM DConfigDecl configDecl)
            <|> (liftM DIncludeDecl includeDecl)
--            <|> (liftM DInstDecl (try instDecl)) --  we have to use InstAlloc
            <?> "declExpr"
			
serviceDecl = do
				reserved "use"
				service <- sepBy1 identifier nsSep				
				reserved "qw"
				methoddecls <- parens (many1 identifier)
				return $ MkServiceDecl (last service) (map (PureE . PString) methoddecls)

useDecl = do
				reserved "use"
				pstr <- pragma
				return $  MkUseDecl True pstr

noDecl = do
				reserved "no"
				pstr <- pragma
				return $  MkUseDecl False pstr
				
pragma = keyword "seq" <|> keyword "strict" <|> keyword "warnings" <|> do {x<- identifier; return x}  <?> "pragma"				
							
methodDecl = 
				try serviceTemplCtor
			<|> try serviceCtor
			<|> try methodDeclF -- C function-style
			<|> methodDeclV -- function-type variable

serviceTemplCtor = do
				service <- identifier
				templargs <- angles (commaSep templArg)
				argtypes <- parens (commaSep typeExpr)
				return $ DeclE $ DOpDecl $ MkOpDecl service (GCFunc (MkFunc (GCTemplObj (MkTemplObj [] [service] templargs)) argtypes)) 		
			
serviceCtor = do
				service <- identifier
				argtypes <- parens (commaSep typeExpr)
				return $ DeclE $ DOpDecl $ MkOpDecl service (GCFunc (MkFunc (GCObj (MkObj [] [service])) argtypes)) 		
				
methodDeclF = do
				rettype <- retTypeExpr
				service <- identifier
				argtypes <- parens (commaSep retTypeExpr)
				return $ DeclE $ DOpDecl $ MkOpDecl service (GCFunc (MkFunc rettype argtypes)) 
				
methodDeclV = do
				ftype <- funcType
				service <- identifier
				return $ DeclE $ DOpDecl $ MkOpDecl service ftype 	
				
configDecl = do
				reserved "configuration"
				configname <- identifier
				configtypes <- braces (semiSepEnd1 typeDef)
				return $ MkConfigDecl configname (map (BindE . BTypeDef) configtypes)

{-
In Gannet-C, an instance declaration is like this:

    Service s(args);

In Gannet-Perl, it must be 

    my $s = Service->new(args);

or

    my $s = new Service(args);
    
So it is actually a bindExpr where we bind to a class method call ( -> ) or call "new" on a class
Using the second version, and making "new" a keyword, we can do
-}
			
instDeclArgs = do
        reserved "my"
        instname <- scalarIdentifier        
        reservedOp "="
        reserved "new"
        insttype <- oldType
        instargs <- parens (commaSep pureExpr)
        return $ MkInstDecl insttype instname (map PureE instargs)

instDeclNoArgs = do
        reserved "my"
        instname <- scalarIdentifier
        reservedOp "="
        reserved "new"
        insttype <- oldType
        return $ MkInstDecl insttype instname []
    
{-
        
instDeclArgs = do
			insttype <- oldType
			instname <- identifier
			instargs <- parens (commaSep pureExpr)
			return $ MkInstDecl insttype instname instargs
			
instDeclNoArgs = do
			insttype <- oldType
			instname <- identifier			
			return $ MkInstDecl insttype instname []		
-}				
instDecl = try instDeclArgs <|> instDeclNoArgs <?> "instDecl"


includeDecl = do
		reserved "#include" 
		(stringLiteral <|> (angles identifier ) ) >>= return
			 													
{--				
service Img {
    ImgBlock in(string,int);
    void (ImgBlock,int) out;
    Img();
    Img(int);
}
--}
			
{-- 
data DeclExpr = 
              DConfig ConfigDecl           
            | DServiceDecl ServiceDecl 
            | DInstDecl InstDecl 
            | DInstDeclSpec InstDeclSpec
            | DTypeDecl TypeDef
--}

varExpr = do 
		(vartype,x) <- varIdentifierPair
		return (PVar (MkVar x vartype))
		
stringExpr = 
	do
		x <- strlit
		return (PString x)

boolExpr = boolTrue <|> boolFalse
boolTrue =  
	do
		reserved "true"
		return $ PNumber (NInt 1)                    
boolFalse =  
	do
		reserved "false"
		return $ PNumber (NInt 0)
                  
--with thanks to arthurb@cs.uu.nl and Andrew.Harris@jhuapl.edu 
numberExpr =  do 
	s   <- sign
	num <- unsignedNum
	return (case num of
              Left  uint ->  PNumber (NInt (applySign s (fromInteger uint)))
#if WORDSZ==64
              Right uflt -> PNumber (NFloat (applySign s uflt))
#elif WORDSZ==32              
              Right uflt -> PNumber (NFloat (applySign s (doubleToFloat uflt)))
#endif              
			)
			
signedInt = do		
	s   <- sign
	num <- unsignedNum	
	return (case num of
		Left uint -> applySign s (fromInteger uint)	
		otherwise -> error $ "Only integers are supported as template arguments"
		)

doubleToFloat :: Double -> Float
doubleToFloat = fromRational . toRational                   

data Sign      = Positive | Negative
applySign          :: Num a => Sign -> a -> a
applySign Positive =  id
applySign Negative =  negate
               
sign  :: Parser Sign
sign  =  do { char '-'
            ; return Negative
            }
     <|> do { char '+'
            ; return Positive
            }
     <|> return Positive
     
semiSepEnd p = sepEndBy p semi
semiSepEnd1 p = sepEndBy1 p semi

scalarIdentifier   = do {sigil <- char '$'; c <- (letter<|> char '_');cs <- many (alphaNum <|> char '_'); whiteSpace; return (c:cs)}
arrayIdentifier   = do {sigil <- char '@'; c <- (letter <|> char '_');cs <- many (alphaNum <|> char '_'); whiteSpace; return (c:cs)}
hashIdentifier   = do {sigil <- char '%'; c <- letter;cs <- many (alphaNum <|> char '_'); whiteSpace; return (c:cs)}

typeIdentifier   = do {c <- letter;cs <- many (alphaNum <|> char '_'); return (c:cs)}
objIdentifier   = do {c <- upper;cs <- many (alphaNum <|> char '_'); return (c:cs)}

keyword name = reserved name >> return name
resOp name = reservedOp name >> return name
resOps = resOp "+" <|> resOp "-" <|> resOp "*" 

pureExpr :: Parser PureExpr
pureExpr = buildExpressionParser optable atomicPureE <?> "pureExpr"

optable =
	let
		binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(POpCall (MkOpCall name [x,y] GCAny))) } ) assoc
--		prefix name = Prefix ( do {  reservedOp name; return (\x ->(POpCall (MkOpCall name [x]))) } ) 
		postfix name = Postfix ( do {   reservedOp name; return (\x ->(POpCall (MkOpCall name [x] GCAny))) } ) 
		prefix name     = Prefix  ( reservedOp  name >> return (\x ->(POpCall (MkOpCall name [x] GCAny))) ) 
--		postfix name  = Postfix ( reservedOp name >> return (\x ->(POpCall (MkOpCall name [x]))) )
--try (do {many1 anyChar; notFollowedBy (oneOf ">,;")});   
--		arrow = Infix ( try $ do { reservedOp "->";  return (\x y ->(POpCall (MkOpCall "->" [x,y] GCAny))) } ) AssocLeft        
		arrow = Infix ( try $ do { reservedOp "->";  return (\x y ->(arrowAppl x y)) } ) AssocLeft		
		pair = Infix ( try $ do { reservedOp "=>";  return (\x y ->(POpCall (MkOpCall "->" [x,y] GCAny))) } ) AssocRight 		
		ltop = Infix ( try $ do { reservedOp "<";  return (\x y ->(POpCall (MkOpCall "<" [x,y] GCAny))) } ) AssocNone    
--		opupdate name = Infix ( do {  reservedOp name; return (\x y ->(BOpUpdate (MkOpUpdate x name (pureExpr y)))) } ) AssocRight                                                          	
	in
		[ --[ prefix "-", prefix "+" ]
		  [ postfix "++", postfix "--"]
--		, [opupdate "+=", opupdate "-=", opupdate "*=", opupdate "/=",opupdate "%=",opupdate "&=",opupdate "|=",opupdate "^="]      
--        , [binop "->" AssocLeft]
        , [arrow]        
		, [binop "**" AssocRight]
		, [ postfix "!", postfix "~"] 
		, [ binop "^"  AssocRight ]
		, [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
		, [ binop "+"  AssocLeft, binop "-"  AssocLeft ] 
		, [binop "<<" AssocLeft, binop ">>" AssocLeft]
		, [ ltop,  binop "<=" AssocNone, binop ">"  AssocNone, binop ">=" AssocNone ] -- binop "<"  AssocNone,
		, [ binop "==" AssocNone, binop "!=" AssocNone ]
		, [ binop "&" AssocLeft, binop "bitand" AssocLeft]
		, [ binop "|" AssocLeft, binop "^" AssocLeft, binop "bitor" AssocLeft, binop "xor" AssocLeft ]    
		, [ binop "&&" AssocLeft, binop "and" AssocLeft ]
		, [ binop "||" AssocLeft, binop "or" AssocLeft ]    
		]

arrowAppl (PVar (MkVar vname vtype)) (PFunAppl (MkFunAppl fname fargs ftype)) = PServiceCall (MkServiceCall vname fname fargs ftype) -- or is it vtype?
arrowAppl x y = POpCall (MkOpCall "->" [x,y] GCAny) -- this must be FunAppl!!!

-- The lexer
gannetcDef = emptyDef {
	  P.commentStart = "/*"
	, P.commentEnd = "*/"
	, P.commentLine = "#"
	, P.nestedComments = True
	, P.identStart = letter  <|> char '_'
	, P.identLetter = alphaNum <|> char '_'
	, P.opStart = P.opLetter emptyDef
	, P.opLetter = oneOf ":!*+./<=>?^|-~"
	-- I'm not sure what 'reserved' actually means but I observe that '*' is never 
	-- included  
	-- reservedOpNames result in matching the rest of a string not the name, e.g.
	-- xandy => and y 
	, P.reservedOpNames= ["->","=>","+","-","*","=","and","or","xor","bitand","bitor"]
--	, P.reservedOpNames= []
	, P.reservedNames = [
		"case", -- Perl doesn't have this
		"default",
		"if", -- service
		"else",
		"elsif",
		"switch", -- Perl doesn't have this
		"while",
		"do",
		"for",
		"seq",
		"par",
		"goto",
		"continue",
		"break",
		"return", -- service
		"typedef", -- Perl doesn't have this
		"lambda", -- service
		"configuration", -- Perl doesn't have this
		"service", -- Perl doesn't have this
		"void", -- Perl doesn't have this
		"char", -- Perl doesn't have this
		"int", -- Perl doesn't have this
		"uint", -- Perl doesn't have this
		"short", -- Perl doesn't have this
		"long", -- Perl doesn't have this
		"float", -- Perl doesn't have this
		"double", -- Perl doesn't have this
		"signed", -- Perl doesn't have this
		"unsigned", -- Perl doesn't have this
		"bool", -- Perl doesn't have this
		"true", -- Perl doesn't have this
		"false", -- Perl doesn't have this
		"template", -- Perl doesn't have this
		"string", -- Perl doesn't have this
		"tuple", -- Perl doesn't have this
		"data", -- Perl doesn't have this
		"any", -- Perl doesn't have this
		"word", -- Perl doesn't have this
		"symbol", -- Perl doesn't have this
--		"Buf",
--		"Gen",
--		"Lambda",
--		"Stream",
		"local",
		"const", -- Perl doesn't have this
		"inline", -- Perl doesn't have this
        "operator", -- Perl doesn't have this
		"...",
		"#include", -- Perl doesn't have this
		"#define", -- Perl doesn't have this
		"#if", -- Perl doesn't have this
		"#ifdef", -- Perl doesn't have this
		"ifndef", -- Perl doesn't have this
		"#else", -- Perl doesn't have this
		"#elif", -- Perl doesn't have this
		"#endif", -- Perl doesn't have this
		"#pragma", -- Perl doesn't have this
		--"extern",
		--"static",
		--"auto",
		--"register",
		--"volatile",
		"use",
		"no",
		"package",
		"sub",
		"my",
		"bless",		
		"new", -- promoted to keyword
		"strict",
		"warnings"		
--		"seq"  -- only in "use seq"
		-- not sure about shift, print etc 
		]
	, P.caseSensitive = True	
}

lexer       = P.makeTokenParser gannetcDef    

strlit      = P.stringLiteral lexer
intp			= P.integer lexer
unsignedNum         = P.naturalOrFloat lexer
other         = P.identifier lexer <|> P.operator lexer 

operator		= P.operator lexer
parens          = P.parens lexer    
braces          = P.braces lexer
angles          = P.angles lexer    
--sepEndBy		= P.sepEndBy lexer
--sepEndBy1		= P.sepEndBy1 lexer
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
dot 			= P.dot lexer
comma			= P.comma lexer
semi			= P.semi lexer


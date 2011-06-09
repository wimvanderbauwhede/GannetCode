{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}

{--
TODO
----
How do I indicate that the end of a block indicates the end 
of an expression? 
update operators (+=, *= etc) not yet supported


--}

module GannetPerl.Parser (
	prettyGannetPerlFromFile,
	parseGannetPerl
) where

import GannetPerl.AST
import GannetPerl.PerlServices
import qualified GannetPerl.GannetParsec as GP

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 

-- -- WV: Can't get this to work, because I still use parsec-2.1.0.1
--import Text.Parsec hiding (State)
--import Text.Parsec.Expr
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
--program = whiteSpace >> semiSepEnd1 expr >>= \exprs -> return $ MkProg exprs
program = whiteSpace >> semiOrBlockSepEndBy1 expr >>= \exprs -> return $ MkProg [PureE $ PLet $ MkLet [] (perlPrelude++(processPureExprs exprs)) GPAny]
--program = many1 expr >>= \exprs -> return $ MkProg exprs
perlPrelude = 
	let
		ds = DeclE (DVarDecl (MkVarDecl "$_" scalarTypeAny)) 
		da = DeclE (DVarDecl (MkVarDecl "@_" arrayTypeAny)) 
		dh = DeclE (DVarDecl (MkVarDecl "%_" hashTypeAny))
	in
		[ds,da,dh]	
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
        <|> regExpr
        <|> try emptyListConst    
        <|> returnExpr
        <|> try arrayIndex
        <|> try instAlloc
        <|> fileOpenExpr
        <|> try serviceCall 
        <|> try funAppl    		    
        <|> liftM PLet (try letExpr)
        <|> liftM PBegin beginExpr
        <|> condExpr
        <|> whileExpr 
        <|> forExpr
        <|> foreachExpr -- FIXME: allow "for" as "foreach"  
        <|> liftM PLambdaDef lambdaDef
        <|> bareCommand 
        <|> varExpr 
        <|> arefConst
        <|> hrefConst            
        <|> fileReadExpr
        <|> parens pureExpr
		<?> "atomicPureE"
       
instAllocArgs = do
        reserved "new"
        insttype <- instanceType
        instargs <- parens (commaSep pureExpr)
        return $ PInstAlloc ( MkInstAlloc insttype (map PureE instargs))

instAllocNoArgs = do
        reserved "new"
        insttype <- instanceType
        return $ PInstAlloc (MkInstAlloc insttype [])
    				
instAlloc = try instAllocArgs <|> instAllocNoArgs <?> "instAlloc"

funAppl = do
				fname <- identifier
				args <- parens (commaSep pureExpr)
				return $ if fname/="shift" -- TODO: make generic!
							then PFunAppl (MkFunAppl fname (map PureE args) GPAny)
							else PServiceCall (MkServiceCall "Array" fname (map PureE args) GPAny) 

bareCommand = printFHArgs <|>bareCommandArgs <|>  bareCommandNoArgs   				
bareCommandNoArgs = do
				cname <- identifier 
				return $ commandAppl cname [PureE (PVar (MkVar "@_" arrayTypeAny))]			
-- FIXME
bareCommandArgs = do
				cname <- identifier
				args <- commaSep pureExpr
				return $ commandAppl cname (map PureE args)
printFHArgs = do
				symbol "print"
				(fht,fhn) <- scalarIdentifierPair
				args <- commaSep pureExpr
				return $ commandAppl "print" (map PureE (PVar (MkVar fhn (fht fhType) ):args))

regExpr = do
		many (symbol "m")
		x <- regexlit
		return (PRegex x)

{-
How to parse regexes? The problem is mainly slashes in the pattern
The =~ operator is actually a PureE: $v is never modified; it returns a list of matches

(PCRE.match v restr)

$v =~ /http:\/\// ; 

a bare match should translate to an update or assign of $_; so how do we detect that 
the regex is _not_ inside an assign or udate?
-}				
					
-- something like foldlM (<|>) (head perlCommands) (tail perlCommands) -- BROKEN
--command = choice (map symbol perlCommands) -- BROKEN
--command =  
--				symbol "shift" 
--			<|> symbol "pop"
--			<|> symbol "scalar"
--			<|> symbol "push"
--			<|> symbol "unshift"
--			<|> symbol "print"
--			-- and many, many more!
--			<?> "commands"			
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
commandAppl cname args
	| perlService cname/="" = PServiceCall (MkServiceCall (perlService cname) cname args GPAny)
	| otherwise = PFunAppl (MkFunAppl cname args  GPAny) 
		  												
serviceCall = do
				inst <- identifier
				arrow
				meth <- identifier
				args <- parens (commaSep pureExpr)
				return $ PServiceCall (MkServiceCall inst meth (map PureE args) GPAny) 
-- $a[e1][e2]... -> (Array.at (Array.at a e1) e2)				
arrayIndex = do
			(vartype,varname) <- varIdentifierPairAny	
			idxs <- many1 (brackets pureExpr)
			let
				inst = parseArray (PVar (MkVar varname vartype)) idxs	
			return inst				

parseArray var idxs 
	| length idxs>1 = foldl (\a ie -> (PServiceCall (MkServiceCall "Array" "at" [PureE a, PureE ie] GPAny))) var idxs
	| otherwise = 	PServiceCall (MkServiceCall "Array" "at" [PureE var, PureE (head idxs)] GPAny)					


arrow = symbol "->"

arefConst = do
	elts <- brackets (commaSepEnd pureExpr)
	return $ PServiceCall (MkServiceCall "Array" "new" (map PureE elts) GPAny)

hrefConst = do
	elts <- braces (commaSepEnd pairExpr)
	return $ PServiceCall (MkServiceCall "Hash" "new" (map PureE elts) GPAny)

emptyListConst = do
    char '('
    char ')'
    return  $ PServiceCall (MkServiceCall "Array" "new" [] GPAny)
    
pair = symbol "=>"
pairExpr = do
			k <- keyExpr
			pair
			v <- pureExpr
			return $ PPair (MkPair (PureE k) (PureE v) GPAny)

keyExpr = stringExpr <|> numberExpr <|> varExpr
        
blockType =  keyword "par" <|> keyword "seq"       
letExpr = do
			bt <- many blockType
			exprs <- braces exprList
			let
				exprs'' = processPureExprs exprs
				exprs' = processSeq exprs'' []			
			return $ MkLet bt exprs' GPAny
			
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
                       (PureE (PBegin (MkBegin ["seq"] exprs GPAny)),xs')
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
			return $ MkBegin [] exprs GPAny		

processPureExprs el = map bindRegex el                        
bindRegex x = case x of 
	(PureE (PRegex _)) ->  BindE (BUpdate (MkUpdate "$_" x (scalarType regexType)))
	_ -> x
lambdaDef = do
			reserved "sub"
			fbody <- letExpr
			let
				exprs = l_body fbody
				args 
					| hasArgs exprs = [ Arg (MkArgTup arrayTypeAny "@_" ) ]
					| otherwise = []
			return $ MkLambdaDef GPAny args (PureE (PLet fbody)) 						        

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
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PServiceCall (MkServiceCall "Array" "shift" [PureE (PVar (MkVar "@_" arrayType))] _)))))) = True
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PFunAppl (MkFunAppl "shift" [PureE (PVar (MkVar "_" arrayType))] GPAny)) )))) = True
-- to support pop we need a temp stack of the pop'ed values. 
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PServiceCall (MkServiceCall "Array" "pop" [PureE (PVar (MkVar "@_" arrayType))] _)) )))) = True
-- $_[...] but this will only work if the index expression is a constant
-- also, we need to deal with out-of-order indexing, by building a list of pairs and sorting them. 
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PServiceCall (MkServiceCall "Array" "at" [PureE (PVar (MkVar "@_" arrayType)),_] _) ))))) = True
-- @_
isArgExpr (BindE (BAssign (MkAssign _ _  (PureE (PVar (MkVar "@_" arrayType))) ))) = True
isArgExpr _ = False

hasArgs exprs = length (filter isArgExpr exprs) > 0

{-
processArgExpr (BindE (BAssign (MkAssign vtype vname (PServiceCall (MkServiceCall "Array" "shift" [PureE (PVar (MkVar "_" arrayType))] _))))) idx = (vtype,vname,idx+1)
processArgExpr (BindE (BAssign (MkAssign vtype vname (PFunAppl (MkFunAppl "shift" [PVar (MkVar "_" arrayType)] GPAny)) ))) idx = (vtype,vname,idx+1)
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
-- i.e. while(...);
whileExprNoLet = do
                reserved "while"
                c <- parens expr -- pureExpr
                let c' =  case c of
					PureE (PServiceCall (MkServiceCall "IO" "readline" _ _)) -> BindE (BAssign (MkAssign scalarTypeAny "$_" c)) 
					_ -> c
                return $ PWhile (MkWhile c (PureE (PLet (MkLet [] [] GPAny))))

whileExprLet = do
	reserved "while"
	c <- parens expr
	b <- letExpr		
	let c' =  case c of
	-- only a <...> inside a while () is assigned automatically to $_ 
		PureE (PServiceCall (MkServiceCall "IO" "readline" _ _)) -> BindE (BAssign (MkAssign scalarTypeAny "$_" c)) 
		_ -> c
--		_ -> BindE (BAssign (MkAssign scalarType "_" c))
	return $ PWhile (MkWhile c' (PureE (PLet b)))

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

foreachExpr = do
		reserved "foreach"
		reserved "my"
		(ivt,ivn) <- varIdentifierPairAny
		lv <- listExpr -- problem is that Perl is somewhat unlogical here 		
		b <- letExpr		
		let
			lv' = case lv of
				PBegin (MkBegin _ [(PureE (PServiceCall (MkServiceCall "Range" "new" rargs _)))] _) -> PServiceCall (MkServiceCall "Range" "new" rargs GPAny)
				PBegin (MkBegin _ body _) -> PServiceCall (MkServiceCall "Array" "new" body GPAny)
				otherwise -> lv
		return $ PForeach (MkForeach (PureE (PVar (MkVar ivn ivt))) (PureE lv') (PureE (PLet b)))
		
listExpr = commasepList -- <|> dotdotList  

commasepList = liftM PBegin beginExpr 
dotdotList= do
				dotdotExpr <- parens pureExpr
				return $ dotdotExpr
											
returnExpr = reserved "return" >> pureExpr >>= \arg -> return $ PReturn arg
-- Open returns nonzero on success, the undefined value otherwise; here we have to use 0 to mean "undefined"
-- but the runtime will return NIL			 		
fileOpenExpr = do
		reserved "open"
		reserved "my" -- although someone might declare the FH before using it in open(), so maybe "many"
		(fht,fhn) <- varIdentifierPair -- a FH is a uint
		comma
		mode <- stringExpr
		comma
		filename <- stringExpr
		return $ PServiceCall (MkServiceCall "IO" "open" [PureE (PVar (MkVar fhn (fht fhType))),PureE filename,PureE mode] intType) 

fileReadExpr = do
	(fht,fhn) <- angles varIdentifierPair
	return  $ PServiceCall (MkServiceCall "IO" "readline" [PureE (PVar (MkVar fhn (fht fhType)))] GPAny) -- FIXME: it's a String!
	 		
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
	<|>	liftM BindE (try opUpdateExpr)
--	<|> (liftM (BindE . BTypeDef) (try typeDef))
	<|> (liftM (BindE . BFunDef) funDef)
	<?> "bindExpr"
	
-- Problem: in Perl, we can have 
-- my $a; $a=1;
-- or
-- my $a=1;
-- In both cases this is not an update!

-- We could do some context-free type inferencing in assign and update:
-- get type from RHS, use it on LHS. To make it easier, vartype should be a function 	
--assignExpr :: Parser BindExpr
assignExpr = do
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
            		rhs' = transformList (vartype GPAny) rhs            		
                return $ BindE $ BAssign (MkAssign (vartype GPAny) varname rhs') -- FIXME: if assignment returns bool, this should be bool

---- FIXME: parens PureExpr makes the parser stop on empty (), before it gets to initExpr, because try actually succeeds!
--initExpr = do
--        reserved "my"
--        (vartype,varname) <- varIdentifierPair
--        reservedOp "="
--        emptyParens -- can be (), [], {} or even <>; for now only () -> must work for @ and %
--        return $ MkVarDecl varname (vartype GPAny)

{-
We need list assignments and updates as well:

(my $a, my $b, my @c) = @r;

I think we parse this as

my $a = shift @r;
my $b = shift @r;
my @c = @r;

and similarly,
(my $a, $b, @c) = @r;

i.e. any arg in the list can result in either an assign or an update ...

do
	lhsvars <- parens ( commaSep1 varDeclOrVar)  
	reserved "="
	rhsarray <- arrayIdentifierPairAny
	return

varDeclOrVar = do 
	many (reserved "my")
	(vt,vn) <- scalarIdentifierPairAny
	return 
-}

{-
We also need array element updates, e.g.

$a[$i][$j]=rand();

This is non-trivial: in Perl we don't know the size of the array so 
we can't linearise it. 2-D arrays are arrays of arrays, so the 
question is what this looks like in Gannet.

A new array is created as a Word_List or vector<Word>, and we return the 
pointer. 
The only way to create a multi-dimensional array is to cast the pointers to Word.
The problem is this:

my @a=() => (assign 'a (Array.new)) => Word_List* wlp= new Word_list;
$a[$i][$j] = rand() => 
(Array.set (Array.get a) i (Array.new)) ; But only if (Array.at (Array.get a) i) is undefined
(Array.set (Array.get (Array.at (Array.get a) i)) j (rand))

How can we express that ap[i] is undefined? I don't want to use
exceptions if I can help it.
A simple solution is to require declaration like in Ruby.
Maybe this works: 
- get the length of the array. If i>length it must be new  -- and in fact
all intermediate positions must be created as well!

Basically, in C++ we have something like

uint len = ap.length();
if (i>=len) {
for (int k=len;k<=i;k++) {
ap[k]=new Word_List ; // unless ap[i] did already exist.
}
} 
ap[i][j]=rand();


I would prefer to be able to do something like

(Array.at a i j k ...) 

How do we lookup without hardcoding the number of brackets?
It's some kind of recursion: get the array reference, deref it

and

(Array.set a i j k ... val)

Same here, but take into account the code from above. Tricky!

-}
                
isInstAlloc (PInstAlloc ( MkInstAlloc insttype instargs)) = True
isInstAlloc _ = False

{-
If the LHS is an Array and the RHS is a "begin" then the RHS must become 
an Array constructor call. As there is only one, implicit, Array service, this is a ServiceCall:
(Array.new args), instead of (begin args)

MkBegin { -- b_ is taken by Basic
	bb_blocktype::[String] 
,	bb_body::[Expr]
,	bb_type::GPType

data ServiceCall = MkServiceCall
    {
         sc_name::String, sc_op::String, sc_args::[Expr], sc_type::GPType -- sc_args is  [PureExpr] but for Data.Generics, use Expr
    }
 
BindE (BAssign 
(MkAssign {
	a_type = GPTemplObj (MkTemplObj {to_typequal = [], to_qtype = ["Array"], to_args = [TArgT GPAny]}), 
	a_name = "ar", 
	a_rhs = PureE (PServiceCall (MkServiceCall {sc_name = "Range", sc_op = "new", sc_args = [PureE (PNumber (NInt 1)),PureE (PNumber (NInt 10))], sc_type = GPAny}))}))

-}
transformList (GPTemplObj (MkTemplObj [] ["Array"] [TArgT GPAny])) (PBegin (MkBegin bt [(PureE (PServiceCall (MkServiceCall "Range" "new" body GPAny)))] _)) = PureE (PServiceCall (MkServiceCall "Range" "new" body GPAny))
transformList (GPTemplObj (MkTemplObj [] ["Array"] [TArgT GPAny])) (PBegin (MkBegin bt body _)) = PureE (PServiceCall (MkServiceCall "Array" "new" body GPAny))
transformList _ e = PureE e
       
updateExpr = do		
		(vartype,varname) <- varIdentifierPairAny
		reservedOp "="
		rhs <- pureExpr
		return $ BUpdate (MkUpdate varname (PureE rhs) vartype)
		
opUpdateExpr = do		
		(vartype,varname) <- varIdentifierPairAny
		op <- updateOps			
		rhs <- pureExpr
		return $ BOpUpdate (MkOpUpdate varname op (PureE rhs) vartype)				

funDef = do
		reserved "sub"
--		ftype <- typeExpr
		fname <- identifier
--		fargs <- parens ((commaSep argExpr) <|> voidArgExpr)
		fbody <- letExpr
		let
			exprs = l_body fbody
			args
				| hasArgs exprs = [ Arg (MkArgTup arrayTypeAny "@_" ) ]
				| otherwise = []		
		return $ MkFunDef GPAny fname args (PureE (PLet fbody))
		
argExpr = do
		argtype <- typeExpr
		argname <- identifier
		return $ Arg $ MkArgTup argtype argname
---- unused in Perl		
--voidArgExpr = reserved "void" >> return [Void]
---- unused in Perl
--typeDef = do
--		reserved "typedef"
--		otype <- instanceType
--		ntype <- instanceType -- typeExpr
--		return $ MkTypeDef otype ntype 		

-- ------------------------------------------------------------
-- ------------------------------------------------------------

varIdentifierPair = scalarIdentifierPair <|> arrayIdentifierPair <|> hashIdentifierPair <?> "varIdentifierPair"
varIdentifierPairAny = scalarIdentifierPairAny <|> arrayIdentifierPairAny <|> hashIdentifierPairAny <?> "varIdentifierPairAny"

--scalarType t = GPTemplObj $ MkTemplObj [] ["Scalar"] [TArgT t]
--scalarTypeAny = GPTemplObj $ MkTemplObj [] ["Scalar"] [TArgT GPAny]
scalarIdentifierPair = do
        s <- scalarIdentifier
        let
        	s'
        		| s=="_" = "$_"
        		| otherwise = s
        return (scalarType,s')

scalarIdentifierPairAny  = do
        s <- scalarIdentifier
        let
        	s'
        		| s=="_" = "$_"
        		| otherwise = s
        return (scalarTypeAny,s)
         
--arrayType t = GPTemplObj $ MkTemplObj [] ["Array"] [TArgT t]
--arrayTypeAny = GPTemplObj $ MkTemplObj [] ["Array"] [TArgT GPAny]        
arrayIdentifierPair = do
        s <- arrayIdentifier
        let
        	s'
        		| s=="_" = "@_"
        		| otherwise = s
        return (arrayType,s)
arrayIdentifierPairAny = do
        s <- arrayIdentifier
        let
        	s'
        		| s=="_" = "@_"
        		| otherwise = s
        return (arrayTypeAny,s)        
        
--hashType t = GPTemplObj $ MkTemplObj [] ["Hash"] [TArgT t]
--hashTypeAny = GPTemplObj $ MkTemplObj [] ["Hash"] [TArgT GPAny]        
hashIdentifierPair = do
        s <- hashIdentifier
        let
        	s'
        		| s=="_" = "%_"
        		| otherwise = s
        return (hashType,s)
hashIdentifierPairAny = do
        s <- hashIdentifier
        let
        	s'
        		| s=="_" = "%_"
        		| otherwise = s
        return (hashTypeAny,s)

--refType t = GPTemplObj $ MkTemplObj [] ["Ref"] [TArgT t]
--refTypeAny = GPTemplObj $ MkTemplObj [] ["Ref"] [TArgT GPAny]
--fhType = GPObj $ MkObj [] ["FileHandle"]
--
--regexType = GPObj $ MkObj [] ["RegEx"]
-- ------------------------------------------------------------
instanceType = (try funcType) <|> (try templObjType) <|> (try objType) <|> basicType <|> yadaType <|> anyType


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
			return $ GPObj (MkObj tq qobjtype)
--objType = objIdentifier >>= \objtype -> return (GPObj (MkObj [qobjtype]))	

-- Any type should presumably const? But I only enforce const (if I do at all)
-- for basic tyes and List<> types. 
-- I guess List without <> means List<data> or List<any>

-- not used in Perl
templObjType = do
		tq <- many typeQual
		qobjtype <- sepBy1 objIdentifier nsSep
		templargs <- angles (commaSep templArg)
		return $ GPTemplObj (MkTemplObj tq qobjtype templargs)	

templArg = 
		-- (liftM TArgT (try typeExpr))
	-- <|>	(liftM TArgN signedInt)
	    (liftM TArgN signedInt)
	<|> (liftM TArgS strlit)		
    <|>	(liftM TArgT (try typeExpr))

nsSep = symbol "::"
		
yadaType = reserved "..." >> return GPYada
anyType = reserved "any" >> return GPAny

basicType = do
		tq <- many typeQual
		uqbt <- unQualBasicType
		return $ GPBasic $ MkBasic tq uqbt 

unQualBasicType :: Parser UnqualBasicType			
unQualBasicType = 
				liftM (OtherT . MkOtherType) otherType 
			<|> liftM NumberT numberType -- symbol "int" >> return (GPBasic (MkBasic [] (NumberT (SimpleT Int))))
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
			return $ GPFunc $ MkFunc rettype argtypes

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
			<|> (liftM DVarDecl varDecl)      
--            <|> (liftM DConfigDecl configDecl)
--            <|> (liftM DIncludeDecl includeDecl)
--            <|> (liftM DInstDecl (try instDecl)) --  we have to use InstAlloc
            <?> "declExpr"
			
serviceDecl = do
				reserved "use"
				service <- sepBy1 identifier nsSep				
				reserved "qw"
				methoddecls <- parens (many1 identifier)
				return $ MkServiceDecl (last service) (map (PureE . PString) methoddecls)
varDecl = do
        reserved "my"
        (vartype,varname) <- varIdentifierPair
        return $ MkVarDecl varname (vartype GPAny)

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
				return $ DeclE $ DOpDecl $ MkOpDecl service (GPFunc (MkFunc (GPTemplObj (MkTemplObj [] [service] templargs)) argtypes)) 		
			
serviceCtor = do
				service <- identifier
				argtypes <- parens (commaSep typeExpr)
				return $ DeclE $ DOpDecl $ MkOpDecl service (GPFunc (MkFunc (GPObj (MkObj [] [service])) argtypes)) 		
				
methodDeclF = do
				rettype <- retTypeExpr
				service <- identifier
				argtypes <- parens (commaSep retTypeExpr)
				return $ DeclE $ DOpDecl $ MkOpDecl service (GPFunc (MkFunc rettype argtypes)) 
				
methodDeclV = do
				ftype <- funcType
				service <- identifier
				return $ DeclE $ DOpDecl $ MkOpDecl service ftype 	
---- unused in Perl				
--configDecl = do
--				reserved "configuration"
--				configname <- identifier
--				configtypes <- braces (semiSepEnd1 typeDef)
--				return $ MkConfigDecl configname (map (BindE . BTypeDef) configtypes)

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
        insttype <- instanceType
        instargs <- parens (commaSep pureExpr)
        return $ MkInstDecl insttype instname (map PureE instargs)

instDeclNoArgs = do
        reserved "my"
        instname <- scalarIdentifier
        reservedOp "="
        reserved "new"
        insttype <- instanceType
        return $ MkInstDecl insttype instname []
    
instDecl = try instDeclArgs <|> instDeclNoArgs <?> "instDecl"

---- unused in Perl
--includeDecl = do
--		reserved "#include" 
--		(stringLiteral <|> (angles identifier ) ) >>= return
			 													
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

emptyParens = do
    char '('
    char ')'

varExpr = do 
		(vartype,x) <- varIdentifierPair
		return (PVar (MkVar x (vartype GPAny)))
		
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

commaSepEnd p = sepEndBy p comma
commaSepEnd1 p = sepEndBy1 p comma 

{-
FIXME: I need a better combinator than semiSepEnd1

What we do is: look at x, which is an Expr in our case
if it is a block, don't look for a semi, otherwise do
Blocks are parsed into Let or a list of TypeDefs ( in Gannet-C)
but of course these are inside other expressions. We have to consider 
the outermost expression, i.e.

PIf
PFor
PForeach
PWhile 
BFunDef
PLambdaDef
-}
-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@. 

--semiOrBlockSepEndBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
semiOrBlockSepEndBy1 p = do
	x <- p -- consume p
	do
		case x of
			PureE (PFor _) -> noSemi x p
			PureE (PForeach _) -> noSemi x p
			PureE (PWhile _) -> noSemi x p
 			PureE (PLet _) -> noSemi x p
			PureE (PCond _) -> noSemi x p
			otherwise -> do				                            
				semi -- find a sep. So this needs to become conditional: if p was braces, we just go on without sep
				xs <- semiOrBlockSepEndBy p -- recurse
				return (x:xs)
		<|> return [x]
                        
noSemi x p = do
	many semi
	xs <- semiOrBlockSepEndBy p -- recurse
	return (x:xs)
-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi

--semiOrBlockSepEndBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
semiOrBlockSepEndBy p = semiOrBlockSepEndBy1 p <|> return []

strlit = GP.stringLiteral lexer 
regexlit = GP.regexLiteral lexer
--------------------

scalarIdentifier   = do {sigil <- char '$'; c <- (letter<|> char '_');cs <- many (alphaNum <|> char '_'); whiteSpace; return (c:cs)}
arrayIdentifier   = do {sigil <- char '@'; c <- (letter <|> char '_');cs <- many (alphaNum <|> char '_'); whiteSpace; return (c:cs)}
hashIdentifier   = do {sigil <- char '%'; c <- letter;cs <- many (alphaNum <|> char '_'); whiteSpace; return (c:cs)}

typeIdentifier   = do {c <- letter;cs <- many (alphaNum <|> char '_'); return (c:cs)}
objIdentifier   = do {c <- upper;cs <- many (alphaNum <|> char '_'); return (c:cs)}

keyword name = reserved name >> return name

resOp name = reservedOp name >> return name
resOps = resOp "+" <|> resOp "-" <|> resOp "*"

updateOp name = reservedOp name >> return (init name)
updateOps = choice (map updateOp [ "+=",  "-=",  "*=",  "/=", "%=", "**=", "&=", "|=", "^=", "||=", "&&="]) 


pureExpr :: Parser PureExpr
pureExpr = buildExpressionParser optable atomicPureE <?> "pureExpr"

optable =
	let
		binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(POpCall (MkOpCall name [x,y] GPAny))) } ) assoc
--		prefix name = Prefix ( do {  reservedOp name; return (\x ->(POpCall (MkOpCall name [x]))) } ) 
		postfix name = Postfix ( do {   reservedOp name; return (\x ->(POpCall (MkOpCall name [x] GPAny))) } ) 
		prefix name     = Prefix  ( reservedOp  name >> return (\x ->(POpCall (MkOpCall name [x] GPAny))) ) 
--		postfix name  = Postfix ( reservedOp name >> return (\x ->(POpCall (MkOpCall name [x]))) )
--try (do {many1 anyChar; notFollowedBy (oneOf ">,;")});   
--		arrow = Infix ( try $ do { reservedOp "->";  return (\x y ->(POpCall (MkOpCall "->" [x,y] GPAny))) } ) AssocLeft        
		arrow = Infix ( try $ do { reservedOp "->";  return (\x y ->(arrowAppl x y)) } ) AssocLeft		
--		pair = Infix ( try $ do { reservedOp "=>";  return (\x y ->(POpCall (MkOpCall "=>" [x,y] GPAny))) } ) AssocRight 		
		ltop = Infix ( try $ do { reservedOp "<";  return (\x y ->(POpCall (MkOpCall "<" [x,y] GPAny))) } ) AssocNone    
		dotdot = Infix ( try $ do { reservedOp "..";  return (\x y ->(dotdotAppl x y)) } ) AssocNone
		regexmatch = Infix ( try $ do { reservedOp "=~";  return (\x y ->(regexMatchAppl x y)) } ) AssocLeft
--		opupdate name = Infix ( do {  reservedOp name; return (\x y ->(BOpUpdate (MkOpUpdate "x" name (PureE y)))) } ) AssocRight -- FIXME: is not a PureE BOpUpdate !!!                                                          	
	in
		[ --[ prefix "-", prefix "+" ]
		  [ postfix "++", postfix "--"] -- Int
--		, [opupdate "+=", opupdate "-=", opupdate "*=", opupdate "/=",opupdate "%=",opupdate "&=",opupdate "|=",opupdate "^="]      
--        , [binop "->" AssocLeft]
        , [arrow]        
		, [binop "**" AssocRight]
		, [ postfix "!", postfix "~"] 
		, [ binop "^"  AssocRight ]
		, [regexmatch]
		, [ binop "!~"  AssocLeft]
		, [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
		, [ binop "+"  AssocLeft, binop "-"  AssocLeft ] 
		, [binop "<<" AssocLeft, binop ">>" AssocLeft]
		, [ ltop,  binop "<=" AssocNone, binop ">"  AssocNone, binop ">=" AssocNone ] -- binop "<"  AssocNone,
		, [ binop "==" AssocNone, binop "!=" AssocNone ]
		, [ binop "&" AssocLeft, binop "bitand" AssocLeft]
		, [ binop "|" AssocLeft, binop "^" AssocLeft, binop "bitor" AssocLeft, binop "xor" AssocLeft ]    
		, [ binop "&&" AssocLeft, binop "and" AssocLeft ]
		, [ binop "||" AssocLeft, binop "or" AssocLeft ]
--		, [pair]
		, [dotdot]
--		, [ binop ".." AssocNone]    
		]

arrowAppl (PVar (MkVar vname vtype)) (PFunAppl (MkFunAppl fname fargs ftype)) = PServiceCall (MkServiceCall vname fname fargs ftype) -- or is it vtype?
arrowAppl x y = POpCall (MkOpCall "->" [x,y] GPAny) -- this must be FunAppl!!!

dotdotAppl start_expr stop_expr = PServiceCall (MkServiceCall "Range" "new" [PureE start_expr,PureE stop_expr] GPAny)
regexMatchAppl str re = PServiceCall (MkServiceCall "PCRE" "match" [PureE str,PureE re] GPAny)

-- The lexer
gannetcDef = emptyDef {
	  P.commentStart = "/*"
	, P.commentEnd = "*/"
	, P.commentLine = "#"
	, P.nestedComments = True
	, P.identStart = letter  <|> char '_'
	, P.identLetter = alphaNum <|> char '_'
	, P.opStart = P.opLetter emptyDef
	, P.opLetter = oneOf ":!*+./<=>?^|-~\\"
	-- I'm not sure what 'reserved' actually means but I observe that '*' is never 
	-- included  
	-- reservedOpNames result in matching the rest of a string not the name, e.g.
	-- xandy => and y 
	, P.reservedOpNames= ["->","=>","+","-","*","=","and","or","xor","bitand","bitor"]
--	, P.reservedOpNames= []
	, P.reservedNames = [
--		"case", -- Perl doesn't have this
--		"default",
		"if", -- service
		"else",
		"elsif",
--		"switch", -- Perl doesn't have this
		"while",
		"do",
		"for",
		"seq",
--		"par",
		"goto",
		"continue",
		"break",
		"return", -- service
--		"typedef", -- Perl doesn't have this
--		"lambda", -- service
--		"configuration", -- Perl doesn't have this
--		"service", -- Perl doesn't have this
--		"void", -- Perl doesn't have this
--		"char", -- Perl doesn't have this
--		"int", -- Perl doesn't have this
--		"uint", -- Perl doesn't have this
--		"short", -- Perl doesn't have this
--		"long", -- Perl doesn't have this
--		"float", -- Perl doesn't have this
--		"double", -- Perl doesn't have this
--		"signed", -- Perl doesn't have this
--		"unsigned", -- Perl doesn't have this
--		"bool", -- Perl doesn't have this
--		"true", -- Perl doesn't have this
--		"false", -- Perl doesn't have this
--		"template", -- Perl doesn't have this
--		"string", -- Perl doesn't have this
--		"tuple", -- Perl doesn't have this
--		"data", -- Perl doesn't have this
--		"any", -- Perl doesn't have this
--		"word", -- Perl doesn't have this
--		"symbol", -- Perl doesn't have this
		"local",
--		"const", -- Perl doesn't have this
--		"inline", -- Perl doesn't have this
--        "operator", -- Perl doesn't have this
		"...",
--		"#include", -- Perl doesn't have this
--		"#define", -- Perl doesn't have this
--		"#if", -- Perl doesn't have this
--		"#ifdef", -- Perl doesn't have this
--		"ifndef", -- Perl doesn't have this
--		"#else", -- Perl doesn't have this
--		"#elif", -- Perl doesn't have this
--		"#endif", -- Perl doesn't have this
--		"#pragma", -- Perl doesn't have this
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
		]
	, P.caseSensitive = True	
}

lexer       = P.makeTokenParser gannetcDef    

--strlit      = P.stringLiteral lexer
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
--stringLiteral   = P.stringLiteral lexer  
dot 			= P.dot lexer
comma			= P.comma lexer
semi			= P.semi lexer
 
-- with ghc 7.0.2, I had to define char in GP and hide it from Text.ParserCombinators.Parsec
--char = GP.char

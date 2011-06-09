{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{--
TODO
----
Ideally I'd like identifiers to be  
	, P.identStart = lower  <|> char '_'
	, P.identLetter = alphaNum <|> char '_'
and use the objIdentifier to match object types. But I can't get it to work. 	

update operators (+=, *= etc) not yet supported

for-loop

lambda

list

enum

the < sign is still buggy: Matrix<8,8> is OK but vector<8> is wrong

Bugs in parsing of dyn_reconf_buf.gc
--}

module GannetC.Parser (
	prettyGannetCFromFile,
	parseGannetC
) where

import GannetC.AST
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
--import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 

--import Text.Parsec hiding (State)
--import Text.Parsec.Expr
----import Text.ParserCombinators.Parsec.Char
--import qualified Text.Parsec.Token as P
--import Text.Parsec.Language

prettyGannetCFromFile fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> do{ putStr "parse error at "
                           ; print err
                           }
--           Right x  -> print x
           Right (MkProg x)  -> putStrLn $ unlines (map show x)
      }
      
parseGannetC input =  case parse program "" input of
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
			(liftM BindE (try bindExpr)) 
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
        <|> try serviceCall 
        <|> try funAppl 
        <|> try varExpr 
        <|> liftM PLet letExpr
        <|> condExpr
        <|> whileExpr 
--        <|> forExpr 
--        <|> lambdaDef 
        <|> parens pureExpr
		<?> "atomicPureE"

funAppl = do
				fname <- identifier
				args <- parens (commaSep pureExpr)
				return $ PFunAppl (MkFunAppl fname (map PureE args) GCAny) 


serviceCall = do
				inst <- identifier
				dot
				meth <- identifier
				args <- parens (commaSep pureExpr)
				return $ PServiceCall (MkServiceCall inst meth (map PureE args) GCAny) 

--seqLet = do
--			reserved "seq"
--			exprs <- braces exprList
--			return $ MkLet Seq exprs 
--				
--parLet = do
--			reserved "par"
--			exprs <- braces exprList
--			return $ MkLet Par exprs
--			
--implicitParLet = do
--			exprs <- braces exprList
--			return $ MkLet Par exprs

--letExpr = 
--			seqLet        
--        <|> parLet
--        <|> implicitParLet
        
blockType =  keyword "par" <|> keyword "seq"       
letExpr = do
			bt <- many blockType
			exprs <- braces exprList
			return $ MkLet bt exprs GCAny			        

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
		vinit <- (liftM BindE bindExpr)
		semi
		vcond <- pureExpr
		semi 
		vmod <- expr
		return $ MkGuard vinit (PureE vcond) vmod 
								
returnExpr = reserved "return" >> pureExpr >>= \arg -> return $ PReturn arg
			 		
		
--exprList = do 
--			exprs <- sepEndBy1 expr semi
--			return exprs
			
--exprList = many1 expr
exprList = semiSepEnd1 expr			 
			 
-- -----------------------------------------------------------------------------
bindExpr :: Parser BindExpr
bindExpr = 
		try assignExpr 
	<|> try updateExpr
--	<|>	try opUpdateExpr
	<|> (liftM BTypeDef (try typeDef))
	<|> (liftM BFunDef funDef)
	<?> "bindExpr"
	
--assignExpr :: Parser BindExpr
assignExpr = do
		vartype <- oldType -- typeExpr
		varname <- identifier
		reservedOp "="
		rhs <- pureExpr
		return $ BAssign (MkAssign vartype varname (PureE rhs))
		

updateExpr = do		
		varname <- identifier
		reservedOp "="
--		symbol "="
		rhs <- pureExpr
		return $ BUpdate (MkUpdate varname (PureE rhs) GCAny)
		
opUpdateExpr = do		
		varname <- identifier
		op <- resOps		
		symbol "="		
		rhs <- pureExpr
		return $ BOpUpdate (MkOpUpdate varname op (PureE rhs) GCAny)				

funDef = do
		ftype <- typeExpr
		fname <- identifier
		fargs <- parens ((commaSep argExpr) <|> voidArgExpr)
		fbody <- letExpr
		return $ MkFunDef ftype fname fargs (PureE (PLet fbody))
		
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
				(liftM DServiceDecl serviceDecl)
			<|> (liftM DConfigDecl configDecl)
			<|> (liftM DIncludeDecl includeDecl)
			<|> (liftM DInstDecl (try instDecl))
			<?> "declExpr"
			
serviceDecl = do
				reserved "service"
				service <- identifier
				methoddecls <- braces (semiSepEnd1 methodDecl)
				return $ MkServiceDecl service methoddecls
				
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
								
instDeclArgs = do
			insttype <- oldType
			instname <- identifier
			instargs <- parens (commaSep pureExpr)
			return $ MkInstDecl insttype instname (map PureE instargs)
			
instDeclNoArgs = do
			insttype <- oldType
			instname <- identifier			
			return $ MkInstDecl insttype instname []		
			
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
		x <- identifier		
		return (PVar (MkVar x  GCAny))
		
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
		ltop = Infix ( try $ do { reservedOp "<";  return (\x y ->(POpCall (MkOpCall "<" [x,y] GCAny))) } ) AssocNone    
--		opupdate name = Infix ( do {  reservedOp name; return (\x y ->(BOpUpdate (MkOpUpdate x name (pureExpr y)))) } ) AssocRight                                                          	
	in
		[ --[ prefix "-", prefix "+" ]
		  [ postfix "++", postfix "--"]
--		, [opupdate "+=", opupdate "-=", opupdate "*=", opupdate "/=",opupdate "%=",opupdate "&=",opupdate "|=",opupdate "^="]      
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
	
				
-- The lexer
gannetcDef = emptyDef {
	  P.commentStart = "/*"
	, P.commentEnd = "*/"
	, P.commentLine = "//"
	, P.nestedComments = True
	, P.identStart = letter  <|> char '_'
	, P.identLetter = alphaNum <|> char '_'
	, P.opStart = P.opLetter emptyDef
	, P.opLetter = oneOf ":!$%&*+./<=>?^|-~"
	-- I'm not sure what 'reserved' actually means but I observe that '*' is never 
	-- included  
	-- reservedOpNames result in matching the rest of a string not the name, e.g.
	-- xandy => and y 
	, P.reservedOpNames= ["+","-","*","=","and","or","xor","bitand","bitor"]
--	, P.reservedOpNames= []
	, P.reservedNames = [
		"case",
		"default",
		"if", -- service
		"else",
		"switch",
		"while",
		"do",
		"for",
		"seq",
		"par",
		"goto",
		"continue",
		"break",
		"return", -- service
		"typedef",
		"lambda", -- service
		"configuration",
		"service",
		"void",
		"char",
		"int",
		"uint",
		"short",
		"long",
		"float",
		"double",
		"signed",
		"unsigned",
		"bool",
		"true",
		"false",
		"template",
		"string",
		"tuple",
		"data",
		"any",
		"word",
		"symbol",
--		"Buf",
--		"Gen",
--		"Lambda",
--		"Stream",
		"local",
		"const",
		"inline",
        "operator",
		"...",
		"#include",
		"#define",
		"#if",
		"#ifdef",
		"ifndef",
		"#else",
		"#elif",
		"#endif",
		"#pragma"
		--"extern",
		--"static",
		--"auto",
		--"register",
		--"volatile",
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


{-# LANGUAGE DeriveDataTypeable #-}

module GannetC.AST where 
-- Gannet-C Abstract Syntax Tree
import Data.Typeable
import Data.Generics hiding (Infix,Prefix)


-- data NONE = Nothing
data	Program = MkProg [Expr] deriving (Eq, Show, Typeable, Data)
data	Expr=     PureE PureExpr
			    | BindE BindExpr
    			| DeclE DeclExpr
    			| PureECtc PureExpr Bool -- Compile-time computable expressions
    			| BindECtc BindExpr Bool -- Compile-time computable expressions 
    			| DeclECtc DeclExpr Bool -- Compile-time computable expressions  
	deriving (Eq, Show, Typeable, Data)

data BindExpr=
          BAssign AssignExpr
        | BUpdate UpdateExpr
        | BOpUpdate OpUpdateExpr             
        | BFunDef FunDef
        | BTypeDef TypeDef
	deriving (Eq, Show, Typeable, Data)            

data AssignExpr = MkAssign 
    {
        a_type :: GCType,
        a_name :: String,
        a_rhs :: Expr -- was PureExpr            
    }
	deriving (Eq, Show, Typeable, Data)
	
data UpdateExpr = MkUpdate 
    {
        u_name::String,
        u_rhs::Expr,
        u_type::GCType
    }
	deriving (Eq, Show, Typeable, Data)
	    

data OpUpdateExpr = MkOpUpdate 
    {
        ou_vname::String,
        ou_op::String,
        ou_rhs::Expr,
        ou_type::GCType
    }
	deriving (Eq, Show, Typeable, Data) 
	
data FunDef = MkFunDef 
    {
        fd_type::GCType,
        fd_name::String,
        fd_argstypes::[FuncArg],
        fd_body::Expr -- Let
    }
	deriving (Eq, Show, Typeable, Data)

data FuncArg = Arg ArgTup
              | Void 
	deriving (Eq, Show, Typeable, Data)   
	           
data ArgTup = MkArgTup 
    {
        at_type::GCType,
        at_name::String
    }              
	deriving (Eq, Show, Typeable, Data)  
		
data TypeDef = MkTypeDef 
    {
        td_otype::GCType,
        td_ntype::GCType
    }
	deriving (Eq, Show, Typeable, Data)
-- -----------------------------------------------------------------------------

data DeclExpr = 
          DConfigDecl ConfigDecl           
        | DServiceDecl ServiceDecl 
        | DInstDecl InstDecl 
        | DUseDecl UseDecl -- Perl
        | DIncludeDecl String
        | DOpDecl OpDecl
        | DVarDecl VarDecl
--            | DInstDeclSpec InstDeclSpec
--            | DTypeDecl TypeDef
	deriving (Eq, Show, Typeable, Data)
	            	
data ConfigDecl = MkConfigDecl
    {
        cd_name::String,
--        cd_typedefs::[TypeDef]  
        cd_typedefs::[Expr] -- purely to make it accessible via Data.Generics, it's actually TypeDef  
    }
	deriving (Eq, Show, Typeable, Data)
	 
data ServiceDecl = MkServiceDecl
    {
        sd_name::String,
--        sd_opdecls::[OpDecl]
        sd_opdecls::[Expr] -- to make it accessible via Data.Generics            
    }
	deriving (Eq, Show, Typeable, Data)
-- Service s(args)
-- s = new Service(args)
data InstDecl = MkInstDecl
    {
        id_type::GCType, -- Service
        id_name::String, -- s
        id_args::[Expr] -- args
    }
	deriving (Eq, Show, Typeable, Data)

data VarDecl = MkVarDecl {vd_name:: String, vd_type:: GCType} deriving (Eq, Show, Typeable, Data)
--data GannetDecl = MkGannetDecl String
--    deriving (Eq, Show, Typeable, Data)

data UseDecl = MkUseDecl Bool String -- True for "use", False for "no"
    deriving (Eq, Show, Typeable, Data)
 
data SeqDecl = MkSeqDecl String
    deriving (Eq, Show, Typeable, Data)

data NoSeqDecl = MkNoSeqDecl String
    deriving (Eq, Show, Typeable, Data)
    
--data InstDeclSpec = MkInstDeclSpec
--    {
--        ostype::GCType, 
--        isname::String,
--        instid::Number
--    }
--	deriving (Eq, Show, Typeable, Data)                     

data BlockType = Seq | Par | Default
	deriving (Eq, Show, Typeable, Data)

-- -----------------------------------------------------------------------------	  
-- "pure" Expressions are function calls, including operator Expressions, or variable lookups 

data PureExpr =
-- operators via Parsec's built-in capabilities
          POpCall OpCall
        | PServiceCall ServiceCall
        | PFunAppl FunAppl
        | PLambdaDef LambdaDef
        | PReturn PureExpr
        | PCond Cond
        | PWhile While
        | PFor For
        | PForeach Foreach
		| PLet Let
		| PBegin Begin
        | PNumber Number
        | PString String
        | PRegex String -- for Perl
        | PVar Var
        | PPair Pair -- for Perl
        | PInstAlloc InstAlloc
	deriving (Eq, Show, Typeable, Data)
	
data Var = MkVar {v_name:: String, v_type:: GCType}	deriving (Eq, Show, Typeable, Data)
data Pair = MkPair {p_key:: Expr, p_value:: Expr, p_type::GCType}	deriving (Eq, Show, Typeable, Data)
data OpCall = MkOpCall 
    {
        oc_name::String, oc_args::[PureExpr], oc_type::GCType --TODO: make it Expr
    }
	deriving (Eq, Show, Typeable, Data)    
	
data ServiceCall = MkServiceCall
    {
         sc_name::String, sc_op::String, sc_args::[Expr], sc_type::GCType -- sc_args is  [PureExpr] but for Data.Generics, use Expr
    }
	deriving (Eq, Show, Typeable, Data)    

data InstAlloc = MkInstAlloc  {
  ia_type::GCType -- which is an Object 
 ,ia_args::[Expr] -- PureExpr
} deriving (Eq, Show, Typeable, Data)

data FunAppl = MkFunAppl  {
  fa_fname::String
 ,fa_args::[Expr] -- PureExpr
 ,fa_type::GCType 
}	deriving (Eq, Show, Typeable, Data)



data LambdaDef = MkLambdaDef  { 
	 ld_type::GCType 
	,ld_argstypes::[FuncArg] 
	,ld_body::Expr -- Let 
	}	
	deriving (Eq, Show, Typeable, Data)
	
data Cond = MkCond  { 
	c_pred::Expr -- PureExpr
, 	c_iftrue::Expr --Let
,	c_iffalse::Expr --Let
}	
	deriving (Eq, Show, Typeable, Data)
	
data Let = MkLet {
	l_blocktype::[String] -- was BlockType
,	l_body::[Expr]
,	l_type::GCType
}	
	deriving (Eq, Show, Typeable, Data)
-- Begin is not such a good name as
-- the (...,...) list is used in many places
-- and will only be a (begin ...) in some cases	
data Begin = MkBegin { -- b_ is taken by Basic
	bb_blocktype::[String] 
,	bb_body::[Expr]
,	bb_type::GCType
}	
	deriving (Eq, Show, Typeable, Data)
	

data While = MkWhile  { w_pred::Expr, w_body::Expr }	
	deriving (Eq, Show, Typeable, Data)

data For = MkFor { f_guard::Guard, f_body::Expr }	
	deriving (Eq, Show, Typeable, Data)

data Guard = MkGuard {f_init::Expr, f_cond::Expr, f_mod::Expr}
	deriving (Eq, Show, Typeable, Data)

data Foreach = MkForeach { fe_iterator::Expr, fe_list::Expr, fe_body::Expr }	
	deriving (Eq, Show, Typeable, Data)
	  
data Number = NInt Int | NFloat Float
	deriving (Eq, Show, Typeable, Data)                                                 

data GCType =    
			  GCBasic Basic
            | GCObj Obj
            | GCTemplObj TemplObj
            | GCYada 
            | GCFunc Func
            | GCAny
            | GCTypeCtc GCType Bool
	deriving (Eq, Ord, Show, Typeable, Data)
	
data TemplArgType = TArgT GCType | TArgN Int | TArgS String 
	deriving (Eq, Ord, Show, Typeable, Data)
	                    
data Obj = MkObj 
    {
    	o_typequal::[String],
--        o_namespace::[GCType],
        o_qtype::[String] -- Qual
    }               
	deriving (Eq, Ord, Show, Typeable, Data)
data TemplObj = MkTemplObj 
    {
    	to_typequal::[String],
--		to_namespace::[GCType],    
        to_qtype::[String],
        to_args::[TemplArgType]
    }
	deriving (Eq, Ord, Show, Typeable, Data)
data Func = MkFunc 
    {
        f_type::GCType,
        f_argtypes::[GCType]
    }
	deriving (Eq, Ord, Show, Typeable, Data)    

data Basic = MkBasic 
    {
         b_typequal::[String] -- was [TypeQual],
        ,b_type::UnqualBasicType
    }
	deriving (Eq, Ord, Show, Typeable, Data)    
--    basic_type     =   {int} int_type | {non_int} non_int_type 
data    UnqualBasicType     =   NumberT NumberType | OtherT OtherType 
	deriving (Eq, Ord, Show, Typeable, Data)
	
{--
Number types for Gannet are required to know if
- the integer must be extended or not
- the float takes one or two words
However, the Gannet compiler works this out, so we can actually simply say that
it's either an int or a float

data    NumberType =   NTInt | NTFloat 
	deriving (Eq, Show, Typeable, Data) 
On the other hand I'd like to keep some info, let's say:

-int or float
-nbytes  = char=1,short=2,int=4,long=8, long long = 16, float = 4, double = 8)
- if int: signed, unsigned, bool	
--}	
data    NumberType =   CIntT CIntType | SimpleT SimpleNumType
	deriving (Eq, Ord, Show, Typeable, Data) 
data    OtherType =   MkOtherType String
	deriving (Eq, Ord, Show, Typeable, Data)
                   
    
data CIntType       =  MkCIntType 
    {
         signedness::[String] -- Signedness,
        ,spec::[String] -- IntTypeSpec 
    }
	deriving (Eq, Ord, Show, Typeable, Data)    
    --int_type       =   type_qual? signedness? int_type_spec? int
data IntTypeSpec  = Short | Long | LongLong | Normal
	deriving (Eq, Ord, Show, Typeable, Data)
    --long_long      = {long_long} long long 
data Signedness     = Signed | Unsigned
	deriving (Eq, Ord, Show, Typeable, Data)
data TypeQual     = Const | Local 
	deriving (Eq, Ord, Show, Typeable, Data) 
data SimpleNumType = MkSimpleNumType String
	deriving (Eq, Ord, Show, Typeable, Data)                   

-- these are the declarations of operations supported by a service
-- they are similar to C/C++ function declarations    
data OpDecl = MkOpDecl 
    {
         od_name::String,         
         od_type::GCType
    }
	deriving (Eq, Show, Typeable, Data)    

getType :: Expr -> GCType    
getType   (DeclE (DInstDecl ide)) = id_type ide
getType   (DeclE (DVarDecl vd)) = vd_type vd
getType   (BindE (BAssign  ae)) = a_type ae
getType   (BindE (BUpdate  ue)) = u_type ue
getType   (BindE (BOpUpdate  oue)) = ou_type oue
getType   (BindE (BFunDef  fd)) = fd_type fd
getType   (PureE ( PVar  v)) =	v_type v
getType   (PureE ( POpCall  oc)) = oc_type oc
getType   (PureE ( PFunAppl  fa)) = fa_type fa
getType   (PureE ( PServiceCall sc))=sc_type sc
getType   (PureE ( PLambdaDef ld))=ld_type ld	
getType   (PureE ( PReturn pe)) = getType (PureE pe)
getType   (PureE ( PCond c))= getType (c_iftrue c) -- FIXME!!
getType   (PureE ( PWhile w))= getType (w_body w)
getType   (PureE ( PFor f))= getType (f_body f)
getType   (PureE ( PForeach fe))= getType (fe_body fe)
getType   (PureE ( PLet l))=l_type l
getType   (PureE ( PBegin bb))=bb_type bb
getType   (PureE ( PNumber (NInt _)))= GCBasic (MkBasic [] (NumberT (SimpleT (MkSimpleNumType "Int")))) 
getType   (PureE ( PNumber (NFloat _)))= GCBasic (MkBasic [] (NumberT (SimpleT (MkSimpleNumType "Float"))))
getType   (PureE ( PString s))= GCBasic (MkBasic [] (OtherT (MkOtherType "String")))
getType   (PureE ( PPair p))=p_type p
getType   (PureE ( PInstAlloc ia))= ia_type ia
getType   e = GCAny 
        
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

module Main where

import Language.Haskell.TH
import GannetC.GenInstances (genInstances)
import GannetC.AST


$(genInstances ''Program)

prog  :: Program
prog = MkProg [expr]
expr :: Expr 
expr = PureE $ var
assign :: AssignExpr
assign = MkAssignExpr GCYada "x" var
var :: PureExpr
var = PVar "Hello"

main = do 
        runQ [d| data Program = MkProg [Expr] |] >>= print
--        runQ [d| data Expr = PureE PureExpr | BindE BindExpr | DeclE DeclExpr deriving (Eq, Show) |] >>= print
--        runQ [d| data BindExpr= BAssign AssignExpr | BUpdate UpdateExpr | BOpUpdate OpUpdateExpr |] >>= print    
--        runQ [d| data PureExpr = POpCall OpCall | PLet Let | PString String   |] >>= print 
--        runQ [d| data AssignExpr = MkAssignExpr { a_vartype :: GCType, a_vname :: String, a_rhs :: PureExpr } |] >>= print
--        putStrLn $ show ( nameModule ''String == Just "GHC.Base")
        putStrLn $ emit prog
        putStrLn $ emit expr
        putStrLn $ emit var
        putStrLn $ emit assign
        -- runQ ( genInstance "EmitG" "emit" "PureExpr" ) >>= putStrLn.pprint        
--        runQ ( [t| PureExpr |] ) >>= print
--        putStrLn $ show ''PureExpr 

        
-- instance EmitG PureExpr
--     where emit = emitPureExpr
emitFloat = show
emitInt = show
emitTypeDef = show
emitFunDef = show
emitOpUpdateExpr = show
emitVar = show
emitNumber = show
emitLet = show
emitFor = show
emitWhile = show
emitCond = show
emitLambdaDef = show
emitFunAppl = show
emitServiceCall = show
emitOpCall = show
emitInstDecl = show
emitServiceDecl = show
emitConfigDecl = show
emitDeclExpr = show
emitBindExpr = show
emitExpr = show
emitProgram = show
emitPureExpr = show
emitAssignExpr = show
emitUpdateExpr = show

--        runQ [| PureExpr |] >>= print
--        runQ [| EmitG PureExpr |] >>= print
--        runQ [| emit |] >>= print
--        runQ [| emitPureExpr |] >>= print
--        runQ (return $ InstanceD [VarT (mkName "EmitG")] (VarT (mkName "PureEpxr")) [(FunD (mkName "emit") [(Clause [] (NormalB (VarE (mkName "emitAssignExpr"))) [])] )] ) >>= print
--        runQ (return $ genInstance "EmitG" "emit" "PureExpr" ) >>= putStrLn.pprint
-- instance EmitG PureExpr where emit = emitPureExpr |] >>= print




-- context could be e.g. VarT (mkName "Typeable")
--genInstance cname fname tname  = InstanceD [] (AppT (VarT (mkName cname )) (VarT (mkName tname))) [(FunD (mkName fname) [(Clause [] (NormalB (VarE (mkName (fname++tname)))) [])] )]




{--
$(genInstanceT "EmitG" "emit" ''AssignExpr)
$(genInstanceT "EmitG" "emit" ''UpdateExpr)
$(genInstanceT "EmitG" "emit" ''PureExpr)
--}

-- import Control.Monad (liftM)
-- import Control.Monad.State

{--
main =  do
            sth <- runQ (getConstructors ''Expr)
            print sth
            putStrLn $ pprint sth
            putStrLn $ show sth
--}

{--
main1 = putStrLn $ show names            

names :: [Name]            
names =  getConstructorNames []  tyinfo         

--}


{--

The proposed approach:
- give the function a type
- it gets reified in to an Info structure
- extract all the constructor names from the constructors list of this structure; add them to the nameslist
- reify each of those
- apply the function on these new structures, always add the names to the nameslist
- stop when the list of constructors does not contain a NormalC, return the nameslist

--}            

{--
-- getConstructors :: Info       
tyinfo =  TyConI (DataD [] name varbndr constructors [])     
    where
        name = ''Expr
        varbndr = []
        constructors = [(NormalC 'PureE
                        [(NotStrict,ConT ''PureExpr)]), 
                        (NormalC 'BindE
                        [(NotStrict,ConT ''BindExpr)])]           

tyinfo2 name =  TyConI (DataD [] name varbndr constructors [])     
    where
        varbndr = []
        constructors = [(NormalC 'BAssign
                        [(NotStrict,ConT ''AssignExpr)])
                        ] 
        
getConstructorNames :: [Name] -> Info -> [Name] 
getConstructorNames nl t
--do
-- Dec : DataD Cxt Name [TyVarBndr] [Con] [Name]
--       DataD Cxt Name [Name] [Con] [Name]
-- Data Expr = ExprP PureExpr | ExprB BindExpr
-- Ctxt is []
-- Name = Expr
-- [Name] = [ExprP,ExprB] I guess
-- [Con] = [PureExpr,BindExpr] I guess; but it could be NormalC ExprP PureExpr
-- [Name] = [], this is the "deriving ..." info

-- Data AssignExpr = MkAssignExpr A B C 
-- Name = AssignExpr
-- [TyVarBndr] = MkAssignExpr
-- [Con] = [(A,B,C)] I guess, so that would be RecC 
    | length constructors > 1 =
        let
            nt = map getName constructors
            ntt= map tyinfo2 nt
        in       
            mapWithState nnl (getConstructorNames [])  ntt
    | otherwise =  nnl
    where
        TyConI (DataD _ name varbndr constructors _) = t --  <-  reify t       
        nnl = name:nl
    
mapWithState :: [Name] -> (Info->[Name]) -> [Info] -> [Name] 
mapWithState st f tl 
    | length tl == 0 = st
    | otherwise =
        let
            x:xs=tl
            nst = f x 
        in
            mapWithState (nst++st) f xs            
            
getName (NormalC _ [(NotStrict,ConT n)]) = n
getName _ = mkName "NONE"   
           
--}   
  
{--
data Con 	

NormalC Name [StrictType]	
RecC Name [VarStrictType]	
InfixC StrictType Name StrictType	
ForallC [TyVarBndr] Cxt Con	

--}  
{--
  let showClause (NormalC name fields) = do
        let constructorName = nameBase name
        -- Get variables for left and right side of function definition
        (pats,vars) <- genPE (length fields)
        -- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
        let f []       = [| "" |]
            f (v:vars) = [| " " ++ show $v ++ $(f vars) |]
        -- Generate function clause for one constructor
        clause [conP name pats]                                 -- (A x1 x2)
               (normalB [| constructorName ++ $(f vars) |]) []  -- "A "++show x1++" "++show x2

  -- Make body for function `show`:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  --   show (B x1)    = "B "++show x1
  --   show C         = "C"

  showbody <- mapM showClause constructors
--}
  
--import GannetC.Emitter

-- instance EmitG AssignExpr where emit = emitAssignExpr

-- # Perl
-- eval("instance EmitG $type where emit = emit$type")

-- Template Haskell

-- $( genInstance "EmitG" "emit" "PureExpr" )


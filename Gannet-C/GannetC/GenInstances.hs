module GannetC.GenInstances (
--genInstanceStr,
--genInstance,
genInstancesStr,
genInstances
) where 
import Data.List (nub)
{--

Template Haskell code to generate typeclass instances of a particular format

instance <TypeClass> <TypeInst> where <method> = <method><TypeInst>

The most complex part is to read the AST Types 
--}

import Language.Haskell.TH
import GannetC.InstTypes (typeClassName, methodName, instTypeNames)

genInstances :: Name -> Q [Dec]
genInstances topctype = do
    cnames <- getConstructorNames [] topctype
    mapM (genInstance typeClassName methodName) (nub cnames)


-- note that $(genInstanceT	...) will not work, the $() expects Q [Dec], not Q Dec	
genInstance :: String -> String -> Name -> Q Dec
genInstance cname fname t  = do
    TyConI (DataD _ tname varbndr constructors _) <-  reify t    
    let
	    ctxt = [] -- the Context is the list of types to predicate the type class e.g. Data, Show => Emit
	    tclass = ConT (mkName cname ) -- the actual type class, e.g. Emit
	    inst = ConT tname -- the instance type, e.g. PureExpr
	    meth = FunD (mkName fname) [clause] -- the method defined in the type class
	    -- here we assume definitions of the form emit = emitPureExpr, so the clause
	    -- Clause = Clause [Pat] Body [Dec]
	    -- becomes simply a Body
	    clause = Clause [] (NormalB (VarE (mkName (fname++(nameBase tname))))) []		
    return $  InstanceD [] (AppT tclass inst) [meth]
    
    
getConstructorNames :: [Name] -> Name -> Q [Name] 
getConstructorNames tnames tname = do
	if tname `elem` tnames
		then
			return tnames
		else
			do 
			    tinfo <- reify tname    
			    case tinfo of
			        TyConI (DataD _ name _ constructors _) -> do
			            let 
			                ntnames = (name:tnames) -- this is OK for Program
			                mcnames = map getName constructors -- so this must go wrong?
			                nmcnames =filter (\x->(x/= Nothing)) mcnames     
			                ncnames = map (\x-> (case x of {Just n -> n})) nmcnames
			                ncnames_nr = filter (\x->(x/=name)) ncnames          
			            if  (length ncnames_nr > 0)             
			                then
			                    do
			                        nntnames <- mapWithStateQ ncnames_nr (getConstructorNames []) (return ntnames)
			                        return nntnames
			                else
			                    return ntnames
			        otherwise -> return tnames                    
        
mapWithStateQ :: [Name] -> (Name -> Q [Name]) -> Q [Name]-> Q [Name] 
mapWithStateQ tl f st 
    | length tl == 0 =  st -- returns Q [Name]
    | otherwise = do
        let
            x:xs=tl
            nst = f x -- this returns Q [Name] ; we need [Name]
        (mapWithStateQ xs f) (joinQs st nst) -- (st ++ nst)        

-- sometimes monads are a pain        
joinQs :: Q [Name] -> Q [Name] -> Q [Name]
joinQs l1 l2 = do
                l11 <- runQ l1
                l22 <- runQ l2
                return $ l11++l22

getName :: Con -> Maybe Name            
getName (NormalC _ nl) = case nl of
    (_,ConT n):_ -> Just n
--    (_,AppT ListT (ConT n)):_ -> Just n -- This is what it should be according to the output of runQ [d| data Program = MkProg [Expr] |] >>= print
    (_,AppT _ (ConT n)):_ -> Just n -- but the ListT does not match!
    otherwise -> Nothing
getName _ = Nothing   


-------------------------------------------------------------------
-- This is a simpler, less general approach, reading the data declaration strings from InstTypes
genInstancesStr :: Q [Dec]
genInstancesStr = mapM (genInstanceStr typeClassName methodName) instTypeNames

-- context could be e.g. ConT (mkName "Typeable")
genInstanceStr :: String -> String -> String -> Q Dec
--genInstance cname fname tname  = return $  InstanceD [] (AppT (ConT (mkName cname )) (ConT (mkName tname))) [(FunD (mkName fname) [(Clause [] (NormalB (VarE (mkName (fname++tname)))) [])] )]
genInstanceStr cname fname tname  = return $  InstanceD [] (AppT tclass inst) [meth]
	where
		ctxt = [] -- the Context is the list of types to predicate the type class e.g. Data, Show => Emit
		tclass = ConT (mkName cname ) -- the actual type class, e.g. Emit
		inst = ConT (mkName tname ) -- the instance type, e.g. PureExpr
		meth = FunD (mkName fname) [clause] -- the method defined in the type class
		-- here we assume definitions of the form emit = emitPureExpr, so the clause
		-- Clause = Clause [Pat] Body [Dec]
		-- becomes simply a Body
		clause = Clause [] (NormalB (VarE (mkName (fname++tname)))) []


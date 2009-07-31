{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}
-- |Fundamental Gannet types and utility functions for manipulating them.
module Gannet.SBA.Types (
-- * Types
-- ** TokenTree
TokenTree(..),
GannetToken(..),
GannetLabel(..),
GannetBuiltin(..),
-- ** SymbolTree
SymbolTree(..),
SymbolListHeader(..),
-- ** Gannet Symbol and fields
GannetSymbol(..),
GSymbolKind(..),
GMode(..),
GRegister,
GDataType(..),
GPacketType(..),
GannetName(..),
GVarName,
symbolToGVarName,
-- ** Gannet Packet etc
PacketList,
PacketRefList,
GannetPacket,
GannetHeader(..),
-- * Helper functions
addToken,
isTokenList,
stringInToken,
getSL,
getSLH,
getS,
maybeS,
isSL,
isS,
extendGS,
-- * Constants
emptyST,
emptySL,
emptySLH,
emptyTL,
emptyGH,
emptyGPL,
emptyGReg,
emptyGS,
nullGS,
emptyGT,
quoteGS,
errorGS,
debugGS,
GRetVal(..),
GSymbol
) where 
import qualified Data.Map as Hash
--------------------------------------------------------------------------------
-- 
--                    Types for Gannet
--
--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- |A simple type for the intermediate tree
-------------------------------------------------------------------------------
data TokenTree = Token GannetToken | TokenList [TokenTree]
    deriving (Ord,Eq)
showT :: TokenTree -> String
showT (Token name) = show name
showT (TokenList contents) = "(" ++ unwordsList contents ++ ")"
unwordsList :: [TokenTree] -> String
unwordsList = unwords . map showT
instance Show TokenTree where show = showT

addToken :: TokenTree -> TokenTree -> TokenTree
addToken (TokenList v1) (TokenList v2) = (TokenList (v1++v2)) 
addToken v1s@(Token v1) (TokenList v2) = (TokenList ([v1s]++v2))
addToken (TokenList v1) v2s@(Token v2) = (TokenList (v1++[v2s]))     
addToken v1s@(Token v1) v2s@(Token v2) = (TokenList [v1s,v2s])

isTokenList :: TokenTree -> Bool
isTokenList (TokenList t) = True
isTokenList (Token t) = False 

stringInToken :: TokenTree -> String
stringInToken (Token t) = show t
stringInToken (TokenList t) = ""

-- | Used in TokenTree and for the name field in GannetSymbol
data GannetToken = GannetTokenB GannetBuiltin | GannetTokenQ String | GannetTokenL GannetLabel | GannetTokenS GannetLabel 
    deriving (Ord,Eq)
showGT :: GannetToken -> String
showGT (GannetTokenB contents) = show contents
showGT (GannetTokenL contents) = show contents
showGT (GannetTokenS contents) = show contents
showGT (GannetTokenQ contents) = contents
instance Show GannetToken where show = showGT

emptyGT :: GannetToken
emptyGT = GannetTokenL (GannetLabelS "")
{-
GannetName: NOT USED
-if it's a Builtin, make sure we have the right encoding
-if it's a Label, it can be numerified
So apart from the redundant Quote, it's the same as GannetToken
-}
data GannetName = GannetNameB GannetBuiltin | GannetNameL GannetLabel
    deriving (Ord,Eq)
showGN :: GannetName -> String
showGN (GannetNameB contents) = show contents
showGN (GannetNameL contents) = "<" ++ (show contents) ++ ">"
instance Show GannetName where show = showGN

#if WORDSZ==64     
data GannetBuiltin = GannetBuiltinI Integer | GannetBuiltinF Double | GannetBuiltinS String
#elif WORDSZ==32
data GannetBuiltin = GannetBuiltinI Integer | GannetBuiltinF Float | GannetBuiltinS String
#endif
    deriving (Ord,Eq)
showGB :: GannetBuiltin -> String
showGB (GannetBuiltinS contents) = "\"" ++ contents ++ "\"" 
showGB (GannetBuiltinI contents) = show contents
showGB (GannetBuiltinF contents) = show contents
instance Show GannetBuiltin where show = showGB

data GannetLabel = GannetLabelS String | GannetLabelI Integer
    deriving (Eq,Ord)
showGL :: GannetLabel -> String
showGL (GannetLabelS s) = s
showGL (GannetLabelI i) = show i
instance Show GannetLabel where show = showGL

emptyTL :: TokenTree
emptyTL = (TokenList [])
-------------------------------------------------------------------------------
-- | The actual Gannet Symbol Tree
-------------------------------------------------------------------------------
data SymbolTree = Symbol GannetSymbol | SymbolList ([SymbolTree],SymbolListHeader)
showST :: SymbolTree -> String
showST (Symbol s) = (show s) ++ "\n"
showST (SymbolList (contents,slh)) = "(\n" ++ unwordsSList contents ++ ") "++(show slh)++"\n" 
unwordsSList :: [SymbolTree] -> String
unwordsSList = unwords . map showST
instance Show SymbolTree where show = showST

-- |Get the [SymbolTree] list out of a ([SymbolTree],SymbolListHeader) tuple
getSL :: SymbolTree -> [SymbolTree]
getSL st = 
    let 
        (SymbolList (sl,_))=st
    in 
        sl
 
-- |Unwrap a GannetSymbol from a SymbolTree               
getS :: SymbolTree -> GannetSymbol            
getS st =
    let 
        (Symbol s)=st
    in 
        s
-- | Not a real Maybe, returns emptyGS if the arg is not a GannetSymbol
maybeS :: SymbolTree -> GannetSymbol
maybeS st2 =
    case st2 of    
        Symbol gs -> gs
        otherwise -> emptyGS
        
isSL :: SymbolTree -> Bool
isSL st =
    case st of
        SymbolList st -> True
        otherwise -> False    

isS :: SymbolTree -> Bool    
isS st =
    case st of
        Symbol s -> True
        otherwise -> False         
-- |Get the SymbolListHeader list out of a ([SymbolTree],SymbolListHeader) tuple        
getSLH :: SymbolTree -> SymbolListHeader
getSLH st = 
    let 
        (SymbolList (_,gh))=st
    in 
        gh
        
emptySL :: SymbolTree
emptySL = (SymbolList ([],emptySLH))

emptyST = Symbol emptyGS
-- | Auxiliary type for the list header in a Symbol Tree
data SymbolListHeader =     MkSymbolListHeader
                        {                        
                        lto            :: GannetLabel
                        ,lfrom        :: GannetLabel
                        ,label         :: GannetSymbol
                        ,rlambda     :: Integer -- I thought this was used to identify expressions with lambda-vars, but it's broken                        
                        ,to_apply   :: Bool -- Looks like I might get a lot of redundancy: only expressions with lambda-vars need to be send to APPLY
                        ,tagged        :: Bool -- IIRC this is for use with LABEL
                        ,retval         :: GRetVal
                        }
showSLH :: SymbolListHeader -> String                    
showSLH slh= "[" ++ (show (lto slh)) ++ "," ++ (show (lfrom slh)) ++ ",<" ++ (show (label slh)) ++ ">,L:"++(show (rlambda slh)) ++ ","++(show (to_apply slh)) ++ "," ++ (show (tagged slh)) ++ "]<" ++ (show (retval slh)) ++ ">"
instance Show SymbolListHeader where show = showSLH
                     
emptySLH :: SymbolListHeader
emptySLH     = (MkSymbolListHeader (GannetLabelS "_") (GannetLabelS "_") emptyGS 0 False False (GSV (K_Unknown,T_x)) )

--------------------------------------------------------------------------------
-- | Gannet Symbol 
--------------------------------------------------------------------------------
data GannetSymbol = MkGannetSymbol 
                    {
                        kind        :: GSymbolKind,
                        datatype    :: GDataType,
                        ext         :: Int,
                        quoted      :: Int,                         
                        task        :: Int,
                        subtask     :: Integer,
                        name        :: GannetToken,
                        count       :: Integer,
                        lambda        :: Integer,
                        mode        :: GMode,
                        reg         :: GRegister                        
                    } 
    deriving (Eq,Ord)

data GMode = M_normal | M_var | M_buf  
    deriving (Show,Eq,Ord,Enum) -- assuming Acc | Cache are synonyms for Var, Stream for Buf and Get | Peek are requests

type GRegister = (GannetToken, Integer)
emptyGReg = (emptyGT,0)
    
showGS :: GannetSymbol -> String
showGS s =
    let 
         n = name s
         mode_reg
            | kind s == K_S =":<"++(show (mode s))++":"++(show (reg s))++">"
            | kind s == K_D =":<"++(show (mode s))++":"++(show (reg s))++">"
            | otherwise = ""
    in
        (showGSK (kind s)) ++ ":" ++ (showGDT (datatype s)) ++ ":" ++ (show (ext s)) ++":" ++ (show (quoted s)) ++ ":" ++ (show (task s)) ++ ":" ++ (show (subtask s)) ++ ":" ++ (show n) ++ "\t\t\t\t\t\t\t\t;C:" ++ (show (count s))  ++ ";L:" ++ (show (lambda s))++mode_reg 


instance Show GannetSymbol where show = showGS

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n = 2*(pow2 (n-1))

namebits :: Integer
namebits=16
namesize :: Integer
namesize=pow2 namebits -1

numeric=False
-- At the very very least we need B, R, A, L => 1,2,3,4 = 3 bits! 000->111
-- Alas, we also need K_D and K_C, so 4 bits
--                 0     1     2     3     4     5     6     7     8     9     10        11          12        13
data GSymbolKind = K_S | K_D | K_A | K_L | K_R | K_C | K_B | K_Q | K_E | K_X | K_Label | K_Unknown | K_Debug | K_Lref
    deriving (Show,Eq,Ord,Enum)
showGSK :: GSymbolKind -> String 
showGSK k
    | k==K_Unknown = "0"  
    | numeric = show $ fromEnum k     
    | otherwise = k'
        where 
            _:_:k'= show k   

{- | Gannet DataTypes

> i: Integer
> f: Float
> s: String
> b: Bool (unused. use T_i)
> d: Data
> q: Quote
> l: List
> L: Lambda
> x: Any

-}
--                0     1     2     3     4     5     6     7     8
data GDataType =  T_i | T_f | T_s | T_b | T_d | T_l | T_L | T_q | T_x | T_Error 
    deriving (Show,Eq,Ord,Enum)
showGDT :: GDataType -> String 
showGDT = show . fromEnum  
--    | numeric = show . fromEnum      
--    | otherwise = show  
    
emptyGS :: GannetSymbol 
emptyGS = MkGannetSymbol K_Unknown T_x    0 0 0 0 (GannetTokenL (GannetLabelS "_")) 0 0 M_normal emptyGReg
-- this symbol, numerified, should return 0
nullGS :: GannetSymbol
nullGS = MkGannetSymbol K_Unknown T_x  0 0 0 0 (GannetTokenB (GannetBuiltinI 0)) 0 0 M_normal emptyGReg

errorGS :: GannetSymbol
errorGS = MkGannetSymbol K_E T_x 0 0 0 0 (GannetTokenQ "ERROR") 1 0 M_normal emptyGReg
debugGS :: String -> GannetSymbol
debugGS msg = MkGannetSymbol K_Debug T_x 0 0 0 0 (GannetTokenQ msg) 1 0 M_normal emptyGReg
quoteGS :: GannetSymbol
quoteGS = MkGannetSymbol K_Q T_q 0 0 0 0 (GannetTokenQ "'") 1 0 M_normal emptyGReg -- quoteGS doesn't need task as it's a compiletime construct
-- rootGS = (GannetSymbol K_R T_x 0 0 0 0 (GannetTokenL "ROOT") 1 0)

isGannetBuiltin :: GannetToken -> Bool
isGannetBuiltin gt =
    case gt of
        GannetTokenB gtb -> True
        otherwise -> False
    
-- | Turn a Built-in into an Extended symbol.

-- WV11032008: we'll use the Subtask field to hold the number of words
extendGS :: GannetSymbol -> [GannetSymbol]
extendGS gs 
    | isGannetBuiltin (name gs) =
        let 
            GannetTokenB gtb =  name gs
            one=GannetTokenB (GannetBuiltinI 1)
            zero=GannetTokenB (GannetBuiltinI 0)
            uint16=False -- if True, uint16 values are not Extended            
        in
            case gtb of
                GannetBuiltinI gi -> 
                    if uint16 && ((gi>0 && gi<=namesize)|| gi<0) -- (gi<0 && gi> -1*namesize) simpler to only have uint16 in name field
                        then
                            [gs]
                        else -- integer value larger than field
                            let
                                gsn=gs{ext=1,name=zero,subtask=1}
                            in
                                gsn:[extGS gs]
                GannetBuiltinF gf -> 
                    let
                        
                        gsn=gs{ext=1,name=zero,subtask=1}
                    in
                        gsn:[extGS gs]
                GannetBuiltinS gstr ->
                    let 
                        (strlength,padding)= nwords gstr
                        npad=GannetTokenB (GannetBuiltinI padding)
--                        strl=GannetTokenB (GannetBuiltinI strlength)
                        gsn=gs{ext=1,name=npad,subtask=strlength}                        
                        strlist= map extGSstr (splitInWords gstr []) 
                    in 
                        gsn:strlist
    | otherwise = [gs]
    
extGS :: GannetSymbol -> GannetSymbol
extGS gs = emptyGS{kind=K_X,datatype=datatype gs,name=name gs}    

nwords :: String -> (Integer,Integer)
nwords str = 
    let 
        l = length str
#if WORDSZ==64  
        nbytes = 8   
#elif WORDSZ==32
        nbytes = 4
#endif
        m = mod l nbytes
    in
        (toInteger ((div (l-m) nbytes)+(signum m)),toInteger(nbytes-m))
        
splitInWords :: String -> [String] -> [String]
splitInWords str ws 
        | length str > nbytes =
            let
                (w,cs) = splitAt nbytes str 
            in
                splitInWords cs (ws++[w])
        | otherwise = ws++[str]
    where
#if WORDSZ==64  
        nbytes = 8   
#elif WORDSZ==32
        nbytes = 4
#endif

extGSstr w = emptyGS{kind=K_X,datatype=T_s,name=GannetTokenB (GannetBuiltinS w)}


-- symbolToWord :: GannetSymbol -> GannetWord

-- For keeping track of variable bindings
-- A variable is fully identified by kind, subtask and name. 
-- We specifically don't include the Datatype 
type GVarName = (GSymbolKind,Integer,GannetToken)
symbolToGVarName :: GannetSymbol -> GVarName
symbolToGVarName gs = (kind gs, subtask gs, name gs)

--------------------------------------------------------------------------------
-- Gannet Packet 
--------------------------------------------------------------------------------
type GannetPacket = (GannetHeader,[GannetSymbol])

data GannetHeader = MkGannetHeader
                    {
                     ptype        :: GPacketType
                    ,prio        :: Int
                    ,redir      :: Int                    
                    ,plength        :: Int
                    ,to            :: GannetLabel
                    ,return_to    :: GannetLabel
                    ,ack_to     :: GannetSymbol
                    ,return_as    :: GannetSymbol
                    }

showGH :: GannetHeader ->  String
showGH s = "\n========\n"++(showGPT (ptype s))++ ":" ++ (show (prio s))++ ":" ++ (show (redir s))++ ":" ++ (show (plength s)) ++ ":" ++ (show (to s)) ++ ":" ++ (show (return_to s)) ++"\n" ++ (show (ack_to s)) ++ "\n" ++ (show (return_as s)) ++ "\n------------\n"
instance Show GannetHeader where show = showGH 

data GPacketType = P_error | P_subtask | P_code | P_reference | P_request | P_data | P_mm | P_lookup | P_address | P_advertise
    deriving (Show,Eq,Ord,Enum)
showGPT :: GPacketType -> String 
showGPT k  
    | numeric = show . fromEnum $ k     
    | otherwise = k'
        where 
            _:_:k' = show k
                            
emptyGH :: GannetHeader 
emptyGH = (MkGannetHeader P_code 0 0 0 (GannetLabelI 0) (GannetLabelI 0) nullGS emptyGS)

--------------------------------------------------------------------------------
-- Gannet Task Description 
--------------------------------------------------------------------------------

type PacketList = Hash.Map GannetSymbol GannetPacket

emptyGPL :: PacketList
emptyGPL = Hash.empty

type PacketRefList = [(GannetSymbol,GannetPacket)]
------
--type TGSymbol = (String, Bool)
--
--data TSymbolTree = TSymbol TGSymbol | TSymbolList ([TSymbolTree],Bool)
--
--showTSVal :: TSymbolTree -> String
--showTSVal (TSymbol (name,q)) = name++":"++ show q
--showTSVal (TSymbolList (contents,q)) = "(" ++ unwordsTSList contents ++ ")" ++ ":" ++ show q
--
--unwordsTSList :: [TSymbolTree] -> String
--unwordsTSList = unwords . map showTSVal
--
--instance Show TSymbolTree where show = showTSVal
------
--------------------------------------------------------------------------------
-- Types to determine return value of a service
--------------------------------------------------------------------------------
 
data GRetVal = GSV GSymbol | GData | GAny deriving (Show,Eq,Ord)
--data GSymbol = GBI | GRef | K_Lref | GLex | GArg | GGen deriving (Show,Eq,Ord)
-- this maps to K_B | K_R | K_Lref | K_L | K_A | K_Unknown
type GSymbol = (GSymbolKind,GDataType) 
-- KLRef is new, and it's for compile-time use only
--type GData=[Word32]


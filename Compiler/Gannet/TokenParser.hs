{-# OPTIONS_GHC -cpp -DWORDSZ=32 #-}
-- | TokenParser exports parseGToken, which parses a token 
-- into a Label, Quote or Built-in
-- the Built-in is either Integer, Double or String
module Gannet.TokenParser (
parseGToken
) where
import Gannet.SBA.Types
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 
{- 
This is somewhat heavyweight for my purpose, but it does the job.
-}
parseGToken :: String -> TokenTree -> GannetToken
parseGToken input tree = case parse expr "gannet" input of
 	Left err -> (GannetTokenL (GannetLabelS ("ERROR: "++ input)))
 	Right val -> maybeService tree val

maybeService tree val
	| tree==(TokenList ([])) =
		let
			(GannetTokenL (GannetLabelS x))=val
			sval=(GannetTokenS (GannetLabelS x))
		in
			sval
	| otherwise = val

expr :: Parser GannetToken
expr  =  (try parseNumber) <|>  parseString <|> parseQuote <|> parseLabel 

parseString :: Parser GannetToken
parseString = 
	do
		x <- strlit
		return  $ GannetTokenB (GannetBuiltinS x)
--		return  $ GannetTokenB (GannetBuiltinS (x++"\0"))
                    
parseLabel :: Parser GannetToken
parseLabel =  do
    x <- other
    return (GannetTokenL (GannetLabelS x))

parseQuote :: Parser GannetToken
parseQuote = 
    do
        c <- char '\''
        return (GannetTokenQ [c])
                  
--with thanks to arthurb@cs.uu.nl and Andrew.Harris@jhuapl.edu 
parseNumber :: Parser GannetToken -- (Either Integer Double)
parseNumber =  do 
	s   <- sign
	num <- usignednum
	return (case num of
              Left  uint -> GannetTokenB (GannetBuiltinI (applySign s (fromInteger uint)))
#if WORDSZ==64
              Right uflt -> GannetTokenB (GannetBuiltinF (applySign s uflt))
#elif WORDSZ==32              
              Right uflt -> GannetTokenB (GannetBuiltinF (applySign s (doubleToFloat uflt)))
#endif              
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

				
-- The lexer
gannetdef = emptyDef
lexer       = P.makeTokenParser gannetdef    

strlit      = P.stringLiteral lexer
intp			= P.integer lexer
usignednum         = P.naturalOrFloat lexer
other         = P.identifier lexer <|> P.operator lexer 


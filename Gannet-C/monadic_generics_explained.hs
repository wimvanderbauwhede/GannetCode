module Main where
{-
Simple example of how to combine Data.Generics and the State monad
It is really quite simple: if we want to use the State monad,
the function that works on the datastructure is monadic, e.g. incr

The corresponding Generics functions are everywhereM and mkM
instead of everywhere and mkT

A point to watch out for is that the transformation function 
_must_ have the type 

a -> m s a

That means a -> m s b doesn't work, unlike in non-Generics use of the State monad
    
What happens in this example is:
- we keep a tuple (Int,String) as state
- we traverse a list of lists of Char
- for every Char we encounter, we increment the count in the state with 1+(ord ch)
and we append the Char to the string
- we run the State monad with initial state (0,"_")
- we print out the tuple
-}
import Control.Monad.State
import Data.Generics
import Data.Char

main :: IO ()
main = do
        putStrLn $ show count
        putStrLn $ show res

data StrList = MkList [String] 
    deriving (Ord, Eq, Show, Typeable, Data)

strs :: StrList 
strs = MkList ["a","bb","ccc","dddd"]

incr :: Char -> State (Int,String) Char
incr chr = do
            (ct,str) <- get
            put (ct+1+(ord chr),str++";"++[chr])
            return chr            

count :: (Int,String)
count = execState (everywhereM (mkM incr) strs) (0,"_")

-- without the monad

tf :: Char -> Char 
tf c = chr $ (ord c)+1

res :: StrList
res = everywhere (mkT tf) strs

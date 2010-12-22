module Main where

import Control.Monad.State

main = putStrLn $ show count
-- incr takes a String, updates a State of type Int and returns nothing
incr :: String -> State Int () 
incr str = do
        ct <- get
        put (ct+(length str))

-- incr takes a String, updates a State of type Int and returns a String
incr_ret_str :: String -> State Int String
incr_ret_str str = do
        ct <- get
        put (ct+(length str))
        return str

-- incr takes a String, updates a State of type Int and returns an Int
incr_ret_int :: String -> State Int Int
incr_ret_int str = do
        ct <- get
        put (ct+(length str))
        return ct
        
-- execState :: State Int [String] -> Int -> Int
-- mapM :: (String -> State Int String) -> [String] -> State Int [String]

-- runState returns (a,s) i.e. ([String],Int)
-- execState returns s, i.e. Int
-- evalState returns a, i.e. [String]
count :: Int
count = execState (mapM incr ["a","bb","ccc","dddd"]) 0




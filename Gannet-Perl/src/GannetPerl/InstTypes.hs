module GannetPerl.InstTypes (
typeClassName,
methodName,
instTypeNames
)where

import System.IO.Unsafe

typeClassName = "EmitG"
methodName = "emit"

-- open file
fname = "GannetPerl/AST.hs"
input = readFile fname
astLines = lines (unsafePerformIO input)
astWordLists = map words astLines
dataWordLists = filter (\ws ->(headUnlessEmpty ws == "data")) astWordLists
dataTypeList = map (\ws -> (ws !! 1)) dataWordLists
instTypeNames = map chop dataTypeList


headUnlessEmpty [] = ""
headUnlessEmpty ws = head ws

chop str  
    | last str == '=' = init str
    | otherwise = str

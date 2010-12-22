module GannetC.InstTypes (
typeClassName,
methodName,
instTypeNames
)where

import System.IO.Unsafe

typeClassName = "EmitG"
methodName = "emit"
-- for simple method
--instTypeNames = ["AssignExpr","UpdateExpr","PureExpr"]

-- open file
fname = "GannetC/AST.hs"
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


module Main where

import System.IO.Unsafe
-- open file

main = putStrLn $ show instTypes

fname = "GannetC/AST.hs"
input = readFile fname
astLines = lines (unsafePerformIO input)
astWordLists = map words astLines
dataWordLists = filter (\ws ->(headUnlessEmpty ws == "data")) astWordLists
dataTypeList = map (\ws -> (ws !! 1)) dataWordLists

firstW = map headUnlessEmpty  astWordLists

typeClass = "EmitG"
method = "emit"

instTypes :: [String]
instTypes = map chop dataTypeList
--instTypes = ["AssignExpr","UpdateExpr","PureExpr"]
headUnlessEmpty [] = ""
headUnlessEmpty ws = head ws

chop :: String -> String
chop str  
    | last str == '=' = init str
    | otherwise = str




{--
Program
Expr
PureExpr
Let
Cond
OpCall
FunAppl
ServiceCall
Number
BindExpr
AssignExpr
UpdateExpr
DeclExpr
GCType
--}

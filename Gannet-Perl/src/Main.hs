module Main where

import System.Environment
import System.Console.GetOpt
import Control.Monad.State

import GannetPerl.Parser ( prettyGannetPerlFromFile, parseGannetPerl )
import GannetPerl.AST
import GannetPerl.Emitter
import GannetPerl.TypeMapper (getTypeInfo, inferTypes, inferCtc)

main :: IO ()
main = do
    args <- getArgs
    (opts,inp) <- compilerOpts args
    if  (elem Help opts)||(inp==[])   -- (opts !! 0) ==  
        then
            do showHelp
        else
            do                
                let
                    infile:_=inp   
                prettyGannetPerlFromFile infile
                input <- readFile infile
                putStrLn "\n\n" 
                let
                    tree 
                       | elem Perl opts = parseGannetPerl input
                       | otherwise = parseGannetPerl input					   
                --					decls = getDecls tree
                --				putStrLn $ emit tree 
                --				putStrLn $ (foldl1 (\x y -> (x ++ "\n" ++y)) (map emit decls))	
                --				putStrLn $ "\nTypeMapper: "++ (show (testModState tree))
                let 
                    mctxt = getTypeInfo tree
                    mtree = inferTypes tree mctxt
--                    ctctree = inferCtc mtree mctxt					
                putStrLn $ "\nTypeMapper:\n"++ (show mctxt)
                putStrLn $ "\n\nType Inference:\n"++ (unlines $ map show ((\(MkProg el)->el) mtree))
--                putStrLn $ "\n\nCTC Inference:\n"++ (unlines $ map show ((\(MkProg el)->el) ctctree))  -- (show ctctree)
                putStrLn $ "\n; ========================================================\n"
                let
                	gannetcode = evalState (emit mtree) mctxt
                	outfile = ((init . init) infile)++"td" -- s/pl/td/
                putStrLn gannetcode
                writeFile outfile gannetcode

data Flag 
 = Verbose  | Version | Help | PPrint | PPrintNum | Warnings | Perl | Puffin | Petrel | Skua | Cormorant
 | Input String | Output String | Yaml String
   deriving (Show,Eq,Ord)

options :: [OptDescr Flag]
options =
 [ Option ['v']     ["verbose"]         (NoArg Verbose)        "be verbose"
 , Option ['V']         ["version"]         (NoArg Version)        "show version number"
 , Option ['h','?'] ["help"]            (NoArg Help)            "show some help"
 , Option ['p']     ["print"]            (NoArg PPrint)        "pretty-print the compiled task"
 , Option ['s']     ["show"]            (NoArg PPrintNum)        "show the compiled task (like -p but numeric)"
 , Option ['w']     ["warnings"]        (NoArg Warnings)        "show extra warnings"
 , Option ['o']     ["outfile"]        (ReqArg Output "FILE")        "output FILE"
 , Option ['Y']		["yaml"]		(ReqArg Yaml "FILE")		"YAML file"
 , Option ['5']     ["petrel"]        (NoArg Petrel)        "emit Perl5 code for Petrel"
 , Option ['6']     ["puffin"]        (NoArg Puffin)        "emit Perl6 code for Puffin"
 , Option ['S']     ["skua"]        (NoArg Skua)        "emit Scheme code for Skua" 
 , Option ['C']     ["cormorant"]        (NoArg Cormorant)        "emit C++ code"  
-- , Option ['H']     ["Haskell"]        (NoArg H)        "emit Haskell code for H"
 ]
 
--outp :: Maybe String -> Flag
--outp = Output . fromMaybe "stdout"

--ymlSBA :: Maybe String -> Flag
--ymlSBA = Yaml . fromMaybe "SBA.yml"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts args = 
   case getOpt Permute options args of
      (opts,inp,[]) -> return (opts,inp)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: gannetcc [-hpsvwV56SC] Gannet-Perl source file (.pl)"    
  
showHelp = do
    putStrLn "Usage: gannetcc [-hpsvwV56SC] [-Y ymlfile] Gannet-Perl source file (.pl)"
    putStrLn "    -h,-? : this message"
    putStrLn "    -p : pretty-print the compiled task and exit"        
    putStrLn "    -s : show the compiled task and exit (like -p but numeric)"     
    putStrLn "    -o outfile : optional output file (.tdc)"
    putStrLn "    -Y ymlfile : optional YAML input file (.yml)"
    putStrLn "    -v : verbose (NOT IMPLEMENTED)"
    putStrLn "    -w : warnings (NOT IMPLEMENTED)"
    putStrLn "    -V : version (NOT IMPLEMENTED)"
    putStrLn "    -5 : emit Perl5 code for Petrel"
    putStrLn "    -6 : emit Perl6 code for Puffin"
    putStrLn "    -S : emit Scheme code for Skua"
    putStrLn "    -C : emit C++ code"    
                
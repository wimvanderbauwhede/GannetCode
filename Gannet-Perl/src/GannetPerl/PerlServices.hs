{-
Perl Services, i.e. grouping of commands built into Perl 5
The main purpose is reverse lookup from command to the Service providing it
-}

module GannetPerl.PerlServices (
perlCommands,
perlServices,
perlService,
perlOpService
) where

import qualified Data.Map as Hash

-- Given a command or an operator, determine in which service it belongs.
-- The problem is that for operators, a key could have multiple values
-- e.g. + can be ALU.plus or FPU.plus, so we'll need a map of the types as well
perlOpService op ptype =
	let
		(service,table)
			| ptype == "int" = ("ALU",perlALU)
			| ptype == "bool" = ("ALU",perlALU) 
			| ptype == "float" = ("FPU",perlFPU)
			| ptype == "string" = ("String",perlString)
			| otherwise = error $ "Could not find service for type "++ptype
	 in
	 	case Hash.lookup op table of
			Just method -> service++"."++method
			Nothing -> error $ "Could not find "++op++" in "++service
 	
type PerlServiceMap = Hash.Map String String
emptyPS :: PerlServiceMap
emptyPS = Hash.empty

perlService cmd =
	case Hash.lookup cmd perlServices of
		Just service -> service
		Nothing -> ""

perlCommands = Hash.keys perlServices
		 
perlServices :: PerlServiceMap 
perlServices = Hash.fromList (foldl1 (++) (map (\(s,ms)-> zip ms (replicate (length ms) s)) perlServiceTable)) 

perlServiceTable = [
	("ALU", [
		"not","and","or","xor"
	])	
	,("Array", [
-- Functions for real @ARRAYs
		"pop", "push", "shift", "splice", "unshift"
-- Functions for list data
		,"grep", "join", "map", "reverse", "sort", "unpack" -- "qw//",	 
		,"scalar"
	])
	,("Hash", [
-- Functions for real %HASHes
		"delete", "each", "exists", "keys", "values"
	])
	,("IO", [
-- Input and output functions
		"binmode", "close", "closedir", "dbmclose", "dbmopen", "die",
		"eof", "fileno", "flock", "format", "getc", "print", "printf",
		"read", "readdir", "rewinddir", "seek", "seekdir", "select",
		"syscall", "sysread", "sysseek", "syswrite", "tell", "telldir",
		"truncate", "warn", "write"
	])
-- Regular expressions and pattern matching
--	,("PCRE", [
--		"m//", "pos", "quotemeta", "s///", "split", "study", "qr//"	
--	])
-- Numeric functions	
	,("Math", [
		"abs", "atan2", "cos", "exp", "hex", "int", "log", "oct", "rand",
		"sin", "sqrt", "srand"
	])
	,("String", [
-- Functions for SCALARs or strings
		"chomp", "chop"
--		, "chr", "crypt", "hex", "index", "lc", "lcfirst",
--		"length", "oct", "ord", "pack", "q//", "qq//", "reverse", "rindex",
--		"sprintf", "substr", "tr///", "uc", "ucfirst", "y///",
--		"eq","ne","lt","gt","le","ge","cmp"	
	])
	]

perlALU = Hash.fromList [
	 ("+","plus")
	,("-","minus")
	,("*","times")
	,("/","over") -- maybe not: e.g. 8/3 will always be a float 2.66667 in Perl, not 2
	,("**","pow")
	,("%","minus")
	,(">>","rshift")
	,("<<","lshift")
	,("<","lt")
	,(">","gt")
	,("==","eq")
	,("!=","ne")
	,("<=","lte")
	,(">=","gte")
	,("!","not")
	,("&&","and")
	,("||","or")
	,(">=","gte")
	,("&","bitand")
	,("|","bitor")
	,("^","bitxor")
	,("~","bitnot")
	]			
	
perlFPU = Hash.fromList [
	 ("+","plus")
	,("-","minus")
	,("*","times")
	,("/","over")
	,("**","pow")
	,("<","lt")
	,(">","gt")
	,("==","eq")
	,("!=","ne")
	,("<=","le")
	,(">=","ge")
	,("<=>","cmp")
	]
	
perlString = Hash.fromList [
	 (".","concat")
	 ]	
	 			
perlRE =  Hash.fromList [
	 ("=~","match")
	,("=~m","match")	 
	,("!~","nomatch")
	,("=~m","nomatch")
	,("=~s","subst")
	]
	
{-
	Functions for fixed length data or records
		"pack", "read", "syscall", "sysread", "syswrite", "unpack", "vec"

	Functions for filehandles, files, or directories
		"-X", "chdir", "chmod", "chown", "chroot", "fcntl", "glob",
		"ioctl", "link", "lstat", "mkdir", "open", "opendir", "readlink",
		"rename", "rmdir", "stat", "symlink", "sysopen", "umask", "unlink",
		"utime"
		
	   Keywords related to the control flow of your Perl program
	       "caller", "continue", "die", "do", "dump", "eval", "exit", "goto",
	       "last", "next", "redo", "return", "sub", "wantarray"
	
	   Keywords related to scoping
	       "caller", "import", "local", "my", "our", "package", "use"
	       
	Miscellaneous functions
	  "defined", "dump", "eval", "formline", "local", "my", "our",
	  "reset", "scalar", "undef", "wantarray"

	Functions for processes and process groups
	 "alarm", "exec", "fork", "getpgrp", "getppid", "getpriority",
	 "kill", "pipe", "qx//", "setpgrp", "setpriority", "sleep",
	 "system", "times", "wait", "waitpid"
	
	Keywords related to perl modules
	 "do", "import", "no", "package", "require", "use"
	
	Keywords related to classes and object-orientation
	 "bless", "dbmclose", "dbmopen", "package", "ref", "tie", "tied",
	 "untie", "use"
	
	Low-level socket functions
	 "accept", "bind", "connect", "getpeername", "getsockname", 
	 "getsockopt", "listen", "recv", "send", "setsockopt", "shutdown",
	 "socket", "socketpair"
	
	System V interprocess communication functions
	 "msgctl", "msgget", "msgrcv", "msgsnd", "semctl", "semget",
	 "semop", "shmctl", "shmget", "shmread", "shmwrite"
	
	Fetching user and group info
	 "endgrent", "endhostent", "endnetent", "endpwent", "getgrent",
	 "getgrgid", "getgrnam", "getlogin", "getpwent", "getpwnam", 
	 "getpwuid", "setgrent", "setpwent"

       Fetching network info
           "endprotoent", "endservent", "gethostbyaddr", "gethostbyname",
           "gethostent", "getnetbyaddr", "getnetbyname", "getnetent", 
           "getprotobyname", "getprotobynumber", "getprotoent", "getservbyname",
           "getservbyport", "getservent", "sethostent", "setnetent", 
           "setprotoent", "setservent"

       Time-related functions
           "gmtime", "localtime", "time", "times"

-}				
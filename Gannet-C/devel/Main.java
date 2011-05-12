/*
 * WV: a parser for Gannet-C, emitting Gannet
 * */


import java.io.FileReader;
import java.io.PushbackReader;
import java.io.BufferedReader;

import GannetC.parser.*;
import GannetC.lexer.*;
import GannetC.node.*;

/*
 We have to make this more modular and smarter: use separate passes for
 - type extraction
 - AST transformations (e.g. function -> service, let -> begin, let -> return, apply quoting
 - then emit in a straightforward way 
 */

public class Main {

	public static void main(String arguments[]) {

		boolean verbose = false;
		if (arguments.length < 1) {
			System.out.println("usage:");
			System.exit(1);
		}

		if (arguments.length == 2) {
			if (arguments[1].contentEquals("-v")) {
				verbose = true;
			}
		}
//		FileOutputStream asmout; // declare a file output object
//		PrintStream asmprinter; // declare a print stream object

		try {
			FileReader infile = new FileReader(arguments[0]);
			// Lexer l = new Lexer (new PushbackReader (new BufferedReader(new
			// InputStreamReader (System.in))));
			Lexer l = new Lexer(new PushbackReader(new BufferedReader(infile),
					1024));
			Parser p = new Parser(l);
			// Parser p = new Parser(new Lexer(new PushbackReader(new
			// InputStreamReader(System.in), 1024)));
			Start tree = p.parse();

			/*
			 * 1. Determine types of declarations, put in table
			 * The complication is that we need to make sure the scope is correct 
			 * So how do we track the scope? We need the stack of enclosing blocks.
			 * In Gannet-C, blocks that give scope are 
			 * -function/procedure definitions 
			 * -if-else 
			 * -let 
			 * -while 
			 * -for 
			 * -lambda, which in Gannet-C is simply a function declaration that can be bound
			 * -service and configuration declarations give scope to what is enclosed
			 */
						  
			 /* 
			 * Every time we encounter a block we push its ID on the stack;
			 * every time we leave a block, ditto => Is that required? Surely we
			 * know which block we're in; What we do in Haskell is look up the
			 * variable in the current block's scope; if it's not there, we
			 * check what enclosing is and look up there; and so on. So we need
			 * the stack simply for that purpose. I think it might make sense to
			 * put the typemap in the Context
			 */

			Context context = new Context();
			TypeMapper typemapper = new TypeMapper(context,verbose);
			tree.apply(typemapper);
			// if (verbose == true) {
			// Pretty print transformed code
			Emitter emitter = new Emitter(verbose);
			tree.apply(emitter);
			// }

		} catch (Exception e) {
			// throw new RuntimeException("\n"+e.getMessage());
			e.printStackTrace();
		}
	}
}

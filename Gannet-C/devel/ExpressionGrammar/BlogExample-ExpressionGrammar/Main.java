/*
 * WV: a expression parser/calculator 
 * */

import java.io.FileReader;
import java.io.PushbackReader;
import java.io.BufferedReader;

import Expression.parser.*;
import Expression.lexer.*;
import Expression.node.*;

public class Main {

	public static void main(String arguments[]) {

		boolean verbose = false;
		if (arguments.length < 1) {
			System.out.println("usage:");
			System.out.println("java Main sourcefile (.gc)  ");
			System.exit(1);
		}

		if (arguments.length == 2) {
			if (arguments[1].contentEquals("-v")) {
				verbose = true;
			}
		}

		try {
			FileReader infile = new FileReader(arguments[0]);
			Lexer l = new Lexer(new PushbackReader(new BufferedReader(infile),
					1024));
			Parser p = new Parser(l);

			Start tree = p.parse();


			Calculate calc = new Calculate();
			tree.apply(calc);
		

		} catch (Exception e) {
			// throw new RuntimeException("\n"+e.getMessage());
			e.printStackTrace();
		}
	}
}

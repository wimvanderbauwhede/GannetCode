/*
 * A simple example for emitting Gannet code from a C-style function definition
 *
 * (c) Wim Vanderbauwhede 2010
 *
 * */

import java.io.FileReader;
import java.io.PushbackReader;
import java.io.BufferedReader;

import CFundef.parser.*;
import CFundef.lexer.*;
import CFundef.node.*;

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

		try {
			FileReader infile = new FileReader(arguments[0]);
			Lexer l = new Lexer(new PushbackReader(new BufferedReader(infile), 1024));
			Parser p = new Parser(l);
			Start tree = p.parse();
			Emitter emitter = new Emitter(verbose);
			tree.apply(emitter);
		} catch (Exception e) {
			throw new RuntimeException("\n"+e.getMessage());
			// e.printStackTrace();
		}
	}
}

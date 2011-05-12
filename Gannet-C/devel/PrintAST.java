
import GannetC.parser.*;
import GannetC.lexer.*;
import GannetC.node.*;
import GannetC.analysis.*;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Stack;
/**
 * Pretty print the AST 
 */

public class PrintAST extends DepthFirstAdapter {
    private Map<String, Integer> services = new HashMap<String, Integer> ();
/*
  if parent is in quote map, quote 
 */
    private  Map<Node, Integer> quote = new HashMap<Node, Integer> ();
    private int ntabs=0;
    
    public PrintAST () {
        // So here we must get all services and aliases from YAML
        services.put("s1", 1);
        services.put("s2", 1);
        services.put("s3", 1);
//        services.put("s1", 1);
        
    }

    public void outAProgram(AProgram prog) {    
    	System.out.println("; AProgram");
    }

    public void inAPureExpr(APureExpr fndecl) {
        indent();
    	System.out.println("; APureExpr: "+fndecl.getPureExpr().toString());
    }
    public void outAPureExpr(APureExpr fndecl) {}
/*
    public void inAServicecallPureExpr(AServicecallPureExpr node)
    {
        System.out.println("BOOM! ("+node.getSname().toString());
        emit("; "+node.getSargs().toString());
    }

    public void outAServicecallPureExpr(AServicecallPureExpr node)
    {
        
        indent();   
        System.out.println(")");
        ntabs--;
    }
*/
    public void inAOpcallPureExpr(AOpcallPureExpr node)
    {
        indent();
        System.out.println("; Operator Call");
        emitq(node,"("+node.getOp().toString());
    }

    public void outAOpcallPureExpr(AOpcallPureExpr node)
    {
        indent();
        System.out.println(")");
        ntabs--;
    }    
    
    public void inANumberPureExpr(ANumberPureExpr node)
    {        
        indent();
        System.out.println("; Number:");
        emit("'"+node.getNumber().toString());
    }

    public void outANumberPureExpr(ANumberPureExpr node)
    {
        ntabs--;
    }
    
    public void inAVarPureExpr(AVarPureExpr node)
    {
        indent();
        System.out.println("; Variable:");
        emit(node.getIdentifier().toString());
    }

    public void outAVarPureExpr(AVarPureExpr node)
    {
        ntabs--;
    }
    
    public void inAProcapplPureExpr(AProcapplPureExpr node)
    {
        indent();
        System.out.println("; Procedure Call");
        emitq(node,node.getFname().toString());
    }

    public void outAProcapplPureExpr(AProcapplPureExpr node)
    {
        ntabs--;
    }

    public void inAFunapplPureExpr(AFunapplPureExpr node)
    {
        if (services.containsKey(node.getFname().toString().trim())) { 
            indent();
            System.out.println("; Service Call");
            emitq(node,"("+node.getFname().toString());
        } else {
            indent();
            System.out.println("; Function Application");
            emitq(node,"(apply "+node.getFname().toString());
            // apply is complicated: numerical service should not be quoted, others should be quoted.
            // so it is not enough to say "quote all args of apply"
            // we should say, if apply, look at the type of the argument; if it's a number or a symbol, don't quote
            // otherwise quote
        }
    }

    public void outAFunapplPureExpr(AFunapplPureExpr node)
    {
        
        indent();
        System.out.println(")");
        ntabs--;
    }

    
    
    public void inAParletPureExpr(AParletPureExpr node)
    {
        System.out.println("; Parallel LET "+node.hashCode());
        emitq(node,"(let ");
    }

    public void outAParletPureExpr(AParletPureExpr node)
    {   
        
        indent();
        System.out.println(")");
        ntabs--;
    }

    public void inASeqletPureExpr(ASeqletPureExpr node)
    {
        indent(); 
        System.out.println("; Sequential LET");
        emitq(node,"(let* "); // FIXME: need a stack for quoting        
        quote.put(node, 1);
    }

    public void outASeqletPureExpr(ASeqletPureExpr node)
    {
        
        indent();
        System.out.println(")");
        ntabs--;
//        quotes.pop();        
    }

    public void inAAssignBindExpr(AAssignBindExpr node)
    {
        indent();
        System.out.println("; Assignment " );
        emitq(node,"(assign::"+node.getAvartype().toString()+" '"+node.getVname().toString()); // FIXME: need quoting here too        

    }

    public void outAAssignBindExpr(AAssignBindExpr node)
    {
        indent();
        System.out.println(")");
        ntabs--;
    }
    
    
// ------------------------------------------------
    
    private void emitq(Node node, String exprstr) {
        ntabs++;
        indent();        
        System.out.println(maybeQuote(node)+exprstr);
    }

    private void emit(String exprstr) {
        ntabs++;
        indent();        
        System.out.println(exprstr);
    }    
    
    private String maybeQuote(Node node) {
        if (quote.containsKey(node.parent().parent())) {
            return "'";
        } else {
            return "";
        }
    }

    public String toString() {
	return  new String();
    }

    public String getStr(Node node) {
	return "";
    }

    private String joinList(LinkedList<Node> list, String join) {
    	return new String();
    }
    private void indent() {
    	for (int i=0;i<ntabs;i++) {
    		System.out.print("    ");
    	}
    }
}

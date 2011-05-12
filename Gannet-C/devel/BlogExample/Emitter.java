/*
 * A simple example for emitting Gannet code from a C-style function definition
 *
 * (c) Wim Vanderbauwhede 2010
 *
 * */

import CFundef.node.*;
import CFundef.analysis.*;

import java.util.*;

/**
 * Emitter for Gannet code of C-style function definition
 */

public class Emitter extends DepthFirstAdapter {
    private int ntabs=0;
    private int nl=1;
    private boolean verbose=false;
    
    public Emitter (boolean verbose) {
        this.verbose=verbose;        
    }
    public void inAProgram(AProgram prog) {
        ntabs--;
        annotate("; AProgram");
        emit("(let");
    }

    public void outAProgram(AProgram prog) {    
    	emitcp("End of program");
    }
// ---------------------------------------------------------------
    public void inAPureExpr(APureExpr fndecl) {
        annotate("; APureExpr: "+fndecl.getPureExpr().toString());
    }
    public void outAPureExpr(APureExpr fndecl) {}
 // ---------------------------------------------------------------
    
    
    public void inANumberPureExpr(ANumberPureExpr node)
    {        
        annotate("; Number:");
        emitnnl("'"+node.getNumber().toString());
    }

    public void outANumberPureExpr(ANumberPureExpr node)
    {
        ntabs--;
    }
    
    
    public void inAVarPureExpr(AVarPureExpr node)
    {
        annotate("; Variable:");        
        emitnnl(node.getIdentifier().toString());        
    }

    public void outAVarPureExpr(AVarPureExpr node)
    {
        ntabs--;
    }
    

    public void inACondPureExpr(ACondPureExpr node)
    {
        annotate("If-Else");
        emit("(if"); 
    }

    public void outACondPureExpr(ACondPureExpr node)
    {
        emitcp("If-Else");
    }
    
    public void caseACondPureExpr(ACondPureExpr node)
    {
        inACondPureExpr(node);
        if(node.getPred() != null)
        {
            node.getPred().apply(this);
        }
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getIftrue());
            boolean lett=false;
            if (copy.size()>1) {
                lett=true;
                emit("'(let");
            } else {
                    emitnnl("'(return ");
            }           
            
            for(PExpr e : copy)
            {
                e.apply(this);
            }
            if (lett==true) {
                emitcp("let"); // for let
            } else {
                    emitcp("If-return");
            }            
        }
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getIffalse());
            boolean letf=false;
            if (copy.size()>1) {
                letf=true;
                emit("'(let");
            } else {
                    emitnnl("'(return ");
            }           
            
            for(PExpr e : copy)
            {
                e.apply(this);
            }
            if (letf==true) {
                emitcp("let"); // for let
            } else {
                    emitcp("If-return");
            }            
        }
        outACondPureExpr(node);
    }
    
// ------------------------------------------------------------------------------
/*
 * Function definition:
 * ftype f([argtype argvar]) {body} => 
 * (assign 'f (lambda [quoted argvars] '[body]))
 */
    public void inAFundefBindExpr(AFundefBindExpr node)
    {
        annotate("; Function Definition");
        emit("'(assign '"+node.getFname().toString()+" (lambda ");
        
        for ( PFuncarg arg_type: node.getFargstypes()) {
            AArgtupFuncarg arg = (AArgtupFuncarg)arg_type;
            emitnnl("'"+arg.getArgvar().toString()+" ");    
        }            
        emitnnl("'");        
    }

    public void outAFundefBindExpr(AFundefBindExpr node)
    {
        emitcp("FunDef-lambda");
        emitcp("Fundef-assign");        
    }
    
// ------------------------------------------------

    public void inACmpPureExpr(ACmpPureExpr node)
    {
        annotate("; Operator Call");
        emitnnl("("+node.getOp().toString());
    }

    public void outACmpPureExpr(ACmpPureExpr node)
    {
        emitcp("OpCall");    
    }   
// ------------------------------------------------
    private void annotate(String commentstr) {
        if (verbose) {
        	if (nl==1) {
        		indent();
        	}
            System.out.println(commentstr );
            nl=1;
        }
        
    }
    
    private void emitnnl(String exprstr) {
        ntabs++;
        if (nl==1) {
        	indent();   
        }
        System.out.print(exprstr);    
        nl=0;
    }

    private void emit(String exprstr) {
        ntabs++;
        if (nl==1) {
        indent();   
        }
        System.out.println(exprstr);
        nl=1;
    }    

    private void emitcp() {    
    	if (nl==1) {
    		indent();
    	}
        System.out.println(")");
        ntabs--;nl=1;
    }
    
    private void emitcp(String str) {
    	if (nl==1) {    
    		indent();
    	}
        if (verbose) {
            System.out.println(") ; "+str);
        } else {
            System.out.println(")");            
        }
        ntabs--;nl=1;
    }

    public String toString() {
    	return  new String();
    }

    public String getStr(Node node) {
    	return "";
    }

    private void indent() {
    	for (int i=0;i<ntabs;i++) {
    		System.out.print("    ");
    	}
    }
    
}

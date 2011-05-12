
import GannetC.parser.*;
import GannetC.lexer.*;
import GannetC.node.*;
import GannetC.analysis.*;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Stack;
/**
 * Emit Gannet code
 */

public class Emitter extends DepthFirstAdapter {
    private Map<String, Integer> services = new HashMap<String, Integer> ();
/*
  if parent is in quote map, quote 
 */
    private  Map<Node, Integer> quote = new HashMap<Node, Integer> ();
 // we need a mapping from Gannet-C type info to Gannet type info
    private  Map<String, String> typemapping = new HashMap<String, String> ();
    private int ntabs=0;
    private int label=0;
    private boolean atom=false;
    private boolean verbose=false;
    private boolean withTypeInfo=false;
    
    public Emitter (boolean verbose) {
        // So here we must get all services and aliases from YAML
    	// WV23122009: we now declare the services; the only problem is that now
    	// everything is always an alias. But as this is only used for actual
    	// non-language services, it should be fine.
        this.verbose=verbose;
        services.put("s1", 1);
        services.put("s2", 1);
        services.put("s3", 1);
        services.put("print", 1); // we should use printf but I'm not keen on implementing the patterns.
//        services.put("s1", 1);
        typemapping.put("unsigned int","uint32");
        typemapping.put("unsigned short int","uint16");
        typemapping.put("unsigned long int","uint64");
        typemapping.put("unsigned long long int","uint128");
        typemapping.put("int","int32");
        typemapping.put("short int","int16");
        typemapping.put("long int","int64");
        typemapping.put("long long int","int128");
        typemapping.put("Data","data");
        typemapping.put("Symbol","symbol");
        
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
    
    public void inAOpcallPureExpr(AOpcallPureExpr node)
    {
        annotate("; Operator Call");
        emitq(node,"("+node.getOp().toString());
    }

    public void outAOpcallPureExpr(AOpcallPureExpr node)
    {
        emitcp("OpCall");    
    }    
    
    public void inALtcallPureExpr(ALtcallPureExpr node)
    {
        annotate("; Operator Call");
        emitq(node,"("+node.getOp().toString());
    }

    public void outALtcallPureExpr(ALtcallPureExpr node)
    {
        emitcp("LtCall");    
    }
    
    public void inALGtcallPureExpr(AGtcallPureExpr node)
    {
        annotate("; Operator Call");
        emitq(node,"("+node.getOp().toString());
    }

    public void outAGtcallPureExpr(AGtcallPureExpr node)
    {
        emitcp("GtCall");    
    }
    
    public void inANumberPureExpr(ANumberPureExpr node)
    {        
        annotate("; Number:");
        emit("'"+node.getNumber().toString());
    }

    public void outANumberPureExpr(ANumberPureExpr node)
    {
        atom=true;
        ntabs--;
    }
    
    public void inAStringPureExpr(AStringPureExpr node)
    {        
        annotate("; String:");
        emit(node.getStringLiteral().toString());
    }

    public void outAStringPureExpr(AStringPureExpr node)
    {
        atom=true;
        ntabs--;
    }    
    
    public void inAVarPureExpr(AVarPureExpr node)
    {
        annotate("; Variable:");        
        emit(node.getIdentifier().toString());        
    }

    public void outAVarPureExpr(AVarPureExpr node)
    {
        atom=true;
        ntabs--;
    }
    
//    public void inAProcapplPureExpr(AProcapplPureExpr node)
//    {
//        annotate("; Procedure Call");
//        emitq(node,node.getFname().toString());
//    }
//
//    public void outAProcapplPureExpr(AProcapplPureExpr node)
//    {
//        ntabs--;
//    }

    public void inAFunapplPureExpr(AFunapplPureExpr node)
    {
        if (services.containsKey(node.getFname().toString().trim())) { 
            annotate("; Service Call");
            emitq(node,"("+node.getFname().toString());
        } else {
            annotate("; Function Application");
            emitq(node,"(apply "+node.getFname().toString());
            // apply is complicated: numerical service should not be quoted, others should be quoted.
            // so it is not enough to say "quote all args of apply"
            // we should say, if apply, look at the type of the argument; if it's a number or a symbol, don't quote
            // otherwise quote
        }
    }

    public void outAFunapplPureExpr(AFunapplPureExpr node)
    {
        emitcp("FunAppl");    
    }

    
    
    public void inAParletPureExpr(AParletPureExpr node)
    {
        annotate("; Parallel LET "+node.hashCode());
        emitq(node,"(let ");
    }

    public void outAParletPureExpr(AParletPureExpr node)
    {   
        emitcp("ParLet");    
    }

    public void inASeqletPureExpr(ASeqletPureExpr node)
    {
        annotate("; Sequential LET");
        emitq(node,"(let");         
        quote.put(node, 1);
    }

    public void outASeqletPureExpr(ASeqletPureExpr node)
    {        
        emitcp("SeqLet");    
//        quotes.pop();        
    }
    
    public void inAWhilePureExpr(AWhilePureExpr node)
    {
/*
 we need:
 (label L (if [cond]
     '(return 'L [block])
     '(return '0)
     )
 )
         
*/        
        label++;
        annotate("; While " );
        emitq(node,"(label L"+label+" (if "); // FIXME: need quoting here too                
    }
    
    public void caseAWhilePureExpr(AWhilePureExpr node)
    {
        inAWhilePureExpr(node);
        if(node.getPred() != null)
        {
            node.getPred().apply(this);
        }
        {
            emit("'(return 'L"+label);            
            List<PExpr> copy = new ArrayList<PExpr>(node.getWbody());
            boolean let=false;
            if (copy.size()>1) {
                if(node.getBlocktype() != null) {
                    quote.put(node, 1);
                }                
                let=true;
                emit("(let");
            }
            for(PExpr e : copy)
            {
                e.apply(this);
            }
            if (let==true) {
                emitcp("let"); // for let
            }
            emitcp("return"); // for return
        }
        if(node.getBlocktype() != null)
        {
            node.getBlocktype().apply(this);
        }
        
        outAWhilePureExpr(node);
    }
    

    public void outAWhilePureExpr(AWhilePureExpr node)
    {
        emit("'(return '0)" );        
        emitcp("if"); // paren for the if
        emitcp("While-label");            
    }
    
/*    
for (init, cond, mod) { block }

(let
    [init]
    (label L (if [cond]
        (return 'L (return '[mod] [block]))
        (return '0)
    ))
)

*/
    public void inAForPureExpr(AForPureExpr node)
    {
        label++;
        annotate("; For " );
        emitq(node,"(let");
                        
    }

    public void outAForPureExpr(AForPureExpr node)
    {
        emit("'(return '0)" );        
        emitcp("if"); // paren for the if
        emitcp("For-label"); // paren for label
        emitcp("For"); // paren for let
    }

    @Override
    public void caseAForPureExpr(AForPureExpr node)
    {
        inAForPureExpr(node);
        if(node.getInit() != null)
        {            
            node.getInit().apply(this);
            emitq(node,"(label L"+label+" (if "); // FIXME: need quoting here too
        }
        if(node.getCond() != null)
        {
            node.getCond().apply(this);
        }
        emit("(return 'L (return '");
        if(node.getMod() != null)
        {            
            node.getMod().apply(this);
        }
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getLbody());
            boolean let=false;
            if (copy.size()>1) {
                if(node.getBlocktype() != null) {
                    quote.put(node, 1);
                }
                let=true;
                emit("(let");
            }            
            for(PExpr e : copy)
            {
                e.apply(this);
            }
            if (let==true) {
                emitcp("For-let"); // for let
            }
            emitcp("return"); // for return
            emitcp("return L"); // for return L
        }
        outAForPureExpr(node);
    }

    public void inACondPureExpr(ACondPureExpr node)
    {
        annotate("If-Else");
        emitq(node,"(if"); 
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
                if(node.getBlocktypet() != null) {
                    quote.put(node, 1);
                }
                lett=true;
                emit("'(let");
            } else {
                // If we have atoms we must wrap them in (return )
                if (atom==true) {                        
                    emit("'(return");
                } else {                        
                    // only quote
                    emit("'");
                }                                
            }           
            
            for(PExpr e : copy)
            {
                e.apply(this);
            }
            if (lett==true) {
                emitcp("let"); // for let
            } else {
                if (atom==true) {
                    atom=false;
                    emitcp("If-return");
                } else {                        
                    // nothing                    
                }                                
                
            }
            
        }
        if(node.getBlocktypet() != null)
        {
            node.getBlocktypet().apply(this);
        }
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getIffalse());
            boolean letf=false;
            if (copy.size()>1) {
                if(node.getBlocktypef() != null) {
                    quote.put(node, 1);
                }
                letf=true;
                emit("'(let");
            } else {
                // If we have atoms we must wrap them in (return )
                if (atom==true) {                        
                    emit("'(return");
                } else {                        
                    // only quote
                    emit("'");
                }                                                
            }           
            
            for(PExpr e : copy)
            {
                e.apply(this);
            }
            if (letf==true) {
                emitcp("let"); // for let
            } else {
                if (atom==true) {
                    atom=false;
                    emitcp("If-return");
                } else {                        
                    // nothing                    
                }                                                
            }
            
        }
        if(node.getBlocktypef() != null)
        {
            node.getBlocktypef().apply(this);
        }
        outACondPureExpr(node);
    }
    
// ------------------------------------------------------------------------------
    public void inAAssignBindExpr(AAssignBindExpr node)
    {
        annotate("; Assignment " );
        emitq(node,"(assign"+addTypeInfo(node)+" '"+node.getVname().toString()); // FIXME: need quoting here too        
    }

    public void outAAssignBindExpr(AAssignBindExpr node)
    {
        emitcp("Assign");    
    }
    
/*
    a+=1 => (update 'a (+ a '1))
*/    
    public void inAOpupdateBindExpr(AOpupdateBindExpr node)
    {
        annotate("; Operator Update " );
        emitq(node,"(update '"+node.getVname().toString()); // FIXME: need quoting here too
        String update_operator=node.getOp().toString().trim();
        String operator=update_operator.substring(0, update_operator.length()-1); 
        emit("("+operator+" "+node.getVname().toString());
    }

    public void outAOpupdateBindExpr(AOpupdateBindExpr node)
    {
        emitcp("OpUpdate-opexpr");
        emitcp("OpUpdate");    
    }
   
    public void inAUpdateBindExpr(AUpdateBindExpr node)
    {
        annotate("; Update " );
        emitq(node,"(update '"+node.getVname().toString()); // FIXME: need quoting here too        
    }

    public void outAUpdateBindExpr(AUpdateBindExpr node)
    {
        emitcp("Update");    
    }
    
// ------------------------------------------------
/*
 * Function definition:
 * ftype f([argtype argvar]) {body} => 
 * (assign 'f (lambda [quoted argvars] '[body]))
 */
    public void inAFundefBindExpr(AFundefBindExpr node)
    {
        if (node.getFargstypes().size()==0) {
            // This never happens            
        } else {
            if ( (node.getFargstypes().size()==1) &&
             node.getFargstypes().element().getClass().isInstance(new GannetC.node.AVoidFuncarg()) ) {
                // it's a procdure call, use LABEL
                annotate("; Procedure Definition");
                //label++;
                emitq(node,"(label "+node.getFname().toString());
                if (node.getFbody().size()>1) {
                    emit("(let");
                    if(node.getBlocktype() != null) {
                        quote.put(node, 1);
                    }                
                } else {
                    if (atom==true) {                        
                        emit("(return");
                    } else {                        
                        // it's a call, emit nothing
                    }
                }    
            } else {
                annotate("; Function Definition");
                emitq(node,"'(assign '"+node.getFname().toString()+" (lambda ");
                
                for ( PFuncarg arg_type: node.getFargstypes()) {
                    AArgtupFuncarg arg = (AArgtupFuncarg)arg_type;
                    emit("'"+arg.getArgvar().toString());    
                }
            }
        }
    }

    public void outAFundefBindExpr(AFundefBindExpr node)
    {
        emitcp("FunDef-lambda?");
        if (node.getFargstypes().size()==0) {
            // This never happens            
        } else {
            if ( (node.getFargstypes().size()==1) &&
             node.getFargstypes().element().getClass().isInstance(new GannetC.node.AVoidFuncarg()) ) {
                // it's a procdure call, use LABEL    
                if (node.getFbody().size()>1) {
                    emitcp("Fundef-let");
                } else {
                    if (atom==true) {
                        atom=false;
                        emitcp("Fundef-return");
                    } else {
                        // it's a call, emit nothing
                    }
                }    
                
            } else {
                emitcp("Fundef-assign");
            }
        }
    }
    
// ------------------------------------------------
// ------------------------------------------------
    private void annotate(String commentstr) {
        if (verbose) {
            indent();
            System.out.println(commentstr );            
        }
    }
    
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

    private void emitcp() {
        indent();
        System.out.println(")");
        ntabs--;
    }
    
    private void emitcp(String str) {
        indent();
        if (verbose) {
            System.out.println(") ; "+str);
        } else {
            System.out.println(")");            
        }
        ntabs--;
    }
    
    private String maybeQuote(Node node) {
        if (quote.containsKey(node.parent().parent()) ||
            (node.parent().parent().getClass().isInstance(new APureExpr()) &&
                    quote.containsKey(node.parent().parent().parent())
             )       
        ) {
            return "'";
        } else {            
            return "";//+node.parent().parent().getClass().toString();            
        }
    }

    public String toString() {
	return  new String();
    }

    public String getStr(Node node) {
	return "";
    }

//    private String joinList(LinkedList<Node> list, String join) {
//    	return new String();
//    }
    private void indent() {
    	for (int i=0;i<ntabs;i++) {
    		System.out.print("    ");
    	}
    }
//-----------------------------------------------------------------------------
    
    private String addTypeInfo(Node node) {
        if (withTypeInfo) {
            // If the node is a declaration, get the info from the node? We might as well look it up in the table
/*
            if (context.typeinfo.containsKey(nodeId(node)) {
            String gctype =  context.typeinfo.get(nodeId(node));   
            // If the node is a call, overloaded operator or variable, get info from the typeinfo table
            }
*/            
            return "::";
        } else {
            return "";
        }
    }
    
}

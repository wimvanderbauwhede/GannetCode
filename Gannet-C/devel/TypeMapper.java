
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
 * Extract/Infer type information from AST
 * Apart from the C-like types Gannet-C also has
 * - template object-like types Buf<int>, List<...>
 * - object-like types (Lambda?)
 * - the Data and Symbol types.
 * - elision ... which means any type that is not a Configuration
 * - function types e.g. Matrix<int>(int,Matrix<int>,Data)
 * 
 * What is the type of a lambda definitions?
 * Lambda<Data(Data)> lbd=lambda(...);
 * 
 * We could allow soft typing: support both List<int> l1 and List l2 (which really is List<Symbol>)
 * 
 * There are also the tuples, but that is for later. For now a tuple is an object-like type
 * 
 */

public class TypeMapper extends DepthFirstAdapter {
        
    private Context context;
    boolean verbose;
    private  Map<String, Map<String, Integer> > subtypes = new HashMap<String, Map<String, Integer> > ();
    
    public TypeMapper (Context context, boolean verbose) {
        this.context=context;
        this.verbose=verbose;
      
        subtypes.put("int", new HashMap<String, Integer > ());
        subtypes.get("int").put("uint8", 1);
        subtypes.get("int").put("uint16", 1);
        subtypes.get("int").put("uint32", 1);
        subtypes.get("int").put("uint8", 1);
        subtypes.get("int").put("uint16", 1);
        subtypes.get("int").put("uint32", 1);
        subtypes.get("int").put("unsigned int", 1);
        subtypes.get("int").put("uint", 1);
        
    }
/*
    What are the nodes that require actions for this?
    -for every block we must manage the stack
x            | {fundef} [ftype]:type [fname]:identifier [fargstypes]:funcarg+ [fbody]:expr+ [blocktype]:seq?
x            | {cond} [pred]:pure_expr [iftrue]:expr+ [blocktypet]:seq? [iffalse]:expr+  [blocktypef]:seq?
x            | {while} [pred]:pure_expr [wbody]:expr+ [blocktype]:seq?
x            | {for} [init]:bind_expr [cond]:pure_expr [mod]:bind_expr [lbody]:expr+ [blocktype]:seq?
x            | {seqlet} expr+
x            | {parlet} expr+
x            | {group} pure_expr
x            | {lambdadef} [lambdatypes]:funcarg+ [lambdabody]:expr+
    -for every function/procedure/service/variable declaration we need to capture the type
            | {fundef} [ftype]:type [fname]:identifier [fargstypes]:funcarg+ [fbody]:expr+ [blocktype]:seq?
            | {servicedecl} [stype]:type [sname]:identifier [sargstypes]:type+
              {assign} [avartype]:type [vname]:identifier [rhs]:pure_expr    
     Extract type information from 
     - function definitions: return type, arg types. Works for procedures as well
     - variable assignment: return type. As this can be a function type, works for procedures as well
     - service      
     - service instance declaration:
     - configuration
     - configuration instance declaration
     - typedef: we need a typedef map in the Context, to look upo
     -  
              
    -for every application or variable reference we need to lookup the type
  TODO 25/09/09:
  - type checking of arguments in function/service application
  - lambda type??
  - extracting types from service declarations
*/
    
    public void inAProgram(AProgram node)
    {
    	context.outercount.push(0);
    	context.scope.put(context.blockcount, new ScopeRecord(context.outercount.peek()) );
    }
    public void caseAProgram(AProgram node)
    {
        inAProgram(node);
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getExpr());
            PExpr lastexpr=copy.get(0);
            for(PExpr e : copy)
            {
                e.apply(this);
                lastexpr=e;
            }
            addType(node,lastexpr);            
        }
        outAProgram(node);
    }
        
// ---------------------------------------------------------------
    public void outAPureExpr(APureExpr fndecl) {
        addType(fndecl,fndecl.getPureExpr());                
    }
    
    public void caseAPureExpr(APureExpr node)
    {
        inAPureExpr(node);
        if(node.getPureExpr() != null)
        {
            node.getPureExpr().apply(this);
            System.out.println("APureExpr "+node.hashCode()+": Copying type <"+context.typeinfo.get(node.getPureExpr())+"> from PureExpr "+node.getPureExpr().toString()+" ("+node.getPureExpr().hashCode()+")");
            context.typeinfo.put(node,context.typeinfo.get(node.getPureExpr()));
            
        }
        outAPureExpr(node);
    }
 // ---------------------------------------------------------------
//    Operators can be overloaded. To get the type for the operator means to get the type of all its arguments,
//    so that means deciding on the outA*
        
    /* The arguments of the operator can be
     * - numbers/strings => Type is obvious
     * - function/operator calls (including if, for, while) => look up return type for that node
     * - variables => look up declaration
     * - function parameters => look up from signature
     * - I guess I should even allow passing functions as argument to functions ... => these are Symbols 
     */
    /*
     * Loop over every argument in the operator expression
     * If the arguments are not all of the same type, throw an exception
     * Otherwise that is the type of the node
     */
    
    public void casePPureExpr(Token op, LinkedList<PPureExpr> args, PPureExpr node)
    {
        if(op != null)
        {
        	op.apply(this);
        }
        {
            List<PPureExpr> copy = new ArrayList<PPureExpr>(args);
            AAnyOtherType any = new AAnyOtherType();
            Node optype=(Node)any;
            String optypestr=any.toString().trim();
            for(PPureExpr e : copy)
            {
                e.apply(this);
                // now determine the type
                Node etype = context.typeinfo.get(e);
                String etypestr="any"; //FIXME! How do I get a toString() on a Node?
                System.out.println(node.toString()+"("+node.hashCode()+"):"+optype+"<> "+e.toString()+"("+e.hashCode()+";"+e.getClass().toString()+"):"+etype);
                if (optypestr.equals("any")&&(etype!=null)) {
                    optype=etype;
                } else if ((etype==null) || etypestr.equals("any")) { // FIXME!
                	etype=optype;    
                } else if (!etypestr.equals(optypestr)) { 
                	// if the optype is more general than the etype, it's OK, keep the optype
                	/*
                	 * The problem here is to make the link between e.g. word, unsigned short int and uint16
                	 * */
                	if (subtypes.get(optypestr).containsKey(etypestr)) {
                			// Do nothing
                		System.out.println("Soft typing "+op.toString()+":<"+ etype+">=~<"+optype+">");
                	} else {
                    throw new RuntimeException("Type error call to "+op.toString()+":<"+ etype+">!=<"+optype+">");
                	}
                }
            }
            context.typeinfo.put(node,optype);
            System.out.println("AOpcallPureExpr:"+node.toString()+"("+node.hashCode()+"):"+optype);
        }
    } // casePPureExpr
    
    public void caseAOpcallPureExpr(AOpcallPureExpr node)
    {
        inAOpcallPureExpr(node);
        casePPureExpr(node.getOp(), node.getArgs(), node);
        /*
        if(node.getOp() != null)
        {
            node.getOp().apply(this);
        }
        {
            List<PPureExpr> copy = new ArrayList<PPureExpr>(node.getArgs());
            String optype="Any";
            for(PPureExpr e : copy)
            {
                e.apply(this);
                // now determine the type
                String etype = context.typeinfo.get(e);
                System.out.println(node.toString()+"("+node.hashCode()+"):"+optype+"<> "+e.toString()+"("+e.hashCode()+"):"+etype);
                if (optype.equals("Any")&&(etype!=null)) {
                    optype=etype;
                } else if ((etype==null) || etype.equals("Any")) { // FIXME!
                	etype=optype;    
                } else if (!etype.equals(optype)) { 
                    throw new RuntimeException("Type error call to "+node.getOp().toString()+":<"+ etype+">!=<"+optype+">");
                }
            }
            context.typeinfo.put(node,optype);
            System.out.println("AOpcallPureExpr:"+node.toString()+"("+node.hashCode()+"):"+optype);
        }
        */
        outAOpcallPureExpr(node);
    } // caseAOpcallPureExpr
        
    public void caseALtcallPureExpr(ALtcallPureExpr node)
    {
        inALtcallPureExpr(node);
        casePPureExpr(node.getOp(), node.getArgs(), node);
        /*
        if(node.getOp() != null)
        {
            node.getOp().apply(this);
        }
        {
            List<PPureExpr> copy = new ArrayList<PPureExpr>(node.getArgs());
            String optype="Any";
            for(PPureExpr e : copy)
            {
                e.apply(this);
                // now determine the type
                String etype = context.typeinfo.get(e);
                if (optype.equals("Any")&&(etype!=null)) {
                    optype=etype;
                } else if (etype.equals("Any")) { // FIXME!
                	etype=optype;
                } else if (!etype.equals(optype)) {
                    throw new RuntimeException("Type error call to "+node.getOp().toString()+":"+ etype+"!="+optype);
                }
            }
            context.typeinfo.put(node,optype);
        }
        */
        outALtcallPureExpr(node);
    } // caseALtcallPureExpr

    public void caseAGtcallPureExpr(AGtcallPureExpr node)
    {
        inAGtcallPureExpr(node);
        casePPureExpr(node.getOp(), node.getArgs(), node);
        /*
        if(node.getOp() != null)
        {
            node.getOp().apply(this);
        }
        {
            List<PPureExpr> copy = new ArrayList<PPureExpr>(node.getArgs());
            String optype="Any";
            for(PPureExpr e : copy)
            {
                e.apply(this);
                // now determine the type
                String etype = context.typeinfo.get(e);
                if (optype.equals("Any")&&(etype!=null)) {
                    optype=etype;
                } else if (etype.equals("Any")) { // FIXME!
                	etype=optype;                    
                } else if (!etype.equals(optype)) {
                    throw new RuntimeException("Type error call to "+node.getOp().toString() );
                }
            }
            context.typeinfo.put(node,optype);
        }
        */
        outAGtcallPureExpr(node);
    } // caseALtcallPureExpr    
    
// The type of a Number is actually quite tricky. A dot means it's a float or double, we'll assume float
// a minus sign means signed integer, but actually that might be an operator call
// then we need to determine the size of the integer     
    public void inANumberPureExpr(ANumberPureExpr node)
    {        
        String numstr=node.getNumber().toString().trim();
//        String numtype="Any";
        if (numstr.matches("\\.")) {  
            //TODO: distinguish float & double         	
        	context.typeinfo.put(node,new AFltSimpleNumType());            
        } else {
            //TODO: distinguish int8,int16,int32,int64
            if (numstr.startsWith("-")) {
//                numtype="int32";
                context.typeinfo.put(node,new AIntSimpleNumType(new TInttype("int32")));
            } else {
//                numtype="uint32";
                context.typeinfo.put(node,new AIntSimpleNumType(new TInttype("uint32")));
            }
        }
//        context.typeinfo.put(node,numtype);
    }
    
    public void inAStringPureExpr(AStringPureExpr node)
    {        
        context.typeinfo.put(node,new AStrOtherType());
    }

    public void outAVarPureExpr(AVarPureExpr node)
    {
        String varstr=node.getIdentifier().toString().trim();
        // look up in var table
        Node vartype=context.getTypeOf(varstr,context.blockcount);
        context.typeinfo.put(node,vartype);

    }
    
//    public void outAProcapplPureExpr(AProcapplPureExpr node)
//    {
//        String procnamestr=node.getFname().toString().trim();
//        AAnyOtherType any = new AAnyOtherType();
//        Node proctype=(Node)any;
//      if (context.fnsigtable.containsKey(procnamestr)) {
//          proctype=context.fnsigtable.get(procnamestr).ftype;
//      }
//      context.typeinfo.put(node,proctype);
//    }

    public void outAFunapplPureExpr(AFunapplPureExpr node)
    {
        String fnnamestr=node.getFname().toString().trim();
        AAnyOtherType any = new AAnyOtherType();
        Node fntype=(Node)any;
      if (context.fnsigtable.containsKey(fnnamestr)) {
          fntype=context.fnsigtable.get(fnnamestr).ftype;
      }
      context.typeinfo.put(node,fntype);

      // TODO: check if each of the argument types matches the function signature
    }

    
    public void inAFundefBindExpr(AFundefBindExpr node)
    {
        enterScope();
    }

    public void outAFundefBindExpr(AFundefBindExpr node)
    {
        leaveScope();
    }    
    public void caseAFundefBindExpr(AFundefBindExpr node)
    {
        inAFundefBindExpr(node);
        if(node.getFtype() != null)
        {
            node.getFtype().apply(this);            
        }
        Node ftype=node.getFtype();
        if(node.getFname() != null)
        {
            node.getFname().apply(this);
        }
        String fname=node.getFname().toString().trim();
        {
            List<PFuncarg> copy = new ArrayList<PFuncarg>(node.getFargstypes());
            for(PFuncarg e : copy)
            {
                e.apply(this);
            }
            context.fnsigtable.put(fname, new FnTypeSig(ftype,copy));
        }
        // now add the arguments to the scope as well
        ScopeRecord scoperec=context.scope.get(context.blockcount);
        scoperec.varmap=  new HashMap<String,Node> ( context.fnsigtable.get(fname).argsigtable);
        context.scope.put(context.blockcount,scoperec);
        
        
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getFbody());

            for(PExpr e : copy)
            {
                e.apply(this);
            }

        }
        if(node.getBlocktype() != null)
        {
            node.getBlocktype().apply(this);
        }
        outAFundefBindExpr(node);
    }    
    // ------------------------------------------------
    public void inALambdadefPureExpr(ALambdadefPureExpr node)
    {
        enterScope();
    }

    public void outALambdadefPureExpr(ALambdadefPureExpr node)
    {
        leaveScope();
    }
    
    public void caseALambdadefPureExpr(ALambdadefPureExpr node)
    {
        inALambdadefPureExpr(node);
    	String fname="lambda"+context.blockcount;
    	AObjType lambda= new AObjType(new TObjIdentifier("Lambda"));
    	Node ftype=(Node)lambda; // FIXME!
        {

            List<PFuncarg> copy = new ArrayList<PFuncarg>(node.getLambdatypes());
            for(PFuncarg e : copy)
            {
                e.apply(this);
            }
            context.fnsigtable.put(fname, new FnTypeSig(ftype,copy));
        }
        // now add the arguments to the scope as well
        ScopeRecord scoperec=context.scope.get(context.blockcount);
        scoperec.varmap=  new HashMap<String,Node> ( context.fnsigtable.get(fname).argsigtable);
        context.scope.put(context.blockcount,scoperec);
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getLambdabody());

            for(PExpr e : copy)
            {
                e.apply(this);
            }

        }
        outALambdadefPureExpr(node);
    }
    // ------------------------------------------------    
    public void inAParletPureExpr (AParletPureExpr node) {
    	enterScope();    	
    }
    public void outAParletPureExpr (AParletPureExpr node) {
    	leaveScope();
    }   
    
    public void caseAParletPureExpr(AParletPureExpr node)
    {
        inAParletPureExpr(node);
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getExpr());
            PExpr lastexpr=copy.get(0);
            for(PExpr e : copy)
            {
                e.apply(this);
                lastexpr=e;
            }
            AAnyOtherType any = new AAnyOtherType();
            Node lettype=(Node)any;
            if (context.typeinfo.containsKey(lastexpr)) {
                lettype=context.typeinfo.get(lastexpr);
            }
        }
        outAParletPureExpr(node);
    }    
    
 // ------------------------------------------------
    public void inASeqletPureExpr (ASeqletPureExpr node) {
    	enterScope();    	
    }
    public void outASeqletPureExpr (ASeqletPureExpr node) {
    	leaveScope();
    } 
    
    public void caseASeqletPureExpr(ASeqletPureExpr node)
    {
        inASeqletPureExpr(node);
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getExpr());
            PExpr lastexpr=copy.get(0);;
            for(PExpr e : copy)
            {
                e.apply(this);
                lastexpr=e;
            }
            AAnyOtherType any = new AAnyOtherType();
            Node lettype=(Node)any;
            if (context.typeinfo.containsKey(lastexpr)) {
                lettype=context.typeinfo.get(lastexpr);
            }
            context.typeinfo.put(node,lettype);
        }
        outASeqletPureExpr(node);
    }    
    
 // ------------------------------------------------

    public void inAWhilePureExpr(AWhilePureExpr node)
    {
    	enterScope();
    }

    public void outAWhilePureExpr(AWhilePureExpr node)
    {
    	AObjType symbol= new AObjType(new TObjIdentifier("Symbol"));
    	Node symnode=(Node)symbol; // FIXME!
        context.typeinfo.put(node,symnode);
        leaveScope();
    }
    
 // ------------------------------------------------    
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
    public void inAForPureExpr(AForPureExpr node) {
    	enterScope();
    }

    public void outAForPureExpr(AForPureExpr node)
    {
    	AObjType symbol= new AObjType(new TObjIdentifier("Symbol"));
    	Node symnode=(Node)symbol; // FIXME!
        context.typeinfo.put(node,symnode);
        leaveScope();
    }
    
 // ------------------------------------------------
    /*
       public void inAGroupPureExpr(AGroupPureExpr node)
    {
        enterScope();        
    }

    public void outAGroupPureExpr(AGroupPureExpr node)
    {
        leaveScope();
    }

    public void caseAGroupPureExpr(AGroupPureExpr node)
    {
        inAGroupPureExpr(node);
        if(node.getPureExpr() != null)
        {
            node.getPureExpr().apply(this);
            System.out.println("AGroupPureExpr "+node.hashCode()+": Copying type <"+context.typeinfo.get(node.getPureExpr())+"> from PureExpr "+node.getPureExpr().toString()+" ("+node.getPureExpr().hashCode()+")");
            context.typeinfo.put(node,context.typeinfo.get(node.getPureExpr()));
        }
        outAGroupPureExpr(node);
    }
   */ 
 // ------------------------------------------------
    public void caseACondPureExpr(ACondPureExpr node)
    {
        inACondPureExpr(node);
        if(node.getPred() != null)
        {
            node.getPred().apply(this);
        }
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getIftrue());
//            if (copy.size()>1) {
              enterScope();
//            }
            for(PExpr e : copy)
            {
                e.apply(this);
            }
//            if (copy.size()>1) {
                leaveScope();
//              }            
        }
        if(node.getBlocktypet() != null)
        {
            node.getBlocktypet().apply(this);
        }
        {
            List<PExpr> copy = new ArrayList<PExpr>(node.getIffalse());
//            if (copy.size()>1) {
                enterScope();
//              }            
            for(PExpr e : copy)
            {
                e.apply(this);
            }
//            if (copy.size()>1) {
                leaveScope();
//              }               
        }
        if(node.getBlocktypef() != null)
        {
            node.getBlocktypef().apply(this);
        }
        outACondPureExpr(node);
    }    
// ------------------------------------------------------------------------------

    public void outAAssignBindExpr(AAssignBindExpr node)
    {
        Node vartype=node.getAvartype();
        // so far so good, now we need to store this in the correct scope!
        String varname=node.getVname().toString().trim();
        ScopeRecord scoperec=context.scope.get(context.blockcount);
        scoperec.varmap.put(varname, vartype);
        context.scope.put(context.blockcount,scoperec);
    }
    
 // ------------------------------------------------    
/*
    a+=1 => (update 'a (+ a '1))
*/    
    
    public void inAOpupdateBindExpr(AOpupdateBindExpr node)
    {
    	String update_operator=node.getOp().toString().trim();
    	String operator=update_operator.substring(0, update_operator.length()-1); 
    	node.setOp(new TUpdateOperator(operator));
    }
// All we can do here is typechec if LHS and RHS are of the same type
// Considering we check in out*, we can simply get the type of the nodes    
    public void outAOpupdateBindExpr(AOpupdateBindExpr node)
    {
        Node lhstype=checkType(node.getVname());
        Node rhstype=checkType(node.getRhs());
        if (lhstype.getClass()!=rhstype.getClass()) {
            throw new RuntimeException("Type error update of "+node.getVname().toString());
        }
    }
   
    public void inAUpdateBindExpr(AUpdateBindExpr node)
    {
              
    }

    public void outAUpdateBindExpr(AUpdateBindExpr node)
    {
       
    }
    /*
    public void inAAtomPureExpr(AAtomPureExpr node)
    {
        System.out.println(">>>AAtomPureExpr"+node.toString()+":"+node.hashCode());
    }
    
    public void caseAAtomPureExpr(AAtomPureExpr node)
    {
        inAAtomPureExpr(node);
        if(node.getPureExpr() != null)
        {
            node.getPureExpr().apply(this);
            System.out.println("AAtomPureExpr "+node.hashCode()+": Copying type <"+context.typeinfo.get(node.getPureExpr())+"> from getPureExpr "+node.getPureExpr().toString()+" ("+node.getPureExpr().hashCode()+")");
            context.typeinfo.put(node,context.typeinfo.get(node.getPureExpr()));
            
        }
        outAAtomPureExpr(node);
    }
    
    public void inAOpPureExpr(AOpPureExpr node)
    {
        System.out.println(">>>AOpPureExpr"+node.toString()+":"+node.hashCode());
    }
    
    public void caseAOpPureExpr(AOpPureExpr node)
    {
        inAOpPureExpr(node);
        if(node.getPureExpr() != null)
        {
            node.getPureExpr().apply(this);
            System.out.println("AOpPureExpr "+node.hashCode()+": Copying type <"+context.typeinfo.get(node.getPureExpr())+"> from getPureExpr "+node.getPureExpr().toString()+" ("+node.getPureExpr().hashCode()+")");
            context.typeinfo.put(node,context.typeinfo.get(node.getPureExpr()));
            
        }
        outAOpPureExpr(node);
    }
    */
// ------------------------------------------------
 

    public String toString() {
	return  new String();
    }

    public String getStr(Node node) {
	return "";
    }

    private Node checkType(Node node) {
    	AAnyOtherType any = new AAnyOtherType();
    	Node ntype=(Node)any;
    	if (context.typeinfo.containsKey(node)) {
    	    ntype=context.typeinfo.get(node);
    	}
    	return ntype;
    }
    
    private void addType(Node node, Node tnode) {
    	Node ntype=checkType(tnode);
        context.typeinfo.put(node,ntype);
    }
    public void enterScope () {
    	
    	// on entering a block: increase counter
    	context.blockcount++;
    	System.out.println("Entering scope "+context.blockcount+", outer = "+context.outercount.peek());
    	// create a new scope record
    	context.scope.put(context.blockcount, new ScopeRecord(context.outercount.peek()) );
    	// push the current scope on the stack
    	context.outercount.push(context.blockcount);    	
    }
    public void leaveScope () {
    	context.outercount.pop();
    }
}

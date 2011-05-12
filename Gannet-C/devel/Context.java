import GannetC.node.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import java.util.LinkedList;
import java.util.List;

class Context { 
    public int blockcount;// block subtask count, i.e. the current scope
    public Stack<Integer> outercount;//, -- ^ Stack of enclosing blocks
//    public    String current;// Service name of current block
//    public Stack<String> callerstack;// Caller stack of current block 
    public Map<Integer,ScopeRecord> scope;
    public Map<Node, Node> typeinfo;
    public Map<String, FnTypeSig> fnsigtable; // a complication is that we now support int(int) f as well as int f(int)!
    
    public Context() {
        blockcount=0; // this is the running counter for the scopes
        outercount = new Stack<Integer>();
//        current = "";
//        callerstack = new Stack<String>;
        scope = new HashMap<Integer,ScopeRecord>();
        typeinfo  = new HashMap<Node, Node>(); // is this expressive enough? Storing the types as a string seems rather unwieldy; but what is a better choice?
        fnsigtable = new HashMap<String, FnTypeSig >();
    }
    public Node getTypeOf(String var, Integer currentscope) {
    	System.out.print(var+" ["+currentscope+"] => ");
    	Map<String,Node> varmap=scope.get(currentscope).varmap;
    	if (varmap.containsKey(var)) {
    		System.out.println(varmap.get(var).toString());
    		return varmap.get(var);
    	} else {
    		if (currentscope!=0) {
    			Integer enclosingscope=scope.get(currentscope).enclosing; 
    			System.out.println(getTypeOf( var, enclosingscope));
    			return getTypeOf( var, enclosingscope);
    		} else {
    			throw new RuntimeException("No declaration found for "+var+" in current scope.");
    		}
    	}
    }
};

class ScopeRecord {         
    public int enclosing;
    public Map<String,Node> varmap;// a map from  varname to vartype
    public ScopeRecord(Integer _enc) {
        enclosing=_enc;
        varmap = new HashMap<String,Node>();
    };
                    
};

class FnTypeSig {
	public Map<String,Node> argsigtable;
	public List<Node> argsigs;
	public Node ftype;
	public FnTypeSig(Node ftype, List<PFuncarg> argtups) {
		argsigtable= new HashMap<String,Node>();
		argsigs= new LinkedList<Node>();
		this.ftype=ftype;		
        for(PFuncarg node : argtups)
        {	
        	System.out.println(node.toString());
            if (node.getClass().isInstance(new AArgtupFuncarg())) {
            	AArgtupFuncarg argtup=(AArgtupFuncarg)node;
            	String argname="";
            	Node argtype = new AAnyOtherType();
                if(argtup.getArgvar() != null)
                {
                    argname=argtup.getArgvar().toString().trim();
                }
                if(argtup.getArgtype() != null)
                {
                    argtype = argtup.getArgtype();
                }
                if (argname!="") {
                	System.out.println("Putting tuple ("+argname+","+argtype+") in FnTypeSig");
                	argsigtable.put(argname, argtype);                	
                	argsigs.add(argtype);
                }
            }
        }
	};	
}
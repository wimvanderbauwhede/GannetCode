
import java.util.*;
import expression.analysis.*;
import expression.node.*;

public class Calculate extends DepthFirstAdapter
{
  private Map<Node, Integer> node2int = new HashMap<Node, Integer> ();
  private Map<String, Integer> varmap = new HashMap<String, Integer> ();
  // when we find a function definition, we put the argvars in a list
  // when we find an application we bind the values to the vars
  // then we compute the body. Suppose we find a new call inside the body?
  // then we'd have to store the old vars somewhere, i.e. we need a stack
  // but first do a non-recursive implementation
  private Map funcmap  = new HashMap ();
  private Map funcargs = new HashMap ();
  private Random rnd = new Random ();

  private int getNodeInt (Node node)
  {
//	  if (node2int.containsKey(node)) {
    return ((Integer)node2int.get(node)).intValue();
//	  } else {		  
//		  System.out.println ("TROUBLE: "+node.toString()+";"+node.getClass().toString());
//		  throw new RuntimeException("TROUBLE");
//	  }
  }

  private void setNodeInt (Node node, int val)
  {
    node2int.put (node, new Integer (val));
  }
  
    private void writeVar (String var, int val)
  {
      
      varmap.put (var, new Integer (val));
  }
  
  private void updateVar (String var, int val)
  {      
    //System.out.print("<"+var.toString()+"> => <"+readVar(var)+"> =? <"+val+">\n");
    
    if (varmap.containsKey(var) ) {
        varmap.remove(var);        // get current binding and remove it
        varmap.put (var, new Integer (val));
    } else {
        // raise exception
        System.out.println ("Variable "+var+" is not assigned");
    }
/*    
    // print out the contents of the table
    for (Iterator i = varmap.keySet().iterator(); i.hasNext(); ) {
        Object key = i.next();
        System.out.println("<"+key + "> -> <" + varmap.get(key)+">");
    }    
*/    
  }
  
  private int readVar (String var)
  { 
    return ((Integer)varmap.get(var)).intValue();
  }

  public void outAGrammar (AGrammar node)
  {
    Iterator it = node.getExp().iterator();
    while (it.hasNext())
      {
        PExp exp = (PExp)it.next();
        //System.out.print (exp);
        System.out.print (getNodeInt (exp));
//        if ( it.hasNext() ) System.out.println (";");
        System.out.println ();
      }    
  }
  
  public void outABlockExp(ABlockExp node)
  {
      Iterator it = node.getExps().iterator();
      int retval=0;
      while (it.hasNext())
        {
          PExp exp = (PExp)it.next();
          retval=getNodeInt (exp);
          System.out.print ("B:"+retval);
          System.out.println ();
        }
      setNodeInt (node, retval);
  }
  
  public void outAFunappRhs(AFunappRhs node)
  {
      setNodeInt (node, -1);
  }
  
  public void outAAssignmentExp(AAssignmentExp node)
  {   
      int varval = getNodeInt(node.getRhs());
      TIdentifier var = node.getVar();
      String varname = var.toString();
      writeVar(varname,varval);
      setNodeInt (node, varval);
  }
  
  public void outAUpdateExp(AUpdateExp node)
  {      
      int varval = getNodeInt(node.getRhs());
      TIdentifier var = node.getVar();
      String varname = var.toString();
      updateVar(varname,varval);
      setNodeInt (node, varval);
  }
  public void outAVarRhs(AVarRhs node)
  {      
      TIdentifier var = node.getVar();
      String varname = var.toString();
      int varval = readVar(varname);
      setNodeInt (node, varval);      
  }
  
  
  public void outARhsExp(ARhsExp node)
  {
      setNodeInt (node, getNodeInt(node.getRhs()));
  }
  
  public void outAPostincrRhs(APostincrRhs node)
  {
      setNodeInt (node, getNodeInt(node.getTerm()) + 1);
  }
  
  public void outAPostdecrRhs(APostdecrRhs node)
  {
      setNodeInt (node, getNodeInt(node.getTerm()) - 1);
  }
  public void outAPreincrRhs(APreincrRhs node)
  {
      setNodeInt (node, getNodeInt(node.getTerm()) + 1);
  }
  public void outAPredecrRhs(APredecrRhs node)
  {
      setNodeInt (node, getNodeInt(node.getTerm()) - 1);
  }
  public void outANegateRhs(ANegateRhs node)
  {
	  setNodeInt (node, 0 - getNodeInt(node.getTerm()) );      
  } 
  
  public void outANumberRhs (ANumberRhs node)
  {
    setNodeInt (node, Integer.parseInt (node.getNumber().getText()));
  }

  public void outAPlusRhs (APlusRhs node)
  {
    setNodeInt (node, getNodeInt (node.getL()) + getNodeInt (node.getR()));
  }

  public void outAMinusRhs (AMinusRhs node)
  {
    setNodeInt (node, getNodeInt (node.getL()) - getNodeInt (node.getR()));
  }

  public void outAMultRhs (AMultRhs node)
  {
    setNodeInt (node, getNodeInt (node.getL()) * getNodeInt (node.getR()));
  }

  public void outADivRhs (ADivRhs node)
  {
    // maybe we should check here for division by zero? :)
    setNodeInt (node, getNodeInt (node.getL()) / getNodeInt (node.getR()));
  }

  public void outAT1Textual (AT1Textual node)
  {
    setNodeInt (node, 1);
  }

  public void outAT2Textual (AT2Textual node)
  {
    setNodeInt (node, 2);
  }

  public void outAT3Textual (AT3Textual node)
  {
    setNodeInt (node, 3);
  }

  public void outATextualRhs (ATextualRhs node)
  {
    int res = 0;
    int mul = 1;
    ListIterator it = node.getTextual().listIterator (node.getTextual().size());
    while ( it.hasPrevious () )
      {
        PTextual t = (PTextual)it.previous();
        res += mul * getNodeInt (t);
        mul *= 10;
      }
    setNodeInt (node, res);
  }

  public void outARandomX2Rhs (ARandomX2Rhs node)
  {
    setNodeInt (node, rnd.nextInt(100));
  }

}



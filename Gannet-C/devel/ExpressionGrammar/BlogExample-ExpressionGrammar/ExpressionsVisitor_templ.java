// This class has been generated from ExpressionsVisitor_templ.java
import java.lang.Math;
import java.util.*;
import expression.analysis.*;
import expression.node.*;

public class ExpressionsVisitor extends DepthFirstAdapter
{
  private Map<Node, Integer> node2int = new HashMap<Node, Integer> ();  
  private Map<String, Integer> optable = new HashMap<String, Integer> ();
  
  public ExpressionsVisitor () {
//OP_EXPR_OPTABLE_ENTRIES
  }

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
  

  public void outAGrammar (AGrammar node)
  {
    Iterator it = node.getExpr().iterator();
    while (it.hasNext())
      {
        PExpr exp = (PExpr)it.next();
        //System.out.print (exp);
        System.out.print (getNodeInt (exp));
//        if ( it.hasNext() ) System.out.println (";");
        System.out.println ();
      }    
  }
  
  
  public void outANumberExpr (ANumberExpr node)
  {
    String intstr=  node.getNumber().getText();
    if (intstr.startsWith("+")) {
        intstr=intstr.substring(1);
    }
    setNodeInt (node, Integer.parseInt (intstr));

  }
      
  public void outAPlusExpr (APlusExpr node)
  {
    setNodeInt (node, getNodeInt (node.getL()) + getNodeInt (node.getR()));
  }
  
//OP_EXPR_VISITORS

}



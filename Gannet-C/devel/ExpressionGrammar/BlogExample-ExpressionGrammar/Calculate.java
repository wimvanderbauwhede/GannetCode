import java.lang.Math;
import java.util.*;
import Expression.analysis.*;
import Expression.node.*;

public class Calculate extends DepthFirstAdapter
{
  private Map<Node, Integer> node2int = new HashMap<Node, Integer> ();  
  private Map<String, Integer> optable = new HashMap<String, Integer> ();
  
  public Calculate () {
//OP_EXPR_OPTABLE_ENTRIES
		optable.put("++",10);
		optable.put("--",11);
		optable.put("**",20);
		optable.put("!",30);
		optable.put("~",31);
		optable.put("*",40);
		optable.put("/",41);
		optable.put("%",42);
		optable.put("+",50);
		optable.put("-",51);
		optable.put("<<",60);
		optable.put(">>",61);
		optable.put("<=",70);
		optable.put(">=",71);
		optable.put(">",72);
		optable.put("<",73);
		optable.put("==",80);
		optable.put("!=",81);
		optable.put("&",90);
		optable.put("bitand",91);
		optable.put("|",100);
		optable.put("^",101);
		optable.put("bitor",102);
		optable.put("xor",103);
		optable.put("&&",110);
		optable.put("and",111);
		optable.put("||",120);
		optable.put("or",121);
		optable.put("=",130);
  }

  private int getNodeInt (Node node)
  {
    return ((Integer)node2int.get(node)).intValue();
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
        System.out.print (getNodeInt (exp));
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
  
    public void outASignExpr(ASignExpr node)
    {
    String opstr=node.getOp().toString().trim();
    if (opstr.equals("-")) {
        setNodeInt (node, 0 - getNodeInt (node.getR()));
    }      else {

    setNodeInt (node, getNodeInt (node.getR()));
  }
  }
  
//OP_EXPR_VISITORS

    public void outAL1Expr(AL1Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 10: // '++'
                setNodeInt (node, getNodeInt (node.getL()) + 1 );
                break;

            case 11: // '--'
                setNodeInt (node, getNodeInt (node.getL()) - 1 );
                break;
                            
        }
    }

    public void outAL2Expr(AL2Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 20: // '**'
                setNodeInt (node, (int)Math.pow((double)getNodeInt (node.getL()) , (double)getNodeInt (node.getR())));	
                break;
                            
        }
    }

    public void outAL3Expr(AL3Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 30: // '!'
                setNodeInt (node, 1 - getNodeInt (node.getL()));
                break;

            case 31: // '~'
                setNodeInt (node, ~ getNodeInt (node.getL()));
                break;
                            
        }
    }

    public void outAL4Expr(AL4Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 40: // '*'
                setNodeInt (node, getNodeInt (node.getL()) * getNodeInt (node.getR()));
                break;

            case 41: // '/'
                setNodeInt (node, getNodeInt (node.getL()) / getNodeInt (node.getR()));
                break;

            case 42: // '%'
                setNodeInt (node, getNodeInt (node.getL()) % getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL5Expr(AL5Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 50: // '+'
                setNodeInt (node, getNodeInt (node.getL()) + getNodeInt (node.getR()));
                break;

            case 51: // '-'
                setNodeInt (node, getNodeInt (node.getL()) - getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL6Expr(AL6Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 60: // '<<'
                setNodeInt (node, getNodeInt (node.getL()) << getNodeInt (node.getR()));
                break;

            case 61: // '>>'
                setNodeInt (node, getNodeInt (node.getL()) >> getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL7Expr(AL7Expr node)
    {
        String opstr=node.getOp().toString().trim();
        int val=0;
        switch (optable.get(opstr)) {
        
                case 70: // '<='                
                if (getNodeInt (node.getL()) <= getNodeInt (node.getR())) {val=1;}
                break;
                
                case 71: // '>='
                if ( getNodeInt (node.getL()) >= getNodeInt (node.getR())) {val=1;}
                break;
                
                case 72: // '>'
                if ( getNodeInt (node.getL()) > getNodeInt (node.getR())) {val=1;}
                break;
                
                case 73: // '<'
                if ( getNodeInt (node.getL()) < getNodeInt (node.getR())) {val=1;}
                break;
        }
        setNodeInt (node, val);
    }

    public void outAL8Expr(AL8Expr node)
    {
        String opstr=node.getOp().toString().trim();
        int val=0;        
        switch (optable.get(opstr)) {

            case 80: // '=='
                if ( getNodeInt (node.getL()) == getNodeInt (node.getR())) {val=1;}
                break;

            case 81: // '!='
                if ( getNodeInt (node.getL()) != getNodeInt (node.getR())) {val=1;}
                break;
                            
        }
        setNodeInt (node, val);
    }

    public void outAL9Expr(AL9Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 90: // '&'
                setNodeInt (node, getNodeInt (node.getL()) & getNodeInt (node.getR()));
                break;

            case 91: // 'bitand'
                setNodeInt (node, getNodeInt (node.getL()) & getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL10Expr(AL10Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 100: // '|'
                setNodeInt (node, getNodeInt (node.getL()) | getNodeInt (node.getR()));
                break;

            case 101: // '^'
                setNodeInt (node, getNodeInt (node.getL()) ^ getNodeInt (node.getR()));
                break;

            case 102: // 'bitor'
                setNodeInt (node, getNodeInt (node.getL()) | getNodeInt (node.getR()));
                break;

            case 103: // 'xor'
                setNodeInt (node, getNodeInt (node.getL()) ^ getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL11Expr(AL11Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 110: // '&&'
                setNodeInt (node, getNodeInt (node.getL()) & getNodeInt (node.getR()));
                break;

            case 111: // 'and'
                setNodeInt (node, getNodeInt (node.getL()) & getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL12Expr(AL12Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 120: // '||'
                setNodeInt (node, getNodeInt (node.getL()) | getNodeInt (node.getR()));
                break;

            case 121: // 'or'
                setNodeInt (node, getNodeInt (node.getL()) | getNodeInt (node.getR()));
                break;
                            
        }
    }

    public void outAL13Expr(AL13Expr node)
    {
        String opstr=node.getOp().toString().trim();
        switch (optable.get(opstr)) {

            case 130: // '='
                setNodeInt (node, getNodeInt (node.getR()));
                break;
                            
        }
    }


}



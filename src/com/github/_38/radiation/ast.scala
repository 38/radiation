import org.mozilla.javascript.{ast => RhinoAST, Parser}
import com.github._38.radiation.CodeMaker.{Conversions, CodeInfo, CodeGeneratePattern}
import scala.math.max
import scala.language.postfixOps
import scala.language.implicitConversions

package com.github._38.radiation.ast {
    import Conversions._
    /** Describe a Location in a source code */
    case class SourceLocation(val line:Int, val column:Int);
    /** The base class for all AST Nodes */
    abstract class Node(location:SourceLocation) {
        val pattern:CodeGeneratePattern;
        lazy val length:Int = pattern length; 
        def targetCode:String = pattern render;
        def targetCodeInfo:List[CodeInfo] = pattern info
        def sourceLocation:SourceLocation = location
    }
    object Node {
        def main(s:Array[String]) {
            val l = SourceLocation(0,0)
            System.out.println(Lst(List(Num("1", l), Num("2", l)), l).targetCodeInfo)
        }
    }
    
    trait Statement extends Node;
    trait Expression extends Statement;
    trait Scope extends Node;
    trait ControlFlow extends Statement;
   
    case class Block(statements:List[Statement], location:SourceLocation) extends Node(location) with Statement {
        val pattern = "{" -- mkList(statements) -- "}"
    }
    case class Break(location:SourceLocation) extends Node(location) with ControlFlow {
        val pattern = "break;":CodeGeneratePattern
    }
    case class Catch(cond:Node, body:Block, location:SourceLocation) extends Node(location) {
        val pattern = if(cond != null) "catch(" -- cond -- ")"  -- body else "finally" -- body
    }
    case class ?:(cond:Expression,  trueExpr:Expression, falseExpr:Expression, location:SourceLocation) extends Node(location) with Expression {
        val pattern = cond -- "?" -- trueExpr -- ":" -- falseExpr
    }
    case class Lst(values:List[Node], location:SourceLocation) extends Node(location) with Expression {
        val pattern = "[" -- mkList(values, ",") -- "]"
    }
    case class Num(value:String, location:SourceLocation) extends Node(location) with Expression {
        val pattern = value:CodeGeneratePattern;
    }
    case class Try(val tryBlock:Block, val catchBlocks:List[Catch], location:SourceLocation) extends Node(location) with Statement {
        val pattern = "try" -- tryBlock -- mkList(catchBlocks)
    }
   
}


import scala.language.postfixOps
import scala.language.implicitConversions

import org.mozilla.javascript.{ast => RhinoAST, Parser, Node => RhinoNode}
import com.github._38.radiation.CodeMaker.{Conversions, CodeInfo, CodeGeneratePattern, Empty}
import scala.math.max
import scala.collection.JavaConverters._

package com.github._38.radiation.ast {
    import Conversions._
    /** Describe a Location in a source code */
    case class SourceLocation(val line:Int, val column:Int);
    /** The base class for all AST Nodes */
    abstract class Node {
        val pattern:CodeGeneratePattern;
        var location:Option[SourceLocation] = None;
        lazy val length:Int = pattern length; 
        def targetCode:String = pattern render;
        def targetCodeInfo:List[CodeInfo] = pattern info
        def apply(loc:SourceLocation) {
            location = Some(loc)
        }
    }
    object Node {
        def main(s:Array[String]) {
            System.out.println(Lst(List(Num("1"), Num("2"))).targetCodeInfo)
        }
        implicit def javaIterableToScalaList[T](from:java.lang.Iterable[T]):List[T] = 
            if(from == null) List() else from.asScala.toList 
        implicit def toRhinoNode[A <: RhinoNode, B <: Node](from:RhinoNode):B = rhinoAstConverter[B](from.asInstanceOf[A])
        implicit def toRhinoNodeList[A <: RhinoNode, B <: Node](from:A):List[B] = (from.asInstanceOf[List[RhinoNode]]) map (toRhinoNode[RhinoNode, B](_))
        implicit def listRhinoNode[A <: RhinoNode, B <: Node](from:java.util.List[A]):List[B] = 
            javaIterableToScalaList[A](from) map ((x:A) => rhinoAstConverter[B](x))
        implicit def rhinoAstConverter[T <: Node](rhino_node:RhinoAST.AstNode):T = (rhino_node match {
            case n:RhinoAST.Block                 => Block(n)
            case _:RhinoAST.BreakStatement        => Break
            case n:RhinoAST.CatchClause           => Catch(n getVarName, n getCatchCondition, n getBody)
            case n:RhinoAST.ContinueStatement     => Continue
            case n:RhinoAST.DoLoop                => DoWhile(n.getBody, n.getCondition)
            case n:RhinoAST.ElementGet            => Index(n.getTarget, n.getElement)
            case n:RhinoAST.ArrayLiteral          => Lst(n.getElements)
            case n:RhinoAST.TryStatement          => Try(n.getTryBlock, n.getCatchClauses, n.getFinallyBlock)
            case n:RhinoAST.ConditionalExpression => ?:(n.getTestExpression, n.getTrueExpression, n.getFalseExpression)
            case n:RhinoAST.Name                  => $(n.getString)
            case n:RhinoAST.EmptyExpression       => EmptyExpr
            case n:RhinoAST.EmptyStatement        => EmptyStat
            case n:RhinoAST.ForInLoop             => {
                (n isForEach) match {
                   case (false) => ForIn  (n getIterator, n getIteratedObject, n getBody) 
                   case (true) => ForEach(n getIterator, n getIteratedObject, n getBody)
                }
            }
        }).asInstanceOf[T]
    }
    
    trait Statement extends Node;
    trait Expression extends Statement;
    trait Scope extends Node;
    trait ControlFlow extends Statement;
    trait Loop extends ControlFlow;
    trait ForLoop extends Loop
   
    case class Block(statements:List[Statement]) extends Statement {
        val pattern = "{" -- mkList(statements) -- "}"
    }
    case object Break extends ControlFlow {
        val pattern = "break;":CodeGeneratePattern
    }
    case class Catch(variable:$, cond:Expression, body:Block) extends Node {
        val pattern = "try(" -- variable -- (if(cond != null) "if" -- cond  else Empty()) -- ")" -- body;
    }
    case object Continue extends ControlFlow {
        val pattern = "continue;":CodeGeneratePattern;
    }
    case class DoWhile(body:Statement, cond:Expression) extends Loop {
        val pattern = "do " -- body -- "while(" -- cond -- ");"
    }
    case class Index(target:Expression, key:Expression) extends Expression {
        val pattern = target -- "[" -- key -- "]"
    }
    case class Lst(values:List[Expression]) extends Expression {
        val pattern = "[" -- mkList(values, ",") -- "]"
    }
    case class Num(value:String) extends Expression {
        val pattern = value:CodeGeneratePattern;
    }
    case class Try(tryBlock:Block, catchBlocks:List[Catch], finallyBlock:Block) extends Statement {
        val pattern = "try" -- tryBlock -- mkList(catchBlocks) -- (if(finallyBlock != null) "finally " -- finallyBlock else Empty())
    }
    case class $(val text:String) extends Expression {
        val pattern = text:CodeGeneratePattern
    }
    case object EmptyExpr extends Expression {
        val pattern = Empty()
    }
    case object EmptyStat extends Expression {
        val pattern = ";":CodeGeneratePattern
    }
    case class ForIn(iterator:Node, iterationObject:Expression, body:Statement) extends ForLoop {
        val pattern = "for(" -- iterator -- " in " -- iterationObject -- ")" -- body
    }
    case class ForEach(iterator:Node, iterationObject:Expression, body:Statement) extends ForLoop {
        val pattern = "for each(" -- iterator -- " in " -- iterationObject -- ")" -- body
    }
    case class ?:(cond:Expression,  trueExpr:Expression, falseExpr:Expression) extends Expression {
        val pattern = cond -- "?" -- trueExpr -- ":" -- falseExpr
    }
   
}


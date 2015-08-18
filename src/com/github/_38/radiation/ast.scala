import scala.language.postfixOps
import scala.language.implicitConversions

import org.mozilla.javascript.{ast => RhinoAST, Parser, Node => RhinoNode, Token => RhinoToken}
import com.github._38.radiation.CodeMaker.{Conversions, CodeInfo, CodeGeneratePattern, Empty, Compound}
import scala.math.max

package com.github._38.radiation.ast {
    import Conversions._
    /** Describe a Location in a source code */
    case class SourceLocation(val line:Int, val column:Int);
    /** The base class for all AST Nodes */
    abstract class Node {
        val pattern:CodeGeneratePattern
        var location:Option[SourceLocation] = None
        lazy val length:Int = pattern length 
        def targetCode:String = pattern render
        def targetCodeInfo:List[CodeInfo] = pattern info
        def apply(loc:SourceLocation) {
            location = Some(loc)
        }
    }
    object Node {
        def main(s:Array[String]) {
            //System.out.println(Lst(List(Num("1"), Num("2"))).targetCodeInfo)
            val parser = new Parser;
            //val ast = parser.parse("[1,2,3,4];", null, 0);
            //System.out.println((ast:Node).targetCodeInfo)
        }
        def _mkScalaList[T](from:java.lang.Iterable[T]):List[T] = {
            import scala.collection.JavaConverters._
            if(from == null) List() else from.asScala.toList 
        }
        def tryConvert(node:RhinoNode):Option[RhinoAST.AstNode] = 
            if(node.isInstanceOf[RhinoAST.AstNode]) Some(node.asInstanceOf[RhinoAST.AstNode]) else None
        def _nodeConversionImp(xs:List[RhinoNode]):List[RhinoAST.AstNode] = xs match {
            case x :: xs => tryConvert(x) match {
                case Some(n) => n :: _nodeConversionImp(xs)
                case None => _nodeConversionImp(xs)
            }
            case List() => List()
        }

        //implicit def asListNode[T <: Node](from:java.lang.Iterable[_ <: RhinoAST.AstNode]):List[T] = _mkScalaList(from) map (rhinoAstConverter[T](_))
        
        implicit def required[T <: Node](opt:Option[T]): T = opt match {
            case Some(what) => what
            case None       => throw new Exception()
        }
        implicit def tryConvert[T <: Node](from: Node): Option[T] = 
            if(from.isInstanceOf[T]) Some(from.asInstanceOf[T]) else None
        /*implicit def rhinoAstConverter[A <: Node](rhino_node:RhinoAST.AstNode):Option[A] = rhino_node match {
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
            case n:RhinoAST.ExpressionStatement   => ExpressionStat(n getExpression)
            case n:RhinoAST.ForInLoop             => {
                (n isForEach) match {
                   case (false)    => ForIn  (n getIterator, n getIteratedObject, n getBody) 
                   case (true)     => ForEach(n getIterator, n getIteratedObject, n getBody)
                }
            }
            case n:RhinoAST.FunctionCall          => Call(n getTarget, n getArguments)
            case n:RhinoAST.InfixExpression       => BinOp(RhinoAST.AstNode operatorToString n.getType, n getLeft, n getRight)
            case n:RhinoAST.KeywordLiteral        => {
                (n getType) match {
                    case RhinoToken.THIS  => This
                    case RhinoToken.NULL  => Null
                    case RhinoToken.TRUE  => True
                    case RhinoToken.FALSE => False
                }
            }
            case n:RhinoAST.LabeledStatement      => {
                val statement = rhinoAstConverter[Statement](n getStatement)
                statement.labels = _mkScalaList(n.getLabels) map ((_:RhinoAST.Label) getName)
                statement
            }
            case n:RhinoAST.AstRoot               => Program(n) 
        }*/
    }
    
    trait Statement extends Node {
        var labels:List[String] = List();
        def labeledPattern = if(labels.length > 0) ((new Compound(List())) /: labels)((p,s) => p -- s -- ":") -- pattern else pattern
        override lazy val length:Int = labeledPattern length 
        override def targetCode:String = labeledPattern render
        override def targetCodeInfo:List[CodeInfo] = labeledPattern info
    }
    trait LHS extends Node;   /* means the lefthand side of assignment, var a = 3 or a = 3 or a["x"] = 3 */
    trait Expression extends Node;
    trait Scope extends Node;
    trait ControlFlow extends Statement;
    trait Loop extends ControlFlow;
    trait ForLoop extends Loop;
    trait Function extends Scope;
    trait Constant extends Expression; /* For some literal constant */
    
    case class Block(statements:List[Statement]) extends Statement {
        val pattern = "{" -- mkList(statements) -- "}"
    }
    case object Break extends ControlFlow {
        val pattern = "break;":CodeGeneratePattern
    }
    case class Catch(variable:$, cond:Option[Expression], body:Block) extends Node {
        val pattern = "try(" -- variable -- (cond match {
            case Some(cond) => " if " -- cond
            case None => Empty()
        }) -- ")" -- body
    }
    case object Continue extends ControlFlow {
        val pattern = "continue;":CodeGeneratePattern;
    }
    case class DoWhile(body:Statement, cond:Expression) extends Loop {
        val pattern = "do " -- body -- "while(" -- cond -- ");"
    }
    case class Index(target:Expression, key:Expression) extends Expression  with LHS{
        val pattern = target -- "[" -- key -- "]"
    }
    case class Lst(values:List[Expression]) extends Expression {
        val pattern = "[" -- mkList(values, ",") -- "]"
    }
    case class Num(value:String) extends Constant {
        val pattern = value:CodeGeneratePattern;
    }
    case class Try(tryBlock:Block, catchBlocks:List[Catch], finallyBlock:Option[Block]) extends Statement {
        val pattern = "try" -- tryBlock -- mkList(catchBlocks) -- (finallyBlock match {
            case Some(finallyBlock) => "finally " -- finallyBlock
            case None               => Empty()
          })
    }
    case class $(text:String) extends Expression with LHS {
        val pattern = text:CodeGeneratePattern
    }
    case object EmptyExpr extends Expression {
        val pattern = Empty()
    }
    case object EmptyStat extends Statement {
        val pattern = ";":CodeGeneratePattern
    }
    case class ExpressionStat(expression:Expression) extends Statement {
        val pattern = expression -- ";"
    }
    case class ForIn(iterator:LHS, iterationObject:Expression, body:Statement) extends ForLoop {
        val pattern = "for(" -- iterator -- " in " -- iterationObject -- ")" -- body
    }
    case class ForEach(iterator:LHS, iterationObject:Expression, body:Statement) extends ForLoop {
        val pattern = "for each(" -- iterator -- " in " -- iterationObject -- ")" -- body
    }
    case class For(initial:LHS, cond:Expression, inc:Expression, body:Statement) extends ForLoop {
        val pattern = "for(" -- initial -- ";" -- cond -- ";" -- inc -- ")" -- body
    }
    case class Call(target:Expression, args:List[Expression]) extends Expression {
        val pattern = target -- "(" -- mkList(args, ",") -- ")"
    }
    case class FuncDef(name:$, args:List[$], body:Block) extends Statement with Function {
        val pattern = "function " -- name -- mkList(args, ",") -- body
    }
    case class FuncExp(name:$, args:List[$], body:Block) extends Expression with Function {
        val pattern = "function " -- name -- mkList(args, ",") -- body
    }
    case class If(cond:Expression, thenClause:Statement, elseCluase:Option[Statement]) extends ControlFlow {
        val pattern = "if(" -- cond -- ")" -- (elseCluase match {
            case None => Empty() 
            case Some(elseCluase) => "else" -- elseCluase
        })
    }
    case class BinOp(op:String, left:Expression, right:Expression) extends Expression {
        val pattern = left -- op -- right;
    }
    case object True extends Constant {
        val pattern = "true":CodeGeneratePattern
    }
    case object False extends Constant {
        val pattern = "false":CodeGeneratePattern
    }
    case object Null extends Constant {
        val pattern = "null":CodeGeneratePattern
    }
    case object This extends Constant {
        val pattern = "this":CodeGeneratePattern
    }
    case object Debugger extends Statement {
        val pattern = "debugger;":CodeGeneratePattern
    }
    case class Program(parts:List[Statement]) extends Node {
        val pattern = mkList(parts, "")
    }
    case class ?:(cond:Expression, trueExpr:Expression, falseExpr:Expression) extends Expression with Function{
        val pattern = cond -- "?" -- trueExpr -- ":" -- falseExpr
    }
   
}


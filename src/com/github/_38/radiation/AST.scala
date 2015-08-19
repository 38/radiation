import scala.language.postfixOps
import scala.language.implicitConversions

import org.mozilla.javascript.{ast => RhinoAST, Parser, Node => RhinoNode, Token => RhinoToken}
import com.github._38.radiation.CodeMaker.{Conversions, CodeInfo, CodeGeneratePattern, Empty, Compound}
import scala.math.max
import scala.reflect.{ClassTag, classTag}

/* note: Let expression is not implemented 
         Arrow Function is not implemented
         for of loop is not implemented
         Block is not Scope 
         Getter and Setter are not implemented 
         Generator is not implemented
         XML
*/
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
        def as[T <: Node: ClassTag]:Option[T] = this match {
            case what if classTag[T].runtimeClass.isInstance(this) => Some(this.asInstanceOf[T])
            case _ => None
        }
    }
    object ASTParser {
        class RhinoASTConversion(from:RhinoAST.AstNode) {
            def AST = Node.rhinoAstToInternal(from)
        }
        implicit def toConversionObject(from: RhinoAST.AstNode) = new RhinoASTConversion(from)
        def fromString(program:String):Node = 
            (new Parser).parse(program, null, 0).AST
        /*def main(arg:Array[String]) {
            System.out.println(fromString("function a(x,y,z) { return x + y + z;}").targetCode)
            System.out.println(fromString("function a(x,y,z) { return x + y + z;}").targetCodeInfo)
        }*/
    }
    object Node {
        import scala.collection.JavaConverters._
        class SyntaxError(message:String) extends Exception {
            override def toString = "Syntax Error : " + message;
        }
        class RhinoASTHelper(from:RhinoAST.AstNode){
            def optional[T <: Node : ClassTag]:Option[T] = if(from != null) rhinoAstToInternal(from).as[T] else None
            def required[T <: Node : ClassTag]:T         = if(from != null) {
                rhinoAstToInternal(from).as[T] match {
                    case Some(value) => value
                    case None        => throw new SyntaxError("Type " + classTag[T] + " is expected, but we got a " + rhinoAstToInternal(from));
                }
            } else throw new SyntaxError("Required field not found")

            def list[T <: Node : ClassTag]:List[T] = {
                if(from == null) List[T]()
                else _listImpl[T](from.asScala.toList)
            }
            def _listImpl[T <: Node : ClassTag](xs:List[RhinoNode]):List[T] = xs match {
                case x :: xs => x match {
                    case x:RhinoAST.AstNode => rhinoAstToInternal(x).as[T] match {
                        case Some(n)        => n :: _listImpl(xs)
                        case None           => {System.out.println(classTag[T].toString + " drop problematic node " + x.toSource); throw new Exception; _listImpl(xs)}
                    }
                    case _                  => _listImpl(xs)
                }
                case List() => List()
            }
        }
        class JavaListHelper(from:java.util.List[_ <: RhinoAST.AstNode]) {
            def list[T <: Node: ClassTag]: List[T] = {
                import scala.collection.JavaConverters._
                if(from == null) List[T]()
                else _listImpl[T](from.asScala.toList)
            }
            def _listImpl[T <: Node: ClassTag](xs: List[RhinoAST.AstNode]):List[T] = xs match {
                case x :: xs => rhinoAstToInternal(x).as[T] match {
                    case Some(n)  => n :: _listImpl(xs)
                    case None     => _listImpl(xs)
                }
                case List() => List()   
            }
        }
        def getLocals(scope: RhinoAST.Scope):Set[String] = 
            if(scope.getSymbolTable != null) scope.getSymbolTable.keySet.asScala.toSet else Set()
        def operatorString(opcode:Int) = 
           (RhinoAST.AstNode operatorToString opcode) + 
           (if(opcode == RhinoToken.TYPEOF || opcode == RhinoToken.DELPROP || opcode == RhinoToken.VOID) " " else "")  
          
        implicit def toHelper(from:RhinoAST.AstNode):RhinoASTHelper = new RhinoASTHelper(from)
        implicit def toHelper(from:java.util.List[_ <: RhinoAST.AstNode]):JavaListHelper = new JavaListHelper(from)

        def rhinoAstToInternal(rhino_node:RhinoAST.AstNode): Node = rhino_node match {
            case n:RhinoAST.ObjectProperty        => :::(n.getLeft.required[Expression], n.getRight.required[Expression])
            case n:RhinoAST.NewExpression         => New(n.getTarget.required[Expression], n.getArguments.list[Expression])
            case n:RhinoAST.PropertyGet           => ->(n.getLeft.required[Expression], n.getRight.required[$])
            
            case n:RhinoAST.ArrayLiteral          => Lst(n.getElements.list[Expression])
            case n:RhinoAST.AstRoot               => Program(n.list[Statement]) 
            case n:RhinoAST.Block                 => Block(n.list[Statement])
            case _:RhinoAST.BreakStatement        => Break
            case n:RhinoAST.CatchClause           => Catch(n.getVarName.required[$], n.getCatchCondition.optional[Expression], n.getBody.required[Block])
            case n:RhinoAST.ConditionalExpression => ?:(n.getTestExpression.required[Expression], 
                                                        n.getTrueExpression.required[Expression], 
                                                        n.getFalseExpression.required[Expression])
            case n:RhinoAST.ContinueStatement     => Continue
            case n:RhinoAST.DoLoop                => DoWhile(n.getBody.required[Statement], n.getCondition.required[Expression])
            case n:RhinoAST.ElementGet            => Index(n.getTarget.required[Expression], n.getElement.required[Expression])
            case n:RhinoAST.EmptyExpression       => EmptyExpr
            case n:RhinoAST.EmptyStatement        => EmptyStat
            case n:RhinoAST.ExpressionStatement   => ExpressionStat(n.getExpression.required[Expression])
            case n:RhinoAST.ForLoop               => For(n.getInitializer.required[ForLoopInitializer], 
                                                         n.getCondition.required[Expression], 
                                                         n.getIncrement.required[Expression],
                                                         n.getBody.required[Statement])
            case n:RhinoAST.ForInLoop             => {
                (n isForEach) match {
                   case (false)    => ForIn  (n.getIterator.required[ForLoopInitializer], n.getIteratedObject.required[Expression], n.getBody.required[Statement]) 
                   case (true)     => ForEach(n.getIterator.required[ForLoopInitializer], n.getIteratedObject.required[Expression], n.getBody.required[Statement])
                }
            }
            case n:RhinoAST.FunctionCall          => Call(n.getTarget.required[Expression], n.getArguments.list[Expression])
            case n:RhinoAST.FunctionNode          => {
                (n getFunctionType) match {
                    case RhinoAST.FunctionNode.FUNCTION_STATEMENT => FuncDef(n.getFunctionName.optional[$], 
                                                                             n.getParams.list[$], 
                                                                             n.getBody.required[Block],
                                                                             getLocals(n))
                    case _                                        => FuncExp(n.getFunctionName.optional[$], 
                                                                             n.getParams.list[$], 
                                                                             n.getBody.required[Block],
                                                                             getLocals(n))
                }
            }
            case n:RhinoAST.InfixExpression       => BinOp(operatorString(n.getType), 
                                                           n.getLeft.required[Expression], 
                                                           n.getRight.required[Expression])
            case n:RhinoAST.IfStatement           => If(n.getCondition.required[Expression], n.getThenPart.required[Statement], n.getElsePart.optional[Statement])
            case n:RhinoAST.KeywordLiteral        => {
                (n getType) match {
                    case RhinoToken.THIS  => This
                    case RhinoToken.NULL  => Null
                    case RhinoToken.TRUE  => True
                    case RhinoToken.FALSE => False
                }
            }
            case n:RhinoAST.LabeledStatement      => {
                import scala.collection.JavaConverters._
                val statement = n.getStatement.required[Statement]
                statement.labels = n.getLabels.asScala.toList map ((_:RhinoAST.Label) getName)
                statement
            }
            case n:RhinoAST.Name                  => $(n.getString)
            case n:RhinoAST.NumberLiteral         => Num(n getValue)
            case n:RhinoAST.ObjectLiteral         => Dict(n.getElements.list[Property])
            case n:RhinoAST.ParenthesizedExpression => PE(n.getExpression.required[Expression])
            case n:RhinoAST.RegExpLiteral         => Reg(n.getValue, (if(n.getFlags == null) None else Some(n.getFlags)))
            case n:RhinoAST.ReturnStatement       => Return(n.getReturnValue.optional[Expression])
            case n:RhinoAST.StringLiteral         => Str(n.getValue)
            case n:RhinoAST.SwitchCase            => Case(n.getExpression.optional[Expression], n.getStatements.list[Statement])
            case n:RhinoAST.SwitchStatement       => Switch(n.getExpression.required[Expression], n.getCases.list[Case])
            /* As Meta-Data Node, Symbol Node is ignored */
            case n:RhinoAST.ThrowStatement        => Throw(n.getExpression.required[Expression])
            case n:RhinoAST.TryStatement          => Try(n.getTryBlock.required[Block], n.getCatchClauses.list[Catch], n.getFinallyBlock.optional[Block])
            case n:RhinoAST.UnaryExpression       => 
                if(n.isPostfix) _$(operatorString(n.getType), n.getOperand.required[Expression])
                else            $_(operatorString(n.getType), n.getOperand.required[Expression])
            case n:RhinoAST.VariableInitializer   => VarDecl(n.getTarget.required[$], n.getInitializer.optional[Expression])
            case n:RhinoAST.VariableDeclaration   => { 
                val how = (n isVar, n isLet, n isConst) match {
                    case (true, false, false) => "var"
                    case (false, true, false) => "let"
                    case (false, false, true) => "const"
                    case _                    => "ooops"
                }
                if(n isStatement) DefineStmt(how, n.getVariables.list[VarDecl]) else DefineInit(how, n.getVariables.list[VarDecl])
            }
            case n:RhinoAST.Scope                 => Block(n.list[Statement]) /* ECMAScript5 do not have block scopes, only scope is function scope */
        }
    }
    
    trait Statement extends Node {
        var labels:List[String] = List();
        def labeledPattern = if(labels.length > 0) ((new Compound(List())) /: labels)((p,s) => p -- s -- ":") -- pattern else pattern
        override lazy val length:Int = labeledPattern length 
        override def targetCode:String = labeledPattern render
        override def targetCodeInfo:List[CodeInfo] = labeledPattern info
    }

    /* Define Node Properties */
    trait ForLoopInitializer extends Node;
    trait LHS extends Node;   /* means the lefthand side of assignment, var a = 3 or a = 3 or a["x"] = 3 */
    trait Expression extends Node with ForLoopInitializer;
    trait Scope extends Node {
        val localSymbols:Set[String];
    }
    trait ControlFlow extends Statement;
    trait Loop extends ControlFlow;
    trait ForLoop extends Loop;
    trait Constant extends Expression; /* For some literal constant */
    abstract class Function(name:Option[$], args:List[$], body:Block, locals:Set[String]) extends Scope {
        val localSymbols = locals
        val shared = " " -- (name match {
            case Some(name) => name
            case None       => Empty()
        }) -- "(" -- mkList(args, ",") -- ")" -- body
    }
    trait Property extends Node;

    /* Actual Node Types */
    case class Block(statements:List[Statement]) extends Statement {
        /* note: ECMAScript 6 Actually treat a block as a scope as well */
        val pattern = "{" -- mkList(statements) -- "}"
    }
    case object Break extends ControlFlow {
        val pattern = "break;":CodeGeneratePattern
    }
    case class Catch(variable:$, cond:Option[Expression], body:Block) extends Node {
        val pattern = "catch(" -- variable -- (cond match {
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
    case class ForIn(iterator:ForLoopInitializer, iterationObject:Expression, body:Statement) extends ForLoop {
        val pattern = "for(" -- iterator -- " in " -- iterationObject -- ")" -- body
    }
    case class ForEach(iterator:ForLoopInitializer, iterationObject:Expression, body:Statement) extends ForLoop {
        val pattern = "for each(" -- iterator -- " in " -- iterationObject -- ")" -- body
    }
    case class For(initial:ForLoopInitializer, cond:Expression, inc:Expression, body:Statement) extends ForLoop {
        val pattern = "for(" -- initial -- ";" -- cond -- ";" -- inc -- ")" -- body
    }
    case class Call(target:Expression, args:List[Expression]) extends Expression {
        val pattern = target -- "(" -- mkList(args, ",") -- ")"
    }
    case class FuncExp(name:Option[$], args:List[$], body:Block, locals:Set[String]) extends Function(name, args, body, locals) with Expression {
        override val pattern = "function" -- shared
    }
    case class FuncDef(name:Option[$], args:List[$], body:Block, locals:Set[String]) extends Function(name, args, body, locals) with Statement {
        override val pattern =  "function"-- shared -- ";"
    }
    case class If(cond:Expression, thenClause:Statement, elseCluase:Option[Statement]) extends ControlFlow {
        val pattern = "if(" -- cond -- ")" -- thenClause -- (elseCluase match {
            case None => Empty() 
            case Some(elseCluase) => " else " -- elseCluase
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
    case object This extends Expression {   /* This is the only exception, because the value of this can change */
        val pattern = "this":CodeGeneratePattern
    }
    case object Debugger extends Statement {
        val pattern = "debugger;":CodeGeneratePattern
    }
    case class Program(parts:List[Statement]) extends Node {
        val pattern = mkList(parts, "")
    }
    case class ?:(cond:Expression, trueExpr:Expression, falseExpr:Expression) extends Expression{
        val pattern = cond -- "?" -- trueExpr -- ":" -- falseExpr
    }
    case class New(target:Expression, args:List[Expression]) extends Expression{ /* initializer is not standard syntax seems not useful */
        val pattern = "new " -- target -- " " -- (if(args.length > 0) "(" -- mkList(args, ",") -- ")" else Empty())
    }
    case class :::(left:Expression, right:Expression) extends Property {
        val pattern = left -- ":" -- right
    }
    case class Dict(props:List[Property]) extends Expression {
        val pattern = "{" -- mkList(props, ",") -- "}"
    }
    case class PE(expr:Expression) extends Expression {
        val pattern = "(" -- expr -- ")"
    }
    case class ->(left:Expression, right:$) extends Expression{
        val pattern = left -- "." -- right
    }
    case class Reg(expr:String, flg:Option[String]) extends Constant {
        val pattern = Empty() -- "/" -- expr -- "/" -- (flg match {
            case Some(flg) => flg
            case None      => Empty()
        })
    }
    case class Str(value:String) extends Constant {
        val pattern = Empty() -- "\"" -- value -- "\""
    }
    case class Return(what:Option[Expression]) extends ControlFlow{
        val pattern = "return" -- (what match {
            case Some(what)  =>  " " -- what 
            case None        =>  Empty()
        }) -- ";"
    }
    case class Case(test:Option[Expression], statements:List[Statement]) extends Node {
        val pattern = test match {
            case Some(test)  => "case " -- test -- ":" -- mkList(statements, "")
            case None        => "default:" -- mkList(statements, "")
        }
    }
    case class Switch(test:Expression, cases:List[Case]) extends ControlFlow {
        val pattern = "switch(" -- test -- "){" -- mkList(cases, " ") -- "}"
    }
    case class Throw(expr:Expression) extends ControlFlow {
        val pattern = "throw " -- expr -- ";"
    }
    case class $_(opcode:String, operand:Expression) extends Expression {
        val pattern = opcode -- operand
    }
    case class _$(opcode:String, operand:Expression) extends Expression {
        val pattern = operand -- opcode
    }
    case class VarDecl(name:$, initval:Option[Expression]) extends Node {
        val pattern = name -- (initval match {
            case Some(value) => "=" -- value
            case None        => Empty()
        })
    }
    case class DefineStmt(how:String, what:List[VarDecl]) extends Statement {
        val pattern = Empty() -- how -- " " -- mkList(what, ",") -- ";"
    }
    case class DefineInit(how:String, what:List[VarDecl]) extends ForLoopInitializer {
        val pattern = Empty() -- how -- " " -- mkList(what, ",")
    }
}


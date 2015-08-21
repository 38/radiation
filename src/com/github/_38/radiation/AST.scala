import scala.language.postfixOps
import scala.language.implicitConversions

import org.mozilla.javascript.{ast => RhinoAST, Parser, Node => RhinoNode, Token => RhinoToken}
import com.github._38.radiation.pattern.{Conversions, Info, Pattern, Empty, Compound}
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
	case class Location(val line:Int, val column:Int);
	/** The base class for all AST Nodes */
	abstract class Node {
		val pattern:Pattern
		val cargs:Seq[AnyRef]    /* This is used because we need to use refelection to create new object */
		var location:Option[Location] = None
		lazy val length:Int = pattern length
		/* change the child of the node */
		def change(what:Seq[AnyRef]):Node = {
			def buildArgs(prev:Seq[AnyRef], curr:Seq[AnyRef]):List[AnyRef] = (prev, curr) match {
				case (p :: ps, c :: cs) => {
					p match {
						case p:Option[_] => Some(c) :: buildArgs(ps, cs)
						case _        => c :: buildArgs(ps, cs)
					}
				}
				case (List(), List())   => List()
				case _                  => throw new Exception("Invalid number of args");  /* TODO define a new exception type */
			}
			val args = buildArgs(cargs, what).toArray[AnyRef]
			val cons = this.getClass.getConstructors()(0)
			cons.newInstance(args:_*).asInstanceOf[Node]
		}
		/* traverse the AST */
		def traverse(transform:Node => Node):Node = {
			val processed = transform(this)
			if(processed ne this) processed   /* The node has been touched, do not go deeper */
			else
			{
				def processList(list:List[Node]):List[Node] = list match {
					case x :: xs => {
						val traverseRes = x traverse transform
						val next      = processList(xs)
						traverseRes match {
							case Bundle(what) => what match {
								case List(identity) if identity eq x => list
								case _                               => what ++ next
							}
							case newNode:Node => {
								val next = processList(xs)
								if((newNode eq x) && (next eq xs)) list
								else newNode :: next
							}
						}
					}
					case List()    => List()
				}
				val childRes = cargs map ((x:AnyRef) => (x match {
					case list:List[Node] => {
						val toProcess = transform(Begin(this)) match {
							case Patch(begin, what, howmany) => list.patch(begin, what, howmany)
							case _                           => list
						};
						val processed = processList(list);
						transform(End(this)) match {
							case Patch(begin, what, howmany) => processed.patch(begin, what, howmany)
							case _                           => processed
						}
					}
					case Some(node:Node) => node traverse transform
					case node:Node       => node traverse transform
					case whaterver       => whaterver
				}));
				val changed = (false /: (childRes zip cargs))((x,y) => (x || (y._1 ne y._2)))
				if(changed) this.change(childRes)
				else this
			}
		}
		def targetCode:String = pattern render
		def targetCodeInfo:List[Info] = pattern info
		def apply(loc:Location) {
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
		    val ast = fromString("function a(x,y,z) { return x + y + z;}");
		    System.out.println(ast.change(0, List[Statement]()))
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
						case None           => _listImpl(xs)
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
			case n:RhinoAST.PropertyGet           => ->(n.getLeft.required[Expression], n.getRight.required[Id])
			
			case n:RhinoAST.ArrayLiteral          => Lst(n.getElements.list[Expression])
			case n:RhinoAST.AstRoot               => Program(n.list[Statement])
			case n:RhinoAST.Block                 => Block(n.list[Statement])
			case _:RhinoAST.BreakStatement        => Break
			case n:RhinoAST.CatchClause           => Catch(n.getVarName.required[Id], n.getCatchCondition.optional[Expression], n.getBody.required[Block])
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
					case RhinoAST.FunctionNode.FUNCTION_STATEMENT => FuncDef(n.getFunctionName.optional[Id],
					                                                         n.getParams.list[Id],
					                                                         n.getBody.required[Block],
					                                                         getLocals(n))
					case _                                        => FuncExp(n.getFunctionName.optional[Id],
					                                                         n.getParams.list[Id],
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
			case n:RhinoAST.Name                  => Id(n.getString)
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
			case n:RhinoAST.VariableInitializer   => VarDecl(n.getTarget.required[Id], n.getInitializer.optional[Expression])
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
		override def targetCodeInfo:List[Info] = labeledPattern info
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
	trait VirtualNode extends Node {   /* Do not use this in the actual tree */
		val pattern = "Oops, you shouldn't see this":Pattern
		val cargs = Seq()
	}
	abstract class Function(name:Option[Id], args:List[Id], body:Block, locals:Set[String]) extends Scope {
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
		val cargs   = Seq(statements)
	}
	case object Break extends ControlFlow {
		val pattern = "break;":Pattern
		val cargs   = Seq()
	}
	case class Catch(variable:Id, cond:Option[Expression], body:Block) extends Node {
		val pattern = "catch(" -- variable -- (cond match {
			case Some(cond) => " if " -- cond
			case None => Empty()
		}) -- ")" -- body
		val cargs   = Seq(variable, cond, body)
	}
	case object Continue extends ControlFlow {
		val pattern = "continue;":Pattern;
		val cargs   = Seq()
	}
	case class DoWhile(body:Statement, cond:Expression) extends Loop {
		val pattern = "do " -- body -- "while(" -- cond -- ");"
		val cargs   = Seq(body, cond)
	}
	case class Index(target:Expression, key:Expression) extends Expression  with LHS{
		val pattern = target -- "[" -- key -- "]"
		val cargs   = Seq(target, key)
	}
	case class Lst(values:List[Expression]) extends Expression {
		val pattern = "[" -- mkList(values, ",") -- "]"
		val cargs   = Seq(values)
	}
	case class Num(value:String) extends Constant {
		val pattern = value:Pattern;
		val cargs   = Seq(value)
	}
	case class Try(tryBlock:Block, catchBlocks:List[Catch], finallyBlock:Option[Block]) extends Statement {
		val pattern = "try" -- tryBlock -- mkList(catchBlocks) -- (finallyBlock match {
			case Some(finallyBlock) => "finally " -- finallyBlock
			case None               => Empty()
		  })
		val cargs   = Seq(tryBlock, catchBlocks, finallyBlock)
	}
	case class Id(text:String) extends Expression with LHS {
		val pattern = text:Pattern
		val cargs   = Seq(text)
	}
	case object EmptyExpr extends Expression {
		val pattern = Empty()
		val cargs   = Seq()
	}
	case object EmptyStat extends Statement {
		val pattern = ";":Pattern
		val cargs   = Seq()
	}
	case class ExpressionStat(expression:Expression) extends Statement {
		val pattern = expression -- ";"
		val cargs   = Seq(expression)
	}
	case class ForIn(iterator:ForLoopInitializer, iterationObject:Expression, body:Statement) extends ForLoop {
		val pattern = "for(" -- iterator -- " in " -- iterationObject -- ")" -- body
		val cargs   = Seq(iterator, iterationObject, body)
	}
	case class ForEach(iterator:ForLoopInitializer, iterationObject:Expression, body:Statement) extends ForLoop {
		val pattern = "for each(" -- iterator -- " in " -- iterationObject -- ")" -- body
		val cargs   = Seq(iterator, iterationObject, body)
	}
	case class For(initial:ForLoopInitializer, cond:Expression, inc:Expression, body:Statement) extends ForLoop {
		val pattern = "for(" -- initial -- ";" -- cond -- ";" -- inc -- ")" -- body
		val cargs   = Seq(initial, cond, inc, body)
	}
	case class Call(target:Expression, args:List[Expression]) extends Expression {
		val pattern = target -- "(" -- mkList(args, ",") -- ")"
		val cargs   = Seq(target, args)
	}
	case class FuncExp(name:Option[Id], args:List[Id], body:Block, locals:Set[String]) extends Function(name, args, body, locals) with Expression {
		override val pattern = "function" -- shared
		val cargs   = Seq(name, args, body, locals)
	}
	case class FuncDef(name:Option[Id], args:List[Id], body:Block, locals:Set[String]) extends Function(name, args, body, locals) with Statement {
		override val pattern =  "function"-- shared -- ";"
		val cargs   = Seq(name, args, body, locals)
	}
	case class If(cond:Expression, thenClause:Statement, elseCluase:Option[Statement]) extends ControlFlow {
		val pattern = "if(" -- cond -- ")" -- thenClause -- (elseCluase match {
			case None => Empty()
			case Some(elseCluase) => " else " -- elseCluase
		})
		val cargs   = Seq(cond, thenClause, elseCluase)
	}
	case class BinOp(op:String, left:Expression, right:Expression) extends Expression {
		val pattern = left -- op -- right
		val cargs   = Seq(op, left, right)
	}
	case object True extends Constant {
		val pattern = "true":Pattern
		val cargs   = Seq()
	}
	case object False extends Constant {
		val pattern = "false":Pattern
		val cargs   = Seq()
	}
	case object Null extends Constant {
		val pattern = "null":Pattern
		val cargs   = Seq()
	}
	case object This extends Expression {   /* This is the only exception, because the value of this can change */
		val pattern = "this":Pattern
		val cargs   = Seq()
	}
	case object Debugger extends Statement {
		val pattern = "debugger;":Pattern
		val cargs   = Seq()
	}
	case class Program(parts:List[Statement]) extends Node {
		val pattern = mkList(parts, "")
		val cargs   = Seq(parts)
	}
	case class ?:(cond:Expression, trueExpr:Expression, falseExpr:Expression) extends Expression{
		val pattern = cond -- "?" -- trueExpr -- ":" -- falseExpr
		val cargs   = Seq(cond, trueExpr, falseExpr)
	}
	case class New(target:Expression, args:List[Expression]) extends Expression{ /* initializer is not standard syntax seems not useful */
		val pattern = "new " -- target -- " " -- (if(args.length > 0) "(" -- mkList(args, ",") -- ")" else Empty())
		val cargs   = Seq(target, args)
	}
	case class :::(left:Expression, right:Expression) extends Property {
		val pattern = left -- ":" -- right
		val cargs   = Seq(left, right)
	}
	case class Dict(props:List[Property]) extends Expression {
		val pattern = "{" -- mkList(props, ",") -- "}"
		val cargs   = Seq(props)
	}
	case class PE(expr:Expression) extends Expression {
		val pattern = "(" -- expr -- ")"
		val cargs   = Seq(expr)
	}
	case class ->(left:Expression, right:Id) extends Expression{
		val pattern = left -- "." -- right
		val cargs   = Seq(left, right)
	}
	case class Reg(expr:String, flg:Option[String]) extends Constant {
		val pattern = Empty() -- "/" -- expr -- "/" -- (flg match {
			case Some(flg) => flg
			case None      => Empty()
		})
		val cargs   = Seq(expr, flg)
	}
	case class Str(value:String) extends Constant {
		val pattern = Empty() -- "\"" -- value -- "\""
		val cargs   = Seq(value)
	}
	case class Return(what:Option[Expression]) extends ControlFlow{
		val pattern = "return" -- (what match {
			case Some(what)  =>  " " -- what
			case None        =>  Empty()
		}) -- ";"
		val cargs   = Seq(what)
	}
	case class Case(test:Option[Expression], statements:List[Statement]) extends Node {
		val pattern = test match {
			case Some(test)  => "case " -- test -- ":" -- mkList(statements, "")
			case None        => "default:" -- mkList(statements, "")
		}
		val cargs   = Seq(test, statements)
	}
	case class Switch(test:Expression, cases:List[Case]) extends ControlFlow {
		val pattern = "switch(" -- test -- "){" -- mkList(cases, " ") -- "}"
		val cargs   = Seq(test, cases)
	}
	case class Throw(expr:Expression) extends ControlFlow {
		val pattern = "throw " -- expr -- ";"
		val cargs   = Seq(expr)
	}
	case class $_(opcode:String, operand:Expression) extends Expression {
		val pattern = opcode -- operand
		val cargs   = Seq(opcode, operand)
	}
	case class _$(opcode:String, operand:Expression) extends Expression {
		val pattern = operand -- opcode
		val cargs   = Seq(opcode, operand)
	}
	case class VarDecl(name:Id, initval:Option[Expression]) extends Node {
		val pattern = name -- (initval match {
			case Some(value) => "=" -- value
			case None        => Empty()
		})
		val cargs   = Seq(name, initval)
	}
	case class DefineStmt(how:String, what:List[VarDecl]) extends Statement {
		val pattern = Empty() -- how -- " " -- mkList(what, ",") -- ";"
		val cargs   = Seq(how, what)
	}
	case class DefineInit(how:String, what:List[VarDecl]) extends ForLoopInitializer {
		val pattern = Empty() -- how -- " " -- mkList(what, ",")
		val cargs   = Seq(how, what)
	}
	case class Begin(what:Node) extends VirtualNode;   /* Indicates we are about to processing the Node */
	case class End(what:Node) extends VirtualNode;     /* Indicates we are almost done with the node */
	case class Patch(begin:Int, what:List[Node], howmany:Int) extends VirtualNode;  /* Callback needs to patch the list */
	case class Bundle(what:List[Node]) extends VirtualNode;
	case object Nop extends VirtualNode;   /* Do Nothing */
}


import scala.language.postfixOps
import scala.language.implicitConversions

import org.mozilla.javascript.{ast => RhinoAST, Parser, Node => RhinoNode, Token => RhinoToken, ScriptRuntime}
import com.github._38.radiation.pattern.{Conversions, Info, Pattern, Empty, Compound}
import scala.math.max
import scala.reflect.{ClassTag, classTag}

import java.io.FileReader

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
	
	/** Define the top level API for the Radiation JS AST */
	object ASTParser {
		/** Implicitly convert helper Rhino AST to Radiation AST
		 *  @param from The input Rhino AST root
		 */
		class RhinoASTConversion(from:RhinoAST.AstNode) {
			/** Get the result AST */
			def AST = Node.rhinoAstToInternal(from)
		}
		/** The implicit converter that convers Rhino AST to Radiation AST Automatically
		 *  @param from The source Rhino AST
		 *  @return The target Radiation AST
		 */
		implicit def toConversionObject(from: RhinoAST.AstNode) = new RhinoASTConversion(from)
		/** Parse a string and return a Radiation AST
		 *  @param program The program text
		 *  @return The result AST Root
		 */
		def fromString(program:String):Node = (new Parser).parse(program, null, 0).AST
		/** Parse a file and return AST
		 *  @param  path the file path
		 *  @return the result ast
		 */
		 def fromFile(path:String):Node = {
             val prev_postConversion = Node.postNodeConvert
             Node.postNodeConvert = ((n:Node) => {
                 n(Location(0, Node.currentPosition))
                 n
             })
             val result = (new Parser).parse(new FileReader(path), path, 0).AST
             Node.postNodeConvert = prev_postConversion
             result
         }
	}
	
	/** Describe a Location in a source code
	 *  @param line Line number in the source code
	 *  @param column the column number, the offset can be out of range, so we need use normalize function
	 */
	case class Location(val line:Int, val column:Int) {
        /** normalize means figure out the undefined line number, and make sure line offset
         *  is in the range of the line
         *  @param lines the list of offset of the beginning of each line
         */
        def normalize(lines:List[Int]) = {
            val base = lines(line)
            val offset = column
            var (l,r) = (0, lines.size - line)
            while(r - l > 1) {
                val m = (l + r) / 2
                if(lines(m) - base <= offset) l = m
                else r = m
            }
            new Location(l + line, column + base - lines(l))
        }
    }
    
	
	
	/** The base class for all AST Nodes */
	abstract class Node {
		/** Code pattern, how to convert this AST to program text */
		val pattern:Pattern
		/** Constructor Args, the Arguments used to build this node */
		val cargs:Seq[AnyRef]    /* This is used because we need to use refelection to create new object */
		/** Source code locations, can be empty if it's a generated node */
		var location:Option[Location] = None
		/** The length of the program text of this node */
		lazy val length:Int = pattern length
		/** Make a new node with modifed constructor arguments
		 *  @note All Radiation AST Node are Immutable, if we needs to change a node, we have to build a new one
		 *        This gives us the benift that we are able to share unchanged subtrees among different ASTs and
		 *        Do not need to worry about the shared part gets changed
		 *  @param what The new constrctor args
		 *  @return The new node build from the args, note that the node type is same as this
		 */
		def change(what:Seq[AnyRef]):Node = {
			def buildArgs(prev:Seq[AnyRef], curr:Seq[AnyRef]):List[AnyRef] = (prev, curr) match {
				case (p :: ps, c :: cs) => {
					p match {
						case p:Option[_] => (c match {
                            case None => None
                            case _    =>Some(c) 
                        }):: buildArgs(ps, cs)
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
		/** traverse the AST, make transformation at the same time
		 *  @note There's serveral virtual nodes that represents the update command,
		 *        They are not real AST Node, but communication packets used between
		 *        This function and transformation function
		 *        Node.stack is used as a AST Node stack containing the path to current Node
		 *        This might be useful we analysing the code
		 *  @param transform A function from a Node to another Node. If the input and the output
		 *                   Is the same reference, continue traverse its child.
		 *                   Otherwise update the AST, construct a new AST contains newly returned Node
		 *  @return The new AST after transformation
		 */
		def traverse(transform:Node => Node):Node = {
			Node.stack push this
			val result = /*if(processed ne this) processed
			else*/
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
			val processed = transform(result)
			Node.stack.pop
			processed
		}
		/** Get the program text from this AST */
		def targetCode:String = pattern render
		/** A list contains Node to Source Code Location information */
		def targetCodeInfo:List[Info] = pattern info
		/** Support for update the location by calling node(newloc) */
		def apply(loc:Location) {
			location = Some(loc)
		}
		/** Try to convert the node to specified type of node, None if not possible */
		def as[T <: Node: ClassTag]:Option[T] = this match {
			case what if classTag[T].runtimeClass.isInstance(this) => Some(this.asInstanceOf[T])
			case _ => None
		}
	}
	object Node {
		import scala.collection.JavaConverters._
		import scala.collection.mutable.Stack
		/** Stack for AST traversing
		 *  @note Not thread-safe, but don't care for now
		 */
		val stack:Stack[Node] = Stack()
        /** The offset of current node, used when parse a file input, evil side-effect :( */
        var currentPosition = 0
        /** The callback function after a node is converted, introducing evil side-effect :( */
        var postNodeConvert = ((n:Node) => n)
        
		/** We have much more strict syntax checking than rhino, might rise SyntaxError
		 *  Even if the rhino parser accepted the program
		 *  @param message The error message to show
		 */
		class SyntaxError(message:String) extends Exception {
			override def toString = "Syntax Error : " + message;
		}
		/** Helper class for conversion from Rhino AST to internal AST reprentations
		 *  @param from The source AST Node
		 */
		class RhinoASTHelper(from:RhinoAST.AstNode){
			/** Return an optional Radiation AST Node */
			def optional[T <: Node : ClassTag]:Option[T] = if(from != null) rhinoAstToInternal(from).as[T] else None
			/** Return a Required AST Node, if not possible, throw Syntax error */
			def required[T <: Node : ClassTag]:T         = if(from != null) {
				rhinoAstToInternal(from).as[T] match {
					case Some(value) => value
					case None        => throw new SyntaxError("Type " + classTag[T] + " is expected, but we got a " + rhinoAstToInternal(from));
				}
			} else throw new SyntaxError("Required field not found")
			
			/** Convert the node to a list of Radiation Node (Typically for block, ast root etc ) */
			def list[T <: Node : ClassTag]:List[T] = {
				if(from == null) List[T]()
				else _listImpl[T](from.asScala.toList)
			}
			private def _listImpl[T <: Node : ClassTag](xs:List[RhinoNode]):List[T] = xs match {
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
		/** Helper class for conversion from Java List to Radiation List
		 *  @param from the source Rhino AST Node
		 */
		class JavaListHelper(from:java.util.List[_ <: RhinoAST.AstNode]) {
			/** convert to Radiation AST Node List */
			def list[T <: Node: ClassTag]: List[T] = {
				import scala.collection.JavaConverters._
				if(from == null) List[T]()
				else _listImpl[T](from.asScala.toList)
			}
			private def _listImpl[T <: Node: ClassTag](xs: List[RhinoAST.AstNode]):List[T] = xs match {
				case x :: xs => rhinoAstToInternal(x).as[T] match {
					case Some(n)  => n :: _listImpl(xs)
					case None     => _listImpl(xs)
				}
				case List() => List()
			}
		}
		/** Get the local symbols from a RhinoAST Scope
		 *  @param scope The rhino scope node
		 */
		private def _getLocals(scope: RhinoAST.Scope):Set[String] =
		    if(scope.getSymbolTable != null) scope.getSymbolTable.keySet.asScala.toSet else Set()
		/** Convert the Rhino Opcode to Operator in Plain text
		 *  @param opcode the Rhino opcode
		 */
		private def _operatorString(opcode:Int) = (if(opcode == RhinoToken.IN || opcode == RhinoToken.INSTANCEOF) " " else "") + 
           (RhinoAST.AstNode operatorToString opcode) + 
		   (if(opcode == RhinoToken.TYPEOF || opcode == RhinoToken.DELPROP || opcode == RhinoToken.VOID || opcode == RhinoToken.IN || opcode == RhinoToken.INSTANCEOF) " " else "")
		/** Convert the Rhino AST Node to helper class */
		implicit def toHelper(from:RhinoAST.AstNode):RhinoASTHelper = new RhinoASTHelper(from)
		/** Convert Java list to Helper class */
		implicit def toHelper(from:java.util.List[_ <: RhinoAST.AstNode]):JavaListHelper = new JavaListHelper(from)
		/** Do actual convert from the Rhino AST to Radiation AST */
		def _rhinoAstToInternalImpl(rhino_node:RhinoAST.AstNode): Node = rhino_node match {
			case n:RhinoAST.ObjectProperty        => :::(n.getLeft.required[Expression], n.getRight.required[Expression])
			case n:RhinoAST.NewExpression         => New(n.getTarget.required[Expression], n.getArguments.list[Expression])
			case n:RhinoAST.PropertyGet           => ->(n.getLeft.required[Expression], n.getRight.required[Id])
			
			case n:RhinoAST.ArrayLiteral          => Lst(n.getElements.list[Expression])
			case n:RhinoAST.AstRoot               => Program(n.list[Statement], _getLocals(n))
			case n:RhinoAST.Block                 => Block(n.list[Statement])
			case _:RhinoAST.BreakStatement        => Break
			case n:RhinoAST.CatchClause           => Catch(n.getVarName.required[Id], n.getCatchCondition.optional[Expression], n.getBody.required[Block])
			case n:RhinoAST.ConditionalExpression => ?:(n.getTestExpression.required[Expression],
			                                            n.getTrueExpression.required[Expression],
			                                            n.getFalseExpression.required[Expression])
			case n:RhinoAST.ContinueStatement     => Continue
			case n:RhinoAST.DoLoop                => DoWhile(n.getBody.required[Statement], n.getCondition.required[Expression])
            case n:RhinoAST.WhileLoop             => While(n.getCondition.required[Expression], n.getBody.required[Statement])
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
					                                                         _getLocals(n))
					case _                                        => FuncExp(n.getFunctionName.optional[Id],
					                                                         n.getParams.list[Id],
					                                                         n.getBody.required[Block],
					                                                         _getLocals(n))
				}
			}
			case n:RhinoAST.InfixExpression       => BinOp(_operatorString(n.getType),
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
			case n:RhinoAST.StringLiteral         => Str(n.getQuoteCharacter.toString, n.getValue)
			case n:RhinoAST.SwitchCase            => Case(n.getExpression.optional[Expression], n.getStatements.list[Statement])
			case n:RhinoAST.SwitchStatement       => Switch(n.getExpression.required[Expression], n.getCases.list[Case])
			/* As Meta-Data Node, Symbol Node is ignored */
			case n:RhinoAST.ThrowStatement        => Throw(n.getExpression.required[Expression])
			case n:RhinoAST.TryStatement          => Try(n.getTryBlock.required[Block], n.getCatchClauses.list[Catch], n.getFinallyBlock.optional[Block])
			case n:RhinoAST.UnaryExpression       =>
			    if(n.isPostfix) _$(_operatorString(n.getType), n.getOperand.required[Expression])
			    else            $_(_operatorString(n.getType), n.getOperand.required[Expression])
			case n:RhinoAST.VariableInitializer   => VarDecl(n.getTarget.required[Id], n.getInitializer.optional[Expression])
			case n:RhinoAST.VariableDeclaration   => {
				val how = (n isVar, n isLet, n isConst) match {
					case (true, false, false) => "var"
					case (false, true, false) => "let"
					case (false, false, true) => "const"
					case _                    => "Oops, you shouldn't see this"
				}
				if(n isStatement) DefineStmt(how, n.getVariables.list[VarDecl]) else DefineInit(how, n.getVariables.list[VarDecl])
			}
			case n:RhinoAST.Scope                 => Block(n.list[Statement]) /* ECMAScript5 do not have block scopes, only scope is function scope */
		}
        /** Convert a rhino ast to the Radiation one, introducing side effect ! */
        def rhinoAstToInternal(node:RhinoAST.AstNode) = {
            val tmp = _rhinoAstToInternalImpl(node)
            currentPosition = currentPosition + node.getPosition
            val result = postNodeConvert(tmp)
            currentPosition = currentPosition - node.getPosition
            result
        }
	}
	
	/** AST node for a statement */
	trait Statement extends Node {
		/** The label set of the statement */
		var labels:List[String] = List();
		/** The shared pattern if this statement contains a label */
		def labeledPattern = if(labels.length > 0) ((new Compound(List())) /: labels)((p,s) => p -- s -- ":") -- pattern else pattern
		override lazy val length:Int = labeledPattern length
		override def targetCode:String = labeledPattern render
		override def targetCodeInfo:List[Info] = labeledPattern info
	}
	/** AST Node that can be used in for loop initializer for(.....;;) */
	trait ForLoopInitializer extends Node;
	/** AST node for a L-value expression */
	trait LHS extends Node;   /* means the lefthand side of assignment, var a = 3 or a = 3 or a["x"] = 3 */
	/** AST represents an expression */
	trait Expression extends Node with ForLoopInitializer;
	/** AST that carry local variables */
	trait Scope extends Node ;
	/** AST node for all scope that is not global scope */
	trait LocalScope extends Scope {
		val localSymbols:Set[String]
	}
	/** AST node for Global scope */
	trait GlobalScope extends Scope {
		val globalSymbols:Set[String]
	}
	/** AST node for Control flow related statements */
	trait ControlFlow extends Statement;
	/** AST node for all loops */
	trait Loop extends ControlFlow;
	/** Any type of For loop */
	trait ForLoop extends Loop;
	/** Any expression that has static value */
	trait Constant extends Expression; /* For some literal constant */
	/** Anything that is not a real AST Node */
	trait VirtualNode extends Node {   /* Do not use this in the actual tree */
		val pattern = "Oops, you shouldn't see this":Pattern
		val cargs = Seq()
	}
	/** Any function declerations */
	abstract class Function(name:Option[Id], args:List[Id], body:Block, locals:Set[String]) extends LocalScope {
		val localSymbols = locals
		val shared = " " -- (name match {
			case Some(name) => name
			case None       => Empty()
		}) -- "(" -- mkList(args, ",") -- ")" -- body
	}
	/** Any properties for an object in Object literal */
	trait Property extends Node;
	
	
	/** Represents a block {stmt; stmt; .... }
	 *  @note ECMAScript 6 Actually treat a block as a scope as well, but not implemented by all browsers
	 */
	case class Block(statements:List[Statement]) extends Statement {
		val pattern = "{" -- mkList(statements) -- "}"
		val cargs   = Seq(statements)
	}
	/** Break statement */
	case object Break extends ControlFlow {
		val pattern = "break;":Pattern
		val cargs   = Seq()
	}
	/** A catch clause catch(e) { .... } */
	case class Catch(variable:Id, cond:Option[Expression], body:Block) extends Node {
		val pattern = "catch(" -- variable -- (cond match {
			case Some(cond) => " if " -- cond
			case None => Empty()
		}) -- ")" -- body
		val cargs   = Seq(variable, cond, body)
	}
	/** continue statement */
	case object Continue extends ControlFlow {
		val pattern = "continue;":Pattern;
		val cargs   = Seq()
	}
	/** do { ... } while */
	case class DoWhile(body:Statement, cond:Expression) extends Loop {
		val pattern = "do " -- body -- "while(" -- cond -- ");"
		val cargs   = Seq(body, cond)
	}
	/** element get [...] */
	case class Index(target:Expression, key:Expression) extends Expression  with LHS{
		val pattern = target -- "[" -- key -- "]"
		val cargs   = Seq(target, key)
	}
	/** List Literal [1,2,3] */
	case class Lst(values:List[Expression]) extends Expression {
		val pattern = "[" -- mkList(values, ",") -- "]"
		val cargs   = Seq(values)
	}
	/** Number Literal */
	case class Num(value:String) extends Constant {
		val pattern = value:Pattern;
		val cargs   = Seq(value)
	}
	/** Try statement */
	case class Try(tryBlock:Block, catchBlocks:List[Catch], finallyBlock:Option[Block]) extends Statement {
		val pattern = "try" -- tryBlock -- mkList(catchBlocks) -- (finallyBlock match {
			case Some(finallyBlock) => "finally " -- finallyBlock
			case None               => Empty()
		  })
		val cargs   = Seq(tryBlock, catchBlocks, finallyBlock)
	}
    /** The Identifier */
	case class Id(text:String) extends Expression with LHS {
		val pattern = text:Pattern
		val cargs   = Seq(text)
	}
    /** Simply means nothing */
	case object EmptyExpr extends Expression {
		val pattern = Empty()
		val cargs   = Seq()
	}
    /** Simply a ';' */
	case object EmptyStat extends Statement {
		val pattern = ";":Pattern
		val cargs   = Seq()
	}
    /** Wrap an expression to a statement */
	case class ExpressionStat(expression:Expression) extends Statement {
		val pattern = expression -- ";"
		val cargs   = Seq(expression)
	}
    /** for(x in y) { ... } loop */
	case class ForIn(iterator:ForLoopInitializer, iterationObject:Expression, body:Statement) extends ForLoop {
		val pattern = "for(" -- iterator -- " in " -- iterationObject -- ")" -- body
		val cargs   = Seq(iterator, iterationObject, body)
	}
    /** for each(x in y) loop */
	case class ForEach(iterator:ForLoopInitializer, iterationObject:Expression, body:Statement) extends ForLoop {
		val pattern = "for each(" -- iterator -- " in " -- iterationObject -- ")" -- body
		val cargs   = Seq(iterator, iterationObject, body)
	}
    /** for(i = 0; i &lt; j; i ++) loop */
	case class For(initial:ForLoopInitializer, cond:Expression, inc:Expression, body:Statement) extends ForLoop {
		val pattern = "for(" -- initial -- ";" -- cond -- ";" -- inc -- ")" -- body
		val cargs   = Seq(initial, cond, inc, body)
	}
    /** A function invocation */
	case class Call(target:Expression, args:List[Expression]) extends Expression {
		val pattern = target -- "(" -- mkList(args, ",") -- ")"
		val cargs   = Seq(target, args)
	}
    /** A function define as an expression */
	case class FuncExp(name:Option[Id], args:List[Id], body:Block, locals:Set[String]) extends Function(name, args, body, locals) with Expression {
		override val pattern = "function" -- shared
		val cargs   = Seq(name, args, body, locals)
	}
    /** A function define statement */
	case class FuncDef(name:Option[Id], args:List[Id], body:Block, locals:Set[String]) extends Function(name, args, body, locals) with Statement {
		override val pattern =  "function"-- shared -- ";"
		val cargs   = Seq(name, args, body, locals)
        def asExpr = FuncExp(name, args, body, locals)
	}
    /** An if(cond) then(); else else(); statement */
	case class If(cond:Expression, thenClause:Statement, elseCluase:Option[Statement]) extends ControlFlow {
		val pattern = "if(" -- cond -- ")" -- thenClause -- (elseCluase match {
			case None => Empty()
			case Some(elseCluase) => " else " -- elseCluase
		})
		val cargs   = Seq(cond, thenClause, elseCluase)
	}
    /** Binary operators */
	case class BinOp(op:String, left:Expression, right:Expression) extends Expression {
		val pattern = left -- op -- right
		val cargs   = Seq(op, left, right)
	}
    /** Constant 'True' */
	case object True extends Constant {
		val pattern = "true":Pattern
		val cargs   = Seq()
	}
    /** Constant 'False' */
	case object False extends Constant {
		val pattern = "false":Pattern
		val cargs   = Seq()
	}
    /** Constant 'Null' */
	case object Null extends Constant {
		val pattern = "null":Pattern
		val cargs   = Seq()
	}
    /** Constant 'this' */
	case object This extends Expression {   /* This is the only exception, because the value of this can change */
		val pattern = "this":Pattern
		val cargs   = Seq()
	}
    /** Keyword debugger */
	case object Debugger extends Statement {
		val pattern = "debugger;":Pattern
		val cargs   = Seq()
	}
    /** root of AST */
	case class Program(parts:List[Statement], globals: Set[String]) extends GlobalScope {
		val globalSymbols = globals
		val pattern = mkList(parts, "")
		val cargs   = Seq(parts, globals)
	}
    /* Tenary operator */
	case class ?:(cond:Expression, trueExpr:Expression, falseExpr:Expression) extends Expression{
		val pattern = cond -- "?" -- trueExpr -- ":" -- falseExpr
		val cargs   = Seq(cond, trueExpr, falseExpr)
	}
    /** A new expression */
	case class New(target:Expression, args:List[Expression]) extends Expression{ /* initializer is not standard syntax seems not useful */
		val pattern = "new " -- target -- " " -- (if(args.length > 0) "(" -- mkList(args, ",") -- ")" else Empty())
		val cargs   = Seq(target, args)
	}
    /** A key value pair in {key:vlaue} literal */
	case class :::(left:Expression, right:Expression) extends Property {
		val pattern = left -- ":" -- right
		val cargs   = Seq(left, right)
	}
    /** {key: value, ...} literal */
	case class Dict(props:List[Property]) extends Expression {
		val pattern = "{" -- mkList(props, ",") -- "}"
		val cargs   = Seq(props)
	}
    /** (expr) */
	case class PE(expr:Expression) extends Expression {
		val pattern = "(" -- expr -- ")"
		val cargs   = Seq(expr)
	}
    /** 'dot' operator x.y */
	case class ->(left:Expression, right:Id) extends Expression{
		val pattern = left -- "." -- right
		val cargs   = Seq(left, right)
	}
    /** Regular expression literal */
	case class Reg(expr:String, flg:Option[String]) extends Constant {
		val pattern = Empty() -- "/" -- expr -- "/" -- (flg match {
			case Some(flg) => flg
			case None      => Empty()
		})
		val cargs   = Seq(expr, flg)
	}
    /** String literal */
	case class Str(quote:String, value:String) extends Constant {
		val pattern = Empty() -- quote -- ScriptRuntime.escapeString(value, quote(0)) -- quote
		val cargs   = Seq(quote, value)
	}
    /** Return statement */
	case class Return(what:Option[Expression]) extends ControlFlow{
		val pattern = "return" -- (what match {
			case Some(what)  =>  " " -- what
			case None        =>  Empty()
		}) -- ";"
		val cargs   = Seq(what)
	}
    /** a switch case */
	case class Case(test:Option[Expression], statements:List[Statement]) extends Node {
		val pattern = test match {
			case Some(test)  => "case " -- test -- ":" -- mkList(statements, "")
			case None        => "default:" -- mkList(statements, "")
		}
		val cargs   = Seq(test, statements)
	}
    /** Switch statement */
	case class Switch(test:Expression, cases:List[Case]) extends ControlFlow {
		val pattern = "switch(" -- test -- "){" -- mkList(cases, " ") -- "}"
		val cargs   = Seq(test, cases)
	}
    /** throw statement */
	case class Throw(expr:Expression) extends ControlFlow {
		val pattern = "throw " -- expr -- ";"
		val cargs   = Seq(expr)
	}
    /** Prefix operators */
	case class $_(opcode:String, operand:Expression) extends Expression {
		val pattern = opcode -- operand
		val cargs   = Seq(opcode, operand)
	}
    /** Suffix operators */
	case class _$(opcode:String, operand:Expression) extends Expression {
		val pattern = operand -- opcode
		val cargs   = Seq(opcode, operand)
	}
    /** Variable declsi a or a = 3 */
	case class VarDecl(name:Id, initval:Option[Expression]) extends Node {
		val pattern = name -- (initval match {
			case Some(value) => "=" -- value
			case None        => Empty()
		})
		val cargs   = Seq(name, initval)
	}
    /** Var Decl statement */
	case class DefineStmt(how:String, what:List[VarDecl]) extends Statement {
		val pattern = Empty() -- how -- " " -- mkList(what, ",") -- ";"
		val cargs   = Seq(how, what)
	}
    /** For(var xxx = 3;;) */
	case class DefineInit(how:String, what:List[VarDecl]) extends ForLoopInitializer {
		val pattern = Empty() -- how -- " " -- mkList(what, ",")
		val cargs   = Seq(how, what)
	}
    /* while(...) xxxxx loop */
    case class While(cond:Expression, body:Statement) extends Loop {
        val pattern = "while(" -- cond -- ")" -- body
        val cargs   = Seq(cond, body)
    }
    /** Virtual Node, AST Traveser provide this to transformer, indicates we are about to process the node */
	case class Begin(what:Node) extends VirtualNode;   /* Indicates we are about to processing the Node */
    /** Virtual Node, AST traverser provide this to transformer, indicates we are finish the process of the node */
	case class End(what:Node) extends VirtualNode;     /* Indicates we are almost done with the node */
    /** Virtual Node, transformer provide this to AST traverser, request to patch a child list */
	case class Patch(begin:Int, what:List[Node], howmany:Int) extends VirtualNode;  /* Callback needs to patch the list */
    /** Virtual Node, transformer provide this to AST traverser, insert the list to current child list */
	case class Bundle(what:List[Node]) extends VirtualNode;
    /** Virtual Node, transformer provide this to AST traverser, means nothing to do, but keep going on to the child */
	case object Nop extends VirtualNode;   /* Do Nothing */
}


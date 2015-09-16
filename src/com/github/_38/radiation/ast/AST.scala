package com.github._38.radiation.ast

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

import org.mozilla.javascript.{Node => RhinoNode, ast => RhinoAST, Token => RhinoToken, ScriptRuntime}
import ScriptRuntime.escapeString

import com.github._38.radiation.source.{Location, NotInSource, InSource}
import RhinoAST.AstNode
import Helper._
import Node.{fromRhinoAst, rhinoConvertHelper, operatorToString}


/** The exception */
class InvalidASTException(val message:String) extends Exception {
	override def toString = "Invalid AST: " + message
}

/** The base class of a node, the AST is represents in a S-Expression-like data structure,
 *  node := (nodeType child1 child2 ... childN) */
trait Node {
	/** The type of the node, should be a type object */
	val nodeType:NodeType
	/** gets the generated code */
	def targetCode:String
	/** gets the generated code length
	 *  @note although this is equivalent to tree.targetCode.length, but this is a faster way to do that */
	val targetCodeLength:Int
	/** gets the last charecter in the target code */
	val lastChar:Char
	/** gets the first charecter in the target code */
	val firstChar:Char
}

object Node {
	/** Implicitly convert a Rhino Ast Node to Node */
	implicit def fromRhinoAst(node:AstNode):Node = node match {
		case n:RhinoAST.ObjectProperty          => :::(n)
		case n:RhinoAST.NewExpression           => New(n)
		case n:RhinoAST.PropertyGet             => ->(n)
		case n:RhinoAST.AstRoot                 => Program(n)
		case n:RhinoAST.ArrayLiteral            => Array(n)
		case n:RhinoAST.Block                   => Block(n)
		case n:RhinoAST.BreakStatement          => Break(n)
		case n:RhinoAST.CatchClause             => Catch(n)
		case n:RhinoAST.ConditionalExpression   => :?(n)
		case n:RhinoAST.ContinueStatement       => Continue(n)
		case n:RhinoAST.DoLoop                  => DoWhile(n)
		case n:RhinoAST.ElementGet              => Index(n)
		case n:RhinoAST.EmptyExpression         => EmptyExpr(n)
		case n:RhinoAST.EmptyStatement          => EmptyStmt(n)
		case n:RhinoAST.ExpressionStatement     => ExprStmt(n)
		case n:RhinoAST.ForLoop                 => For(n)
		case n:RhinoAST.ForInLoop               => if(n.isForEach) ForEach(n) else ForIn(n)
		case n:RhinoAST.FunctionCall            => Call(n)
		case n:RhinoAST.FunctionNode            => if(n.getFunctionType == RhinoAST.FunctionNode.FUNCTION_STATEMENT) FuncStmt(n) else FuncExpr(n)
		case n:RhinoAST.KeywordLiteral          => {
			(n getType) match {
				case RhinoToken.THIS            => This(n)
				case RhinoToken.NULL            => Null(n)
				case RhinoToken.TRUE            => True(n)
				case RhinoToken.FALSE           => False(n)
				case RhinoToken.DEBUGGER        => Debugger(n)
			}
		}
		case n:RhinoAST.IfStatement             => If(n)
		case n:RhinoAST.InfixExpression         => BinOp(n)
		case n:RhinoAST.Name                    => Id(n)
		case n:RhinoAST.NumberLiteral           => Num(n)
		case n:RhinoAST.ObjectLiteral           => Json(n)
		case n:RhinoAST.ParenthesizedExpression => PE(n)
		case n:RhinoAST.RegExpLiteral           => Reg(n)
		case n:RhinoAST.ReturnStatement         => Return(n)
		case n:RhinoAST.StringLiteral           => Str(n)
		case n:RhinoAST.SwitchCase              => Case(n)
		case n:RhinoAST.SwitchStatement         => Switch(n)
		case n:RhinoAST.ThrowStatement          => Throw(n)
		case n:RhinoAST.TryStatement            => Try(n)
		case n:RhinoAST.UnaryExpression         => if(n.isPostfix) Postfix(n) else Prefix(n)
		case n:RhinoAST.VariableInitializer     => Decl(n)
		case n:RhinoAST.VariableDeclaration     => if(n.isStatement) DeclList(n) else DeclForLoopInit(n)
		case n:RhinoAST.WhileLoop               => While(n)
		case n:RhinoAST.Scope                   => Block(n)
	}
	
	/** Implicitly convert a Rhino Ast Node to as list of its children */
	implicit def iterableToList(node:AstNode):List[Node] = node.asScala.toList map (x => fromRhinoAst(x.asInstanceOf[AstNode]))
	
	/** The helper class for Ast conversion
	 *  @note Because Scala can't implicitly convert the type when you trying to build a list using ::
	 *        This class is an alternative way to trigger the implicit conversion
	 */
	class RhinoConvertHelper(what:AstNode) {
		/** To convert */
		def asNode:Node = what
	}
	/** Implicitly convert the Rhino Ast Node to helper */
	implicit def rhinoConvertHelper(what:AstNode) = new RhinoConvertHelper(what)
	
	/** Convert the Rhino Opcode to Operator in Plain text
	 *  @param opcode the Rhino opcode
	 */
	def operatorToString(opcode:Int) =
	    (if(opcode == RhinoToken.IN ||
	        opcode == RhinoToken.INSTANCEOF) " "
	     else "") +
	    (RhinoAST.AstNode operatorToString opcode) +
	    (if(opcode == RhinoToken.TYPEOF ||
	        opcode == RhinoToken.DELPROP ||
	        opcode == RhinoToken.VOID ||
	        opcode == RhinoToken.IN ||
	        opcode == RhinoToken.INSTANCEOF) " "
	     else "")
	
}

/** The lexical token in the tree
 *  @param what the text of this lexical token
 *  @param where the location in the source code
 *  @note all the leaf node in the AST has to be this type
 */
case class Lexical(val what:String, val where:Location) extends Node{
	val nodeType = LexicalToken
	def targetCode = what;
	lazy val targetCodeLength = what.length
	lazy val firstChar = what.head
	lazy val lastChar  = what.last
	/** map this lexical to the Rhino Token List
	 *  @param node the Rhino AstNode
	 *  @param index the index of this token the node token list
	 *  @return the newly created node that contains the location information */
	def at(node:AstNode, index:Int) = {
		val tokens = node.getTokenList
		if(index < tokens.size) {
			val l = tokens.get(index)
			new Lexical(what, InSource(l.getFileName, l.getLineno, l.getColumn))
		} else this
	}
	override def toString = "(%s `%s' %s)".format(nodeType, what, where)
}

/** The node that means Nothing */
object Nothing extends Node {
	val nodeType = Empty
	def targetCode = ""
	lazy val targetCodeLength = 0
	lazy val firstChar:Char = 0
	lazy val lastChar:Char  = 0
	override def toString = "(nothing)"
}

/** The additional metadata about the AST node */
trait MetaData;

/** The non-leaf node in the tree
 *  @param nodeType the type object of this Node
 *  @param child the child list of this Node
 *  @param meta the meta data hold by this node
 */
class Complex(val nodeType:NodeType, val child:List[Node], meta:Option[MetaData] = None) extends Node {
	lazy val targetCode = concat(child)
	lazy val targetCodeLength = concatLength(child)
	lazy val firstChar = child.head.firstChar
	lazy val lastChar  = child.last.lastChar
	override def toString = "(%s %s)".format(nodeType, child.mkString(" "))
	
	/** The additional metadata */
	val metaData:Option[MetaData] = meta
}

/** The metadata for a scope that carries the local symbol list
 *  @param locals the local variable set
 */
class ScopeMetaData(val locals:Set[String]) extends MetaData {
	override def toString = "(scope %s)" format locals
}
object ScopeMetaData {
	def apply(n:RhinoAST.Scope) = new ScopeMetaData(if(n.getSymbolTable != null) n.getSymbolTable.keySet.asScala.toSet else Set())
	def unapply(m:ScopeMetaData) = Some(m.locals)
}

/************** Shared code for Node type *********************/
/** Represents a list of expressions, known subtype is argument list and array literal */
abstract class ExpressionList extends NodeType {
	/** The first token of this list */
	val open:String
	/** The last token of this list */
	val close:String
	/** The seperator between expressions */
	val seperator:String
	/** Get the argument list from an Rhino node, for null object, return an empty List
	 *  @note an argument list is something like '(x,y,z)' it has a '()' wrapped list of node <br/>
	 *        e,g. 'List(Token(x,y,z)' */
	private def _getArguments(node:AstNode, begin:Int, list:java.util.List[AstNode]):Option[List[Node]] = {
		if(list == null) return None
		var tokenPos = begin
		val result:ListBuffer[Node] = new ListBuffer[Node]
		var first = true
		result append open.at(node, tokenPos)
		for(item <- list.asScala) {
			if(!first) {
				tokenPos = tokenPos + 1
				result append seperator.at(node, tokenPos)
			} else first = false
			result append (item:Node)
		}
		result append close.at(node, tokenPos + 1)
		Some(result toList)
	}
	/** Creates an argument list node, this node carries the list of expression
	 *  @param node  the parent ast node
	 *  @param begin the first lexical token this argument list consumed in the token list
	 *  @param list  the actual expression list
	 *  @return the created argument list node or Nothing for null list reference */
	def apply(node:AstNode, begin:Int, list:java.util.List[AstNode]):Node = _getArguments(node, begin, list) match {
		    case Some(list) => new Complex(this, list)
		    case None       => Nothing
	}
	/** whatever(args) */
	def unapply(n:Node) = _unapply[List[Node]](n, x => (0 to (x.length - 1) / 2 - 1).map(k => x(1 + k * 2)).toList)
	/** Get the number of tokens consumed by this argument list node */
	def tokenConsumed(n:Node) = (n.nodeType, n) match {
		case (_:ExpressionList, n:Complex) => 2 + (if(n.child.length == 2) 0 else (n.child.length - 1) / 2 - 1)
		case what:NodeType                  => throw new InvalidASTException("Expression List Node Expected, but got %s" format what)
	}
}

/** Represents a variable declaration list, can be for loop initializer, or decl stmt */
abstract class VarDeclList extends NodeType {
	private def _getDeclType(n:RhinoAST.VariableDeclaration) = (n isVar, n isLet, n isConst) match {
		case (true, false, false) => "var"
		case (false, true, false) => "let"
		case (false, false, true) => "const"
		case _                    => throw new InvalidASTException("Not a valid declaration type")
	}
	/** Get the declaration list
	 *  @param close the closing token of this statement */
	def getDeclList(n:RhinoAST.VariableDeclaration, close:Option[String]) = {
		val list = n.getVariables
		var tokenPos = 0
		var result:List[Node] = _getDeclType(n).at(n, tokenPos) :: Nil
		var first = true
		for(item <- list.asScala) {
			if(!first) {
				tokenPos = tokenPos + 1
				result = result :+ ",".at(n, tokenPos)
			} else first = false
			result = result :+ (item:Node)
		}
		close match {
			case Some(close) => result :+ close.at(n, tokenPos + 1)
			case None => result
		}
	}
	def apply(n:RhinoAST.VariableDeclaration):Node;
	/** whatever(decl-type, var-init-list) </br>
	 *  decl-type: may be "var", "let" or "const </br>"
	 *  var-init-list: the var list that this statement declares */
	def unapply(n:Node) = _unapply[(String, List[Node])](n, x => (x(0).targetCode, x.drop(1)))
}

/** Represents the node that defines a function */
abstract class Func extends Function {
	/** abstract value, indicates if this is a statement */
	val isStatement:Boolean
	def apply(n:RhinoAST.FunctionNode) = new Complex(this, {
		val header = "function".at(n,0) :: Nil
		val name   = if(n.getFunctionName == null) Nil else n.getFunctionName.asNode :: Nil
		val param  = Arguments(n, 1, n.getParams)
		val body   = n.getBody.asNode
		header ++ name ++ (param :: body :: (if(isStatement) ";".at(n, 1 + Arguments.tokenConsumed(param)) :: Nil else Nil))
	}, Some(ScopeMetaData(n)))
	/** Function(name, params, body) */
	def unapply(n:Node) = _unapply[(Option[Node], Node, Node)](n, x => {
		val hasName = (x.length == 5 && isStatement) || (x.length == 4 && !isStatement)
		if(hasName) (Some(x(1)), x(2), x(3))
		else        (None      , x(1), x(2))
	})
}
/************ Node Type Objects **************/

/** left.right */
object -> extends Expression {
	def apply(n:RhinoAST.PropertyGet) = new Complex(this, n.getLeft.asNode :: ".".at(n,0) :: n.getRight.asNode :: Nil)
	/** -&gt;(left, right) */
	def unapply(n:Node) = _unapply[(Node, Node)](n, x => (x(0), x(2)))
	override def toString = "get"
}
/** left:right */
object ::: extends NodeType  {
	def apply(n:RhinoAST.ObjectProperty) = new Complex(this, n.getLeft.asNode :: ":".at(n,0) :: n.getRight.asNode :: Nil)
	/** :::(left, right) */
	def unapply(n:Node) = _unapply[(Node, Node)](n, x => (x(0), x(2)))
	override def toString = ":"
}

/** The tenary operator <br/> condition?true_expr:false_expr */
object :? extends Expression {
	def apply(n:RhinoAST.ConditionalExpression) = new Complex(this, n.getTestExpression.asNode :: "?".at(n,0) ::
	                                                                n.getTrueExpression.asNode :: ":".at(n,1) ::
	                                                                n.getFalseExpression.asNode :: Nil)
	/** :?(condition, true_expr, false_expr) */
	def unapply(n:Node) = _unapply[(Node, Node, Node)](n, x => (x(0), x(2), x(4)))
	override def toString = "tenary"
}


/** Represents the argument list used in function invocation or new expression */
object Arguments extends ExpressionList {
	val open = "("
	val close = ")"
	val seperator = ","
	override def toString = "args"
}

/** Represents an array literal */
object Array extends ExpressionList with Constant {
	val open = "["
	val close = "]"
	val seperator = ","
	def apply(n:RhinoAST.ArrayLiteral):Node = this.apply(n, 0, n.getElements)
	override def toString = "array"
}

/** binary operator <br/> left op right */
object BinOp extends Expression {
	def apply(n:RhinoAST.InfixExpression) = new Complex(this, n.getLeft.asNode :: operatorToString(n.getType).at(n,0) :: n.getRight.asNode :: Nil)
	/** BinOp(left, op, right) */
	def unapply(n:Node) = _unapply[(Node, String, Node)](n, x => (x(0), x(1).targetCode, x(2)))
	override def toString = "binop"
}

/* Represents a statement block */
object Block extends Statement {
	private def _applyImpl(n:RhinoAST.AstNode): Node = new Complex(this,  ("{".at(n,0) :: (n:List[Node])) :+ "}".at(n,1) )
	def apply(n:RhinoAST.Block) = _applyImpl(n)
	def apply(n:RhinoAST.Scope) = _applyImpl(n)
	/** Block(statements) */
	def unapply(n:Node) = _unapply[List[Node]](n, x => (x))
	override def toString = "block"
}

/** break; */
object Break extends ControlFlow {
	def apply(n:RhinoAST.BreakStatement) = new Complex(this, "break".at(n, 0) :: ";".at(n,1) :: Nil)
	/** Break() */
	def unapply(n:Node) = _unapply[Unit](n, _ => ())
	override def toString = "break"
}

/** Represents a function call */
object Call extends Expression {
	def apply(n:RhinoAST.FunctionCall) = new Complex(this, n.getTarget.asNode :: Arguments(n, 0, n.getArguments) ::Nil)
	/** Call(target, params) */
	def unapply(n:Node) = _unapply[(Node, Node)](n, x => (x(0), x(1)))
	override def toString = "apply"
}

/** Represents a switch case */
object Case extends NodeType {
	def apply(n:RhinoAST.SwitchCase) = new Complex(this, if(n.getExpression != null)
	    "case".at(n,0) :: n.getExpression.asNode :: ":".at(n,1) :: n.getStatements.asScala.map(_.asNode).toList
	else
	    "default".at(n,0) :: ":".at(n,1) :: n.getStatements.asScala.map(_.asNode).toList
	)
	def unapply(n:Node) = _unapply[(Option[Node], List[Node])](n, x => {
		val isDefault = x(0).targetCode == "default"
		(if(isDefault) None else Some(x(1)), x.drop(if(isDefault) 2 else 3))
	})
	override def toString = "case"
}

object Catch extends NodeType {
	def apply(n:RhinoAST.CatchClause) = new Complex(this, "catch".at(n,0) :: "(".at(n,1) ::
	                                                      n.getVarName.asNode :: (
	                                                          if(n.getCatchCondition == null) ")".at(n,2) :: n.getBody.asNode :: Nil
	                                                          else "if".at(n,2) :: n.getCatchCondition.asNode :: ")".at(n,3) :: n.getBody.asNode :: Nil))
	def unapply(n:Node) = _unapply[(Node, Option[Node], Node)](n, x => if(x.length == 4) (x(2), None, x(4)) else (x(2), Some(x(4)), x(6)))
	override def toString = "catch"
}

object Continue extends ControlFlow {
	def apply(n:RhinoAST.ContinueStatement) = new Complex(this, "continue".at(n, 0) :: (if(n.getLabel == null)  ";" .at(n,1) :: Nil else n.getLabel.asNode :: ";".at(n,1) :: Nil))
	def unapply(n:Node) = _unapply[Option[String]](n, x => if(x.length == 1) None else Some(x(1).targetCode))
	override def toString = "continue"
}

object Decl extends NodeType {
	def apply(n:RhinoAST.VariableInitializer) = new Complex(this, n.getTarget.asNode ::
	                                                              (if(n.getInitializer == null) Nil else "=".at(n,0) :: n.getInitializer.asNode :: Nil))
	def unapply(n:Node) = _unapply[(Node, Option[Node])](n, x => (x(0), if(x.length == 1) None else Some(x(2))))
	override def toString = "var-init"
}

object DeclList extends VarDeclList with Statement {
	def apply(n:RhinoAST.VariableDeclaration):Node = new Complex(this, getDeclList(n, Some(";")))
	override def toString = "decl-stmt"
}

object DeclForLoopInit extends VarDeclList {
	def apply(n:RhinoAST.VariableDeclaration):Node = new Complex(this, getDeclList(n, None))
	override def toString = "var-for-init"
}

object Debugger extends Statement {
	def apply(n:RhinoAST.KeywordLiteral) = new Complex(this, "debugger".at(n, 0) :: ";".at(n,1) :: Nil)
	def unapply(n:Node) = _unapply[Unit](n, _ => ())
	override def toString = "debugger"
}

object DoWhile extends Loop {
	def apply(n:RhinoAST.DoLoop) = new Complex(this, "do".at(n,0) :: n.getBody.asNode :: "while".at(n,1) :: "(".at(n,2) :: n.getCondition.asNode :: ")".at(n,3) :: Nil)
	def unapply(n:Node) = _unapply[(Node, Node)](n, x => (x(1), x(4)))
	override def toString = "do-while"
}

object EmptyExpr extends Expression {
	def apply(n:RhinoAST.EmptyExpression) = new Complex(this, Nil)
	def unapply(n:Node):Option[Unit] = _unapply[Unit](n, _ => ())
	override def toString = "empty-expr"
}

object EmptyStmt extends Statement {
	def apply(n:RhinoAST.EmptyStatement) = new Complex(this, ";".at(n,0) :: Nil)
	def unapply(n:Node):Option[Unit] = _unapply[Unit](n, _ => ())
	override def toString = "empty-stmt"
}

object ExprStmt extends Expression {
	def apply(n:RhinoAST.ExpressionStatement) = new Complex(this, n.getExpression.asNode :: ";".at(n, 0) :: Nil)
	def unapply(n:Node) = _unapply[Node](n, (_(0)))
	override def toString = "expr-stmt"
}

object False extends Constant {
	def apply(n:RhinoAST.KeywordLiteral) = new Complex(this, "false".at(n, 0) :: Nil)
	def unapply(n:Node) = _unapply[Unit](n, _ => ())
	override def toString = "false"
}

object For extends ForLoop {
	def apply(n:RhinoAST.ForLoop) = new Complex(this, "for".at(n,0) :: "(".at(n,1) ::
	                                                  n.getInitializer.asNode :: ";".at(n,2) ::
	                                                  n.getCondition.asNode :: ";".at(n,3) ::
	                                                  n.getIncrement.asNode :: ")".at(n,4) ::
	                                                  n.getBody.asNode :: Nil)
	def unapply(n:Node) = _unapply[(Node, Node, Node, Node)](n, x => (x(2), x(4), x(6), x(8)))
	override def toString = "for"
}

object ForEach extends ForLoop {
	def apply(n:RhinoAST.ForInLoop) = new Complex(this, "for".at(n,0) :: "each".at(n,1) :: "(".at(n,2) ::
	                                                    n.getIterator.asNode :: "in".at(n,3) :: n.getIteratedObject.asNode :: ")".at(n,4) ::
	                                                    n.getBody.asNode :: Nil)
	def unapply(n:Node) = _unapply[(Node, Node, Node)](n, x => (x(3), x(5), x(7)))
	override def toString = "for-each"
}

object ForIn extends ForLoop {
	def apply(n:RhinoAST.ForInLoop) = new Complex(this, "for".at(n,0) :: "(".at(n,1) ::
	                                                    n.getIterator.asNode :: "in".at(n,2) :: n.getIteratedObject.asNode :: ")".at(n,3)::
	                                                    n.getBody.asNode :: Nil)
	def unapply(n:Node) = _unapply[(Node, Node, Node)](n, x=>(x(2), x(4), x(6)))
	override def toString = "for-in"
}

object FuncExpr extends Func with Expression {
	val isStatement = false
	override def toString = "func-expr"
}

object FuncStmt extends Func with Statement {
	val isStatement = true
	override def toString = "func"
}

object Id extends Expression {
	def apply(n:RhinoAST.Name) = new Complex(this, n.getString.at(n,0) :: Nil)
	def unapply(n:Node) = _unapply[Node](n, (_(0)))
	override def toString = "id"
}

object If extends ControlFlow {
	def apply(n:RhinoAST.IfStatement) = new Complex(this, "if".at(n, 0) :: "(".at(n,1) :: n.getCondition.asNode :: ")".at(n,2) :: n.getThenPart.asNode ::
	                                                       (if(n.getElsePart != null) "else".at(n,3) :: n.getElsePart.asNode :: Nil else Nil))
	def unapply(n:Node) = _unapply[(Node, Node, Option[Node])](n, x => (x(2), x(4), if(x.length > 5) Some(x(6)) else None))
	override def toString = "if"
}

object Index extends Expression {
	def apply(n:RhinoAST.ElementGet) = new Complex(this, n.getTarget.asNode :: "[".at(n,0) :: n.getElement.asNode :: "]".at(n,1) :: Nil)
	def unapply(n:Node) = _unapply[(Node, Node)](n, x => (x(0), x(2)))
	override def toString = "idx"
}

object Json extends ExpressionList with Constant {
	val open = "{"
	val close = "}"
	val seperator = ","
	def apply(n:RhinoAST.ObjectLiteral):Node = apply(n, 0, n.getElements.asInstanceOf[java.util.List[RhinoAST.AstNode]])
	override def toString = "json"
}
object New extends Expression {
	def apply(n:RhinoAST.NewExpression) = new Complex(this, "new".at(n,0) :: n.getTarget.asNode :: Arguments(n, 1, n.getArguments) :: Nil )
	def unapply(n:Node) = _unapply[(Node, Node)](n, n => (n(1), n(2)))
	override def toString = "new"
}

object Null extends Constant {
	def apply(n:RhinoAST.KeywordLiteral) = new Complex(this, "null".at(n, 0) :: Nil)
	def unapply(n:Node) = _unapply[Unit](n, _ => ())
	override def toString = "null"
}

object Num extends Constant {
	def apply(n:RhinoAST.NumberLiteral) = new Complex(this, n.getValue.at(n,0) :: Nil)
	def unapply(n:Node) = _unapply[String](n, n => n(0).targetCode)
	override def toString = "num"
}

object PE extends Expression {
	def apply(n:RhinoAST.ParenthesizedExpression) = new Complex(this, "(".at(n,0) :: n.getExpression.asNode :: ")".at(n,1) :: Nil)
	def unapply(n:Node) = _unapply[Node](n, (_(1)))
	override def toString = "parentheses"
}

object Postfix extends Expression {
	def apply(n:RhinoAST.UnaryExpression) = new Complex(this, n.getOperand.asNode :: operatorToString(n.getType).at(n,0) :: Nil)
	def unapply(n:Node) = _unapply[(Node, String)](n, x => (x(0), x(1).targetCode))
	override def toString = "postifx-op"
}

object Prefix extends Expression {
	def apply(n:RhinoAST.UnaryExpression) = new Complex(this, operatorToString(n.getType).at(n, 0) :: n.getOperand.asNode :: Nil)
	def unapply(n:Node) = _unapply[(String, Node)](n, x => (x(0).targetCode, x(1)))
	override def toString = "prefix-op"
}

object Program extends NodeType {
	def apply(n:RhinoAST.AstRoot) = new Complex(this, n)
	def unapply(n:Node) = _unapply[(List[Node])](n, x => x)
	override def toString = "program"
}

object Reg extends Constant {
	def apply(n:RhinoAST.RegExpLiteral) = new Complex(this, ("/" + n.getValue + "/" + (if(n.getFlags == null) "" else n.getFlags)).at(n, 0) :: Nil)
	def unapply(n:Node) = _unapply[String](n, (_(0).targetCode))
	override def toString = "regex"
}

object Return extends ControlFlow {
	def apply(n:RhinoAST.ReturnStatement) = new Complex(this, "return".at(n,0) :: (
	                                                          if(n.getReturnValue == null) ";" .at(n,1) :: Nil
	                                                          else n.getReturnValue.asNode :: ";".at(n,1) :: Nil))
	def unapply(n:Node) = _unapply[Option[Node]](n, x => if(x.length == 2) None else Some(x(1)))
	override def toString = "return"
}

object Str extends Constant {
	def apply(n:RhinoAST.StringLiteral) = {
		val quote = n.getQuoteCharacter
		new Complex(this, (quote + escapeString(n.getValue, quote) + quote).at(n,0)  :: Nil)
	}
	def unapply(n:Node) = _unapply[String](n, (_(1).targetCode))
	override def toString = "str"
}

object Switch extends ControlFlow {
	def apply(n:RhinoAST.SwitchStatement) = new Complex(this, ("switch".at(n,0) :: "(".at(n,1) :: n.getExpression.asNode :: ")".at(n,2) ::
	                                                           "{".at(n,3) :: n.getCases.asScala.map(_.asNode).toList) :+ "}".at(n,4))
	def unapply(n:Node) = _unapply[(Node, List[Node])](n, x => (x(2), x.slice(5, x.length - 1)))
	override def toString = "switch"
}

object This extends Expression {
	def apply(n:RhinoAST.KeywordLiteral) = new Complex(this, "this".at(n, 0) :: Nil)
	def unapply(n:Node) = _unapply[Unit](n, _ => ())
	override def toString = "this"
}

case object Throw extends ControlFlow {
	def apply(n:RhinoAST.ThrowStatement) = new Complex(this, "throw".at(n,0) :: n.getExpression.asNode :: ";".at(n,1) :: Nil)
	def unapply(n:Node) = _unapply[Node](n, (_(1)))
	override def toString = "throw"
}

object True extends Constant {
	def apply(n:RhinoAST.KeywordLiteral) = new Complex(this, "true".at(n, 0) :: Nil)
	def unapply(n:Node) = _unapply[Unit](n, _ => ())
	override def toString = "true"
}

object Try extends Statement {
	def apply(n:RhinoAST.TryStatement) = new Complex(this, ("try".at(n,0) :: n.getTryBlock.asNode :: Nil) ++
	                                                        n.getCatchClauses.asScala.map(_.asNode) ++ (
	                                                            if(n.getFinallyBlock == null) Nil
	                                                            else "finally".at(n,1) :: n.getFinallyBlock.asNode :: Nil))
	def unapply(n:Node) = _unapply[(Node, List[Node], Option[Node])](n, x => {
		val len = x.length
		val hasFinally = x(len - 2).targetCode == "finally"
		val tryBlock = x(1)
		val finallyBlock = if(hasFinally) Some(x(len - 1)) else None
		val catchBlock = x.slice(2, len - (if(hasFinally) 2 else 0))
		(tryBlock, catchBlock, finallyBlock)
	})
	override def toString = "try"
}

object While extends Loop {
	def apply(n:RhinoAST.WhileLoop) = new Complex(this, "while".at(n,0) :: "(".at(n,1) :: n.getCondition.asNode :: ")".at(n,2) :: n.getBody.asNode :: Nil)
	def unapply(n:Node) = _unapply[(Node, Node)](n, x => (x(2), x(4)))
	override def toString = "while"
}

/******************* Helper Functions ************************/
object AST {
	import java.io.FileReader
	import org.mozilla.javascript.{Parser, CompilerEnvirons}
	val codeEnv = new CompilerEnvirons;
	val tempEnv = new CompilerEnvirons;
	tempEnv.setCodeGeneratorMode(false)
	def parseFromString(code:String):Node = (new Parser(tempEnv)).parse(code, "<template>", 0)
	def parseFromSource(path:String):Node = (new Parser(codeEnv)).parse(new FileReader(path), path, 0)
}

package com.github._38.radiation.ast;

trait NodeType {
	def _unapply[T](n:Node, how:List[Node] => T):Option[T] = if(n.nodeType == this) Some(how(n.asInstanceOf[Complex].child)) else None
}
trait Expression    extends NodeType
trait Statement     extends NodeType
trait ForLoopInit   extends NodeType
trait Scope         extends NodeType
trait LocalScope    extends Scope
trait GlobalScope   extends Scope
trait Function      extends NodeType
trait ControlFlow   extends Statement
trait Loop          extends ControlFlow
trait ForLoop       extends Loop
trait Constant      extends Expression
object Empty         extends NodeType {
	override def toString = "nothing"
}
object LexicalToken extends NodeType {
	override def toString = "lex"
}

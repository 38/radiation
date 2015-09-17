package com.github._38.radiation.ast

import scala.collection.mutable.ListBuffer

/** This class is used to construct a node when doing code transformation
 *  The original node should be given, and the node creation is only happen
 *  When the newly built node different to the orignal one */
class NodeBuilder(original:Complex) {
	private val _buffer = ListBuffer[Node]()
	private var _current:Option[List[Node]] = Some(original.child)
	private var _type = original.nodeType
	def setType(nodeType:NodeType) {
		if(!(nodeType eq original.nodeType)) _current = None
		_type = nodeType
	}
	def append(child:Node) {
		_current = _current match {
			case None       =>  None
			case Some(what) =>  if(what.head eq child) Some(what.tail) else None
		}
		_buffer append child
	}
	def ++= (that:List[Node]) {
		that.foreach(this.append)
	}
	def toNode = _current match {
		case Some(Nil)  =>  original
		case _          =>  new Complex(_type, _buffer.toList)
	}
}
object NodeBuilder {
	def apply(original:Complex) = new NodeBuilder(original)
}

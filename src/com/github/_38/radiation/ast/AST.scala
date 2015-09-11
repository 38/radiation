package com.github._38.radiation.ast

import org.mozilla.javascript.ast._
import com.github._38.radiation.source.Location

/** The base class of a node */
trait Node {
    def targetCode:String
    def targetCodeLength:Int
    def last:Char
    def first:Char
}
object Node {
    private val _idEndings = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq('_', '$')).toSet
    private def _whiteSpace(last:Option[Char], next:Char) = prelast match {
        case Some(prelast) => if((_idEndings contains last) && (_idEndings contains next)) " " else ""
        case None          => ""
    }
    def combine(what:List[Node]) = {
        var result = ""
        var last:Option[Char] = None
        for(str <- what map (_.targetCode)) 
            if(str != "") {
                result = result + _whiteSpace(last, str(0)) + str
                last = Some(str.last)
            }
        result
    }
    def combineLength(what:List[Node]) = {
        var result = 0
        var last:Option[Char] = None
        for(node <- what) 
            if(node.targetCodeLength > 0) {
                result = result + _whiteSpace(last, node.first).length + node.targetCodeLength
                last = Some(node.last)
            }
    }
}

/** The lexical token in the tree */
class Lex(val what:String, val where:Location) extends Node{
    def targetCode = what;
    def targetCodeLength = what.length
    def first = what.head
    def last  = what.last
}

/** The non-leaf node in the tree */
trait NonLeaf extends Node {
    val child:List[Node];
    def targetCode = Node combine child
    def targetCodeLength = Node combineLength child
}

trait Expression    extends Node
trait Statement     extends Node
trait ForLoopInit   extends Node
trait Scope         extends Node
trait LocalScope    extends Scope
trait GlobalScope   extends Scope
trait Function      extends Node
trait ControlFlow   extends Statement
trait Loop          extends ControlFlow
trait ForLoop       extends Loop
trait Constant      extends Expression

class Break(val where:Location) extends ControlFlow {
}

import org.mozilla.javascript.{ast => RhinoAST, Parser}
import scala.math.max
import scala.language.postfixOps
import scala.language.implicitConversions

package com.github._38.radiation.ast {
    /** Describe a Location in a source code */
    case class SourceLocation(val line:Int, val column:Int);
    /** The base class for all AST Nodes */
    abstract class Node(location:SourceLocation) {
        def targetCode:String;
        val length:Int;
        val sourceLocation:SourceLocation = location;
    }
    object Node {
        def mkList(fmt:String, sep:String, values:List[Node]) =
            fmt format (values map (_ targetCode) mkString sep)
        def listLength(wrap:Int, sep:Int, values:List[Node]) = 
            wrap + (values map (_ length) sum) + (values length) * sep - sep
    }
    trait Statement extends Node;
    trait Expression extends Statement;
    trait Scope extends Node;
    
    case class Num(value:String, location:SourceLocation) extends Node(location) with Expression {
        def targetCode = value
        lazy val length = value.length
    }
    case class Lst(children:List[Node], location:SourceLocation) extends Node(location) with Expression {
        def targetCode = Node.mkList("[%s]", ",", children)
        lazy val length = Node.listLength(2, 1, children)
    }
    case class Block(statements:List[Statement], location:SourceLocation) extends Node(location) with Statement {
        def targetCode = Node.mkList("{%s}", "", statements)
        lazy val length = Node.listLength(2, 0, statements)
    }
    case class Catch(val cond:Node, val body:Block) extends Node(location) {
        def targetCode = "try(%s)%s".format(cond targetCode, body targetCode)
        lazy val length = 5 + (cond length) + (body length)
    }
    
}


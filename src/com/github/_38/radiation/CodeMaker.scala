import com.github._38.radiation.ast._
import scala.language.postfixOps
import scala.language.implicitConversions

package com.github._38.radiation.CodeMaker {
    object Conversions {
        implicit def stringToPattern(s:String) = new PlainText(s)
        implicit def nodeToPattern(n:Node) = new SignleNode(n)
        implicit def primitiveToCompound(p:Primitive) = new Compound(List(p))
        def mkList(l:List[Node], s:String = "") = new NodeList(l, s)
    }
  
    case class CodeInfo(node: Node, text:String, offset:Int){  /* offset against the begning of this pattern */
        def +(base:Int) = CodeInfo(node, text, offset + base)
    }
    
    
    trait CodeGeneratePattern {
        import Conversions._
        def render:String
        def info:List[CodeInfo] = List()
        def length = render length
        def asCompound = this match {
            case c:Compound => c
            case p:Primitive => new Compound(List(p))
        }
        def -- (that: CodeGeneratePattern):Compound = (this,that) match {
            case (l:Empty, r) => r asCompound
            case (l, r:Empty) => l asCompound
            case (l:Compound,r:Compound) => new Compound((l values) ++ (r values))
            case (l:Compound,r:Primitive) => new Compound((l values) :+ r)
            case (l:Primitive, r) => (l:Compound) -- r
        }
    }
    
    trait Primitive extends CodeGeneratePattern;
    
    trait ImportantPrimitive extends CodeGeneratePattern with Primitive;
    
    class Empty extends CodeGeneratePattern {
        def render = ""
    }
    object Empty {
        val singleton = new Empty;
        def apply() = singleton;
    }
    class PlainText(text:String) extends CodeGeneratePattern with Primitive {
        def render = text;
    }
    class NodeList(nodes:List[Node], seperator:String) extends CodeGeneratePattern with ImportantPrimitive {
        def render = nodes map (_ targetCode) mkString seperator
        def _scan(todo:List[Node], offset:Int):List[CodeInfo] = todo match {
            case x :: xs => CodeInfo(x, x targetCode, offset) :: _scan(xs, (x length) + offset + (seperator length))
            case _ => List()
        }
        override def info = _scan(nodes, 0)
    }
    class SignleNode(node:Node) extends CodeGeneratePattern with ImportantPrimitive {
        def render = node targetCode
        override def info = List(CodeInfo(node, node targetCode, 0))
    }
    class Compound(val values:List[Primitive]) extends CodeGeneratePattern {
        def render = values map (_ render) mkString
        def _scan(xs:List[Primitive], base:Int):List[CodeInfo] = xs match {
            case x :: xs => x.info.map(_ + base) ++ _scan(xs, base + x.length)
            case _ => List()
        }
        override def info = _scan(values, 0)
    }
}

// vim: set ts=4 sw=4 et:

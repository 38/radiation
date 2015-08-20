import com.github._38.radiation.ast._
import scala.language.postfixOps
import scala.language.implicitConversions

package com.github._38.radiation.pattern {
    object Conversions {
        implicit def stringToPattern(s:String) = new PlainText(s)
        implicit def nodeToPattern(n:Node) = new SignleNode(n)
        implicit def primitiveToCompound(p:Primitive) = new Compound(List(p))
        def mkList(l:List[Node], s:String = "") = new NodeList(l, s)
    }
  
    case class Info(node: Node, offset:Int){  /* offset against the begning of this pattern */
        def +(base:Int) = Info(node, offset + base)
    }
    
    
    trait Pattern{
        import Conversions._
        def render:String
        def info:List[Info] = List()
        def length = render length
        def asCompound = this match {
            case c:Compound => c
            case e:Empty => new Compound(List())
            case p:Primitive => new Compound(List(p))
        }
        def -- (that: Pattern):Compound = (this,that) match {
            case (l:Empty, r) => r asCompound
            case (l, r:Empty) => l asCompound
            case (l:Compound,r:Compound) => new Compound((l values) ++ (r values))
            case (l:Compound,r:Primitive) => new Compound((l values) ++ (r emptify))
            case (l:Primitive, r) => (l:Compound) -- r
        }
    }
    
    trait Primitive extends Pattern{
        def emptify:List[Primitive];
    }
    
    trait Edge extends Primitive;
    
    class Empty extends Pattern {
        def render = ""
    }
    object Empty {
        val singleton = new Empty;
        def apply() = singleton;
    }
    class PlainText(text:String) extends Primitive {
        def render = text;
        def emptify = if(text == null) List() else List(this) 
    }
    class NodeList(nodes:List[Node], seperator:String) extends Edge {
        def render = nodes map (_ targetCode) mkString seperator
        def _scan(todo:List[Node], offset:Int):List[Info] = todo match {
            case x :: xs => Info(x, offset) :: _scan(xs, (x length) + offset + (seperator length))
            case _ => List()
        }
        override def info = _scan(nodes, 0)
        def emptify = if(nodes == null) List() else List(this) 
    }
    class SignleNode(node:Node) extends Edge {
        def render = node targetCode
        override def info = List(Info(node, 0))
        def emptify = if(node == null) List() else List(this)
    }
    class Compound(val values:List[Primitive]) extends Pattern {
        def render = values map (_ render) mkString
        def _scan(xs:List[Primitive], base:Int):List[Info] = xs match {
            case x :: xs => x.info.map(_ + base) ++ _scan(xs, base + x.length)
            case _ => List()
        }
        override def info = _scan(values, 0)
    }
}

// vim: set ts=4 sw=4 et:

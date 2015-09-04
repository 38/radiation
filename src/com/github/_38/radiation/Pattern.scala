import com.github._38.radiation.ast._
import scala.language.postfixOps
import scala.language.implicitConversions

package com.github._38.radiation.pattern {
	/** The conversion helpers */
	object Conversions {
		/** Convert a string to pattern */
		implicit def stringToPattern(s:String) = new PlainText(s)
		/** Convert a signle node to pattern */
		implicit def nodeToPattern(n:Node) = new SignleNode(n)
		/** Convert primitive pattern to compound pattern */
		implicit def primitiveToCompound(p:Primitive) = new Compound(List(p))
		/** Convert list to the pattern list
		 *  @param  l the list to convert
		 *  @param  s the seperator between two nodes
		 *  @return the pattern
		 **/
		def mkList(l:List[Node], s:String = "") = new NodeList(l, s)
		
	}
	
	/** Carries the code info
	 *  @param  node
	 *  @param  offset the offset in the target code
	 **/
	case class Info(val node: Node, val offset:Int){  /* offset against the begning of this pattern */
		def +(base:Int) = Info(node, offset + base)
	}
	
	/** The base interface for a code pattern */
	trait Pattern{
		import Conversions._
		/** Get the target code */
		def render:String
		/** Get the code info list, only contains the node pattern mentioned, a.k.a direct children */
		def info:List[Info] = List()
		/** The length of the target code */
		def length = render length
		/** Convert this pattern to compund */
		def asCompound = this match {
			case c:Compound => c
			case e:Empty => new Compound(List())
			case p:Primitive => new Compound(List(p))
		}
		/** Concat with another pattern */
		def -- (that: Pattern):Compound = (this,that) match {
			case (l:Empty, r) => r asCompound
			case (l, r:Empty) => l asCompound
			case (l:Compound,r:Compound) => new Compound((l values) ++ (r values))
			case (l:Compound,r:Primitive) => new Compound((l values) ++ (r listify))
			case (l:Primitive, r) => (l:Compound) -- r
		}
	}
	/** Pattern related utils */
	object Pattern {
		/** We need add a white space between two identifers */
		val idEndings = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq('_', '$')).toSet
		/** Combine a list of string
		 *  @param strs the strings to combine
		 *  @param sep  the seperator
		 */
		def combine(strs:List[String], sep:String) = {
			var result = ""
			var first = true
			for(str <- strs) if(str != "") {
				if(!first) result = result + sep
				result = result + (if(!first && (idEndings contains result.last) && (idEndings contains str.head)) " " + str else str)
				first = false
			}
			result
		}
	}
	/** The pattern that is not Compound */
	trait Primitive extends Pattern{
		/** Make a Primitive a List of one node or List of nothing */
		def listify:List[Primitive];
	}
	/** This is actually an edge in the AST */
	trait Edge extends Primitive;
	
	/** Bascially Nothing here */
	class Empty extends Pattern {
		def render = ""
	}
	object Empty {
		val singleton = new Empty;
		def apply() = singleton;
	}
	/** Plain text in the code */
	class PlainText(text:String) extends Primitive {
		def render = text;
		def listify = if(text == null) List() else List(this)
	}
	/** Represents a list of node */
	class NodeList(nodes:List[Node], seperator:String) extends Edge {
		def render = Pattern.combine(nodes map (_ targetCode), seperator)
		def _scan(todo:List[Node], offset:Int):List[Info] = todo match {
			case x :: xs => Info(x, offset) :: _scan(xs, (x length) + offset + (seperator length))
			case _ => List()
		}
		override def info = _scan(nodes, 0)
		def listify = if(nodes == null) List() else List(this)
	}
	/** Represents an AST node */
	class SignleNode(node:Node) extends Edge {
		def render = node targetCode
		override def info = List(Info(node, 0))
		def listify = if(node == null) List() else List(this)
	}
	/** Represents the concats of patterns */
	class Compound(val values:List[Primitive]) extends Pattern {
		def render = Pattern.combine(values map (_ render), "")
		def _scan(xs:List[Primitive], base:Int):List[Info] = xs match {
			case x :: xs => x.info.map(_ + base) ++ _scan(xs, base + x.length)
			case _ => List()
		}
		override def info = _scan(values, 0)
	}
}

// vim: set ts=4 sw=4 et:

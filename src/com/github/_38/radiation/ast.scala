import org.mozilla.javascript.{ast => RhinoAST, _}
import scala.math.max
import scala.language.postfixOps
import scala.language.implicitConversions

package com.github._38.radiation.ast{
    class CodePosition(val line:Int, val column:Int){
        def +(that:CodePosition) = new CodePosition(this.line + that.line, max(this.column, that.column))  /* merge the code position change */
        def >> = new CodePosition(line, column + 4);  /* increase the indentation */
        def << = new CodePosition(line, column - 4);  /* decrease the indentation */
    }
    object CodePosition {
        val None = new CodePosition(0,0)
        implicit def intToCodePos(i:Int) = new CodePosition(0, i)
    }
    class IndentedCode(val text:String, val indent:Int = 0) {
        lazy val str = " " * 4 * indent + text     /* the source code string */
        lazy val length = indent * 4 + text.length /* the length of the source code */
        override def toString = str
        def >> = new IndentedCode(text, indent + 1) /* increase the indentation */
        def << = new IndentedCode(text, indent - 1) /* decrease the indentation */
        def ++ (that:IndentedCode) = new IndentedCode(this.text + that.text, this.indent)   /* Concat two code piece */
        def ++ (that:String) = new IndentedCode(this.text + that, this.indent)   /* Concat code piece with string */
    }
    abstract class Node(val child:List[Node]) {
        def <(n:Node) = new Consturctor.ExpectMore(this :+ n);
        def > = new Consturctor.NoMore(this);
        def :+(n:Node):Node = n;             /* Append the node to child list, by default do nothing, so no need override for primitives */
        val source:List[IndentedCode];  /* The source code of this piece of code, for expressions, just have one element */
        def positionGain:CodePosition = (CodePosition.None /: (source map (_ length)))(_+_)
        def getSource(indent:Int) = (source map ("\t" * indent + _)) mkString "\n"
    }
    case class  Number_(val value:String) extends Node(List()) {
        override lazy val source = List(new IndentedCode(value));
    }
    case class  List_(val values:List[Node] = List()) extends Node(values) {
        override def :+(n:Node) = new List_(values :+ n)
        override lazy val source = if(values exists (_.source.length > 1))
            /*(List(new IndentedCode("[")) /: (values map (v => v.source map (s => s >>))))(_++_) :+ new IndentedCode("]")*/ List() /*TODO */
        else
            List((new IndentedCode("[" + values.head.source.head) /: (values.tail map ((_ source 0))))(_++", "++_) ++ "]")
    }
    object Consturctor{
        case class ExpectMore(val node:Node){
            def >          = node                       /* For the case Node < child > */
            def | (n:Node) = new ExpectMore(node :+ n)  /* For the case Node < child1 | child2 */
            def | (last:NoMore) = node :+ last.node 
        }
        case class NoMore(val node:Node);
        implicit def nodeToStream(node:Node) = node child
        def main(arg:Array[String]) 
        {
            System.out.println((
                List_()<
                    Number_("123")|
                    Number_("234")|
                    (List_() <
                        Number_("1")
                     >) |
                    Number_("xxx")
                  >
                ).getSource(1))
        }
    }
}


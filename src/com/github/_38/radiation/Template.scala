import com.github._38.radiation.ast;
import scala.language.implicitConversions;

package com.github._38.radiation.template {
	class TemplateRenderingException(message:String) extends Exception {
		override def toString = "Can't render the template : " + message;
	}
	class InvalidTemplateException(message:String) extends Exception {
		override def toString = "Invalid template : " + message
	}
	class Template(tempStr:String, isExpr:Boolean = false) {
		import ast.ASTParser._
		import ast._
		def t = this
		def e = new Template(tempStr, true)
		def js = render()
		val templateAST = fromString(tempStr) match {
			case Program(ExpressionStat(what) :: _, _) if isExpr => List(what)
			case Program(what, _) =>  what
			case _             => throw new InvalidTemplateException("Malformed template")
		}
		val _isRef = "\\$\\$[0-9]+\\$\\$".r
		def _renderOne(template:Node, args:Node*):Node = {
			def _renderTrans(node:Node):Node = node match {
				case node:VirtualNode => Nop
				case ExpressionStat(Id(arg_ref)) if _isRef.pattern.matcher(arg_ref).matches => {   /* For the case $$1$$; */
					val ref = arg_ref.replace("$","").toInt
					if(args.length <= ref) throw new TemplateRenderingException("Invalid argument reference " + ref)
					else args(ref)
				}
				case Id(arg_ref) if _isRef.pattern.matcher(arg_ref).matches => {    /* For the case x = $$1$$ + 2; */
					val ref = arg_ref.replace("$", "").toInt
					if(args.length <= ref) throw new TemplateRenderingException("Invalid argument reference " + ref)
					else args(ref)
				}
				case _ =>  node
			}
			template traverse _renderTrans
		}
		def render(args:Node*) =  templateAST map (what => _renderOne(what, args:_*))
	}
	object Template {
		import ast.ASTParser._
		import ast.{Node, Program}
		implicit def stringToTemplate(s:String) = new Template(s)
	}
}

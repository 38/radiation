package com.github._38.radiation.template;

import com.github._38.radiation.ast._
import scala.language.implicitConversions

class TemplateRenderingException(message:String) extends Exception {
	override def toString = "Can't render the template : " + message;
}

class InvalidTemplateException(message:String) extends Exception {
	override def toString = "Invalid template : " + message
}

class TemplateHelper(str:String) {
	def t = new StatementTemplate(str)
	def e = new ExpressionTemplate(str)
	def js = {
		val Program(what) = AST parseFromString str
		what
	}
}

abstract class Template {
	val templateAST:List[Node]
	val _isRef = "\\$\\$[0-9]+\\$\\$".r
	def _renderOne(template:Node, args:Node*):Node = template match {
		case ExprStmt(Id(Lexical(name, _))) if _isRef.pattern.matcher(name).matches => {
			val ref = name.replace("$", "").toInt
			if(args.length <= ref) throw new TemplateRenderingException("Invalid argument reference " + ref)
			else args(ref)
		}
		case Id(Lexical(name, _)) if _isRef.pattern.matcher(name).matches => {
			val ref = name.replace("$", "").toInt
			if(args.length <= ref) throw new TemplateRenderingException("Invalid argument reference " + ref)
			else args(ref)
		}
		case complex: Complex => {
			val nb = NodeBuilder(complex)
			complex.child.foreach(child => nb.append(_renderOne(child, args:_*)))
			nb.toNode
		}
		case whatever:Node    => whatever
	}
}

class StatementTemplate(tempStr:String) extends Template {
	val templateAST = AST parseFromString tempStr match {
		case Program(what) =>  what
		case _             => throw new InvalidTemplateException("Malformed template")
	}
	def render(args:Node*) =  templateAST.map(what => _renderOne(what, args:_*))
}
class ExpressionTemplate(tempStr:String) extends Template {
	val templateAST = AST.parseFromString("(" + tempStr + ")") match {
		case Program(ExprStmt(PE(what)) :: _) => List(what)
		case _                                      => throw new InvalidTemplateException("Malformed template")
	}
	def render(args:Node*) = _renderOne(templateAST(0), args:_*)
}
object Template {
	implicit def stringToTemplate(s:String) = new TemplateHelper(s)
}

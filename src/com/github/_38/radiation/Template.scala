import com.github._38.radiation.ast;

package com.github._38.radiation.template {
	class TemplateRenderingException(message:String) extends Exception {
		override def toString = "Can't render the template " + message;
	}
	class Template(tempStr:String) {
		import ast.ASTParser._
		import ast.{Node, $}
		val templateAST = fromString(tempStr)
		val _isRef = "$$[0-9]+$$".r
		def _render(root:Node, args:Seq[Node]):Node = root match {
			case $(arg_ref) if _isRef.pattern.matcher(arg_ref).matches => {
				val ref = arg_ref.replace("$","").toInt
				if(args.length <= ref) throw new TemplateRenderingException("Invalid argument reference " + ref)
				else args(ref);
			}
			/* TODO: We need stream style to void case matches */
		}
	}
}

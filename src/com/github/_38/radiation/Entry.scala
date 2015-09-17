package com.github._38.radiation

import com.github._38.radiation.ast._
import com.github._38.radiation.sourcemap._
import com.github._38.radiation.template.Template._
import Base64IntConverters._
import Helper.fromString
object Main {
	def main(args:Array[String]) {
		/*val ast = AST.parseFromSource("../jsdb/test/jquery.js")
		//System.out.println(ast)
		System.out.println(ast.targetCode.length)
		System.out.println(SourceMap.fromAST(ast, "test.min.js").dump.length)*/
	   val template = """
function $$0$$() {
	return 0;
}
""".t
		val name = new Complex(Id, ("test":Lexical) :: Nil)
		System.out.println(template.render(name).head.targetCode);
	}
}

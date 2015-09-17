package com.github._38.radiation

import com.github._38.radiation.ast._
import com.github._38.radiation.sourcemap._
import Base64IntConverters._
object Main {
	def main(args:Array[String]) {
		val ast = AST.parseFromSource("../jsdb/test/jquery.js")
		System.out.println(ast)
		System.out.println(ast.targetCode)
		System.out.println(SourceMap.fromAST(ast, "test.min.js").dump)
	}
}

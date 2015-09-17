package com.github._38.radiation

object Main {
	def main(args:Array[String]) {
		val ast = com.github._38.radiation.ast.AST.parseFromSource("../test.js")
		val map = com.github._38.radiation.sourcemap.SourceMap.fromAST(ast, "test.min.js")
		System.out.println(ast)
		System.out.println(ast.targetCode)
		System.out.println(map.dump)
	}
}

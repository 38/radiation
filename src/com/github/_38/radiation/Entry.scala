package com.github._38.radiation

object Main {
	def main(args:Array[String]) {
		val ast = com.github._38.radiation.ast.AST.parseFromSource("/tmp/test.js")
		val ret = com.github._38.radiation.module.Closure.run(ast)
		val map = com.github._38.radiation.sourcemap.SourceMap.fromAST(ret, "test.min.js")
		System.out.println(ret)
		System.out.println(ret.targetCode)
		System.out.println(map.dump)
	}
}

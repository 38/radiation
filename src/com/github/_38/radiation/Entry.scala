package com.github._38.radiation

object Main {
	def main(args:Array[String]) {
		val ast = com.github._38.radiation.ast.AST.parseFromSource("../jsdb/test/jquery.js")
		val ret = com.github._38.radiation.module.Closure.run(ast)
		val map = com.github._38.radiation.sourcemap.SourceMap.fromAST(ret, "test.min.js")
		//System.out.println(ret)
		//System.out.println(ret.targetCode)
		//System.out.println(map.dump)
		import java.io.{PrintWriter, File}
		val targetFile = new PrintWriter("../jquery.debug.js")
		targetFile.println(ret.targetCode)
		targetFile.println("//# sourceMappingURL=jquery.debug.map")
		targetFile.close
		val mappingFile = new PrintWriter("../jquery.debug.map")
		mappingFile.println(map.dump)
		mappingFile.close
		System.out.println(com.github._38.radiation.sourcemap.Lexer("{\"names\":\"AAAA,AAAA;AAAA,AAAC\"}".toStream).toList)
	}
}

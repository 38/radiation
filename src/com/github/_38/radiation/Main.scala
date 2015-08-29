import com.github._38.radiation.modules.ASTModule
import com.github._38.radiation.ast.{Node, ASTParser}
import com.github._38.radiation.codemap.{VLQCodeMap, Parser,Generator}
package com.github._38.radiation {
	case class Config(module:String = "", map:String = "" ,path:String = "")
	object Main {
		import scopt._
		val parser = new OptionParser[Config]("radiation") {
			head("radiation", "0.1")
			opt[String]('m', "module")  required()  action {(x,c) => c.copy(module = x)}    text("The class path to the module to run")
			opt[String]('p', "map")     optional()  action {(x,c) => c.copy(map = x)}       text("The source map file for the input")
			arg[String]("<file>")       unbounded() action {(x,c) => c.copy(path = x)}      text("The source file to read")
			help("help")                                                                    text("Show this help message")
		}
		def run_module(module:String, path:String) {
			import scala.reflect.runtime.universe
			val moduleClz = Class.forName("com.github._38.radiation.modules." + module + "$")
			val moduleObj = moduleClz.getField("MODULE$").get(null).asInstanceOf[ASTModule]
			val ast = moduleObj.run(ASTParser.fromFile(path))
			System.out.println(Generator.fromAST("test1.js", path, ast))
			System.out.println(ast.targetCode)
		}
		def main(args:Array[String]) {
			parser.parse(args, Config()) match {
				case Some(config) => run_module(config.module, config.path); System.exit(0)
				case None         => System.exit(1)
			}
			
		}
	}
}

import com.github._38.radiation.modules.ASTModule
import com.github._38.radiation.ast.{Node, ASTParser}

package com.github._38.radiation {
	case class Config(module:String = "")
	object Main {
		import scopt._
		val parser = new OptionParser[Config]("radiation") {
			head("radiation", "0.1")
			opt[String]('m', "module") action {(x,c) => c.copy(module = x)} text("The class path to the module to run")
			help("help")                                                   text("Show this help message")
		}
		def run_module(module:String) {
			import scala.reflect.runtime.universe
			val moduleClz = Class.forName("com.github._38.radiation.modules." + module + "$")
			val moduleObj = moduleClz.getField("MODULE$").get(null).asInstanceOf[ASTModule]
			System.out.println(moduleObj.run(ASTParser.fromString("function f(x,y){return x + y;}")).targetCode)
		}
		def main(args:Array[String]) {
			
            ASTParser.fromFile("../jsdb/test/jquery.js")
			/*parser.parse(args, Config()) match {
				case Some(config) => run_module(config.module); System.exit(0)
				case None         => System.exit(1)
			}*/
			
		}
	}
}

import com.github._38.radiation.ast._

object Main {
	def main(args:Array[String]) {
		System.out.println(AST.parseFromSource("../jsdb/test/jquery.js").targetCode)
	}
}

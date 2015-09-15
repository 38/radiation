package com.github._38.radiation

import com.github._38.radiation.ast._
import com.github._38.radiation.sourcemap._
import Base64IntConverters._
object Main {
	def main(args:Array[String]) {
		//System.out.println(AST.parseFromSource("../jsdb/test/jquery.js").targetCode)
		val a = Base64Int(3145728)
		System.out.println(a:Int)
		val s = "zd.".toStream
		val (what, next) = Base64Int.fromStream(s)
		System.out.println(what)	
		System.out.println(next.toList)	
	}
}

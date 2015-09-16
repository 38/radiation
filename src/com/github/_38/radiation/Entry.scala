package com.github._38.radiation

import com.github._38.radiation.ast._
import com.github._38.radiation.sourcemap._
import Base64IntConverters._
object Main {
	def main(args:Array[String]) {
		val test = "\'\\ntest\\u24fd\"\' { } 123 "
		System.out.println(test)
		System.out.println(Lexer(test.toStream).toList)
	}
}

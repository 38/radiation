package com.github._38.radiation.sourcemap

import org.mozilla.javascript.TokenStream

trait Token;
case class StringLiteral(what:String) extends Token;
case class IntegerLiteral(what:Int) extends Token;
case class Keyword(what:String) extends Token;

object Lexer {
	def apply(chars:Stream[Char]):Stream[Token] = {
	}
}
/** A minimized JSON parser sepecified for source map */ 
object Parser {
	
}

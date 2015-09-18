package com.github._38.radiation.sourcemap

import org.mozilla.javascript.ScriptRuntime

import scala.annotation.tailrec

import Base64Int.fromStream

class SyntaxError(message:String) extends Exception {
	override def toString = "Syntax Error: %s" format message
}

trait Token;
case class StringLiteral(what:String) extends Token {
	override def toString = "StringLiteral(\"%s\")" format ScriptRuntime.escapeString(what, '"')
}
case class IntegerLiteral(what:Int) extends Token;
case class Keyword(what:String) extends Token;
case object VLQListBegin extends Token;
case object VLQListEnd   extends Token;

/** The SourceMap Lexer, a subset of JSON */
object Lexer {
	private object _hex {
		def unapply(c:Char):Option[Int] =
		    if(c >= '0' && c <= '9')        Some(c - '0')
		    else if (c >= 'a' && c <= 'f')  Some(c - 'a' + 10)
		    else if (c >= 'A' && c <= 'F')  Some(c - 'A' + 10)
		    else                            None
	}
	private object _int {
		def unapply(c:Char):Option[(Boolean, Int)] =
		    if(c >= '0' && c <= '9')    Some(((c != '0'), c - '0'))
		    else                        None
	}
	private object _whitespace {
		def unapply(c:Char) =
		    if(c == '\n' || c == '\r' || c == '\t' || c == ' ') Some(())
		    else None
	}
	private def _add(sb:StringBuilder, c:Char, ret:Stream[Char]) = {
		sb append c
		ret
	}
	private def _parseEscape(chars:Stream[Char], result:StringBuilder):Stream[Char] = chars match {
		case '"'  #:: rem => _add(result, '"',  rem)
		case '\''  #:: rem => _add(result, '\'',  rem)
		case '\\' #:: rem => _add(result, '\\', rem)
		case '/'  #:: rem => _add(result, '/',  rem)
		case 'b'  #:: rem => _add(result, '\b', rem)
		case 'f'  #:: rem => _add(result, '\f', rem)
		case 'n'  #:: rem => _add(result, '\n', rem)
		case 'r'  #:: rem => _add(result, '\r', rem)
		case 't'  #:: rem => _add(result, '\t', rem)
		case 'u'  #:: _hex(a) #:: _hex(b) #:: _hex(c) #:: _hex(d) #:: rem => _add(result, (a * 4096 + b * 256 + c * 16 + d).toChar, rem)
		case _            => throw new SyntaxError("Invalid escape charecter")
	}
	@tailrec
	private def _parseString(chars:Stream[Char], result:StringBuilder, quoteChar:Char):(Stream[Char], String) = chars match {
		case '\\' #:: rem => _parseString(_parseEscape(rem, result), result, quoteChar)
		case c   #:: rem if(c == quoteChar) => (rem, result.toString)
		case c   #:: rem => {
			result append c
			_parseString(rem, result, quoteChar)
		}
		case Stream()    => throw new SyntaxError("Unexpected EOS")
	}
	@tailrec
	private def _parseInt(chars:Stream[Char], result:Int):(Stream[Char], Int) = chars match {
		case _int(_, i) #:: rem => _parseInt(rem, result * 10 + i)
		case _                  => (chars, result)
	}
	@tailrec
	def apply(chars:Stream[Char], state:Int = 0):Stream[Token] = chars match {
		case c #:: rem if ((c == '"' || c == '\'') && state == 2) => VLQListBegin #:: apply(chars, 3) 
		case c #:: rem if (c == 3) => {
			val (what, remaining) = fromStream(chars)
			what #:: apply(remaining, 3)
		}
		/* TODO: close the list */
		case c #:: rem if (c == '"' || c == '\'')  => {
			val strval = new StringBuilder
			val (unparsed, what) = _parseString(rem, strval, c)
			val nextState = state match {
				case 0 if(what == "name") => 1
				case _                    => 0
			}
			StringLiteral(what) #:: apply(unparsed, status)
		}
		case _int(true, i) #:: rem => {
			val (unparsed, what) = _parseInt(rem, i)
			IntegerLiteral(what) #:: apply(unparsed, 0)
		}
		case 'n' #:: 'u' #:: 'l' #:: 'l' #:: (rem @ (c #:: _)) if('a' <= c && c <= 'z') => Keyword("null") #:: apply(rem, 0)
		case _whitespace(()) #:: rem    => apply(rem, state)
		case ':' #:: rem if(state == 1) => Keyword(":") #:: apply(rem, 2)
		case whatever #:: rem => Keyword(whatever.toString) #:: apply(rem, 0)
		case Stream() => Stream()
	}
}
/** A minimized JSON parser sepecified for source map */
object Parser {
	/*private def _parseJSONBody(tokens:Stream[Token], result:Map[(String, Any)]):(Stream[Token], Map[(String, Any)]) = {
    }
	private def _parseSourceMap(tokens:Stream[Token]):SourceMap = tokens match {
	case Keyword("{") #:: next  =>  {
	    val (remaining, dict) = _parseJSONBody(next, Map())
	    new SourceMap(Nil)
        }
	case _                      => throw new SyntaxError("Source map must start with `{'")
    }
	def apply(chars:Stream[Char]):SourceMap = {
	
    }*/
}

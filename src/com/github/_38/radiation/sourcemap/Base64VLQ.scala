package com.github._38.radiation.sourcemap

import scala.language.implicitConversions

import scala.annotation.tailrec

class InvalidVLQSequence(what:String) extends Exception {
	override def toString = "Invalid VLQ Value: %s" format what
}

object Base64Char {
	trait CharType {
		val charList:String
		lazy val values = (charList zip (0 to 31)).toMap
		lazy val chars  = ((0 to 31) zip charList).toMap
		def apply(value:Int):Char = chars(value)
		def unapply(char:Char) = values.get(char)
		def contains(char:Char) = values contains char
	}
    object Term extends CharType    {val charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"}
    object Nonterm extends CharType {val charList = "ghijklmnopqrstuvwxyz0123456789+/"}
    def apply(t:CharType, v:Int) = t(v)
    def unapply(c:Char) = c match {
        case Term(c)    => Some(Term, c)
        case Nonterm(c) => Some(Nonterm, c)
        case _          => None
    }
}

class Base64Int(val value:Int) {
    def encode:String = {
        val sb = new StringBuilder;
        var raw = if(value < 0) - value * 2 + 1 else value * 2
        while(raw >= 32) {
            sb append Base64Char(Base64Char.Nonterm, raw % 32)
            raw = raw / 32
        }
        sb append Base64Char(Base64Char.Term, raw)
        sb.toString
    }
    override def toString:String = "VLQ(%d)" format value
}

object Base64Int {
	/** Parse a Base64VLQ Value from a stream of char 
	 *  @return the tuple (Parsed VLQ Value, new stream contains remaining charecters */
	def fromStream(stream:Stream[Char]):(Base64Int, Stream[Char]) = {
		@tailrec def _parseImpl(stream:Stream[Char], raw_value:Int, multipler:Int):(Stream[Char], Int, Int) = stream match {
				case Base64Char(Base64Char.Nonterm, c)  #:: rem => _parseImpl(rem, c * multipler + raw_value, multipler * 32)
				case Base64Char(Base64Char.Term, c)     #:: rem => (rem, c * multipler + raw_value, multipler * 32)
				case whatever #:: rem                           => throw new InvalidVLQSequence(whatever.toString)
				case Stream()                                   => throw new InvalidVLQSequence("<EOS>")
		}
		val (remaining, raw_value, _) =  _parseImpl(stream, 0, 1)
		(Base64Int(if(raw_value % 2 == 1) - raw_value / 2 else raw_value / 2), remaining)
	}
	def apply(what:Int) = new Base64Int(what)
}

object Base64IntConverters {
	implicit def base64Value2Int(what:Base64Int) = what.value
	implicit def base64Value2Str(what:Base64Int) = what.encode
}

package com.github._38.radiation.sourcemap

object Base64Char {
    trait CharType {
        val charList:String
        val values = charList zip (0 to 31) toMap
        val chars  = (0 to 31) zip charList toMap
        def apply(value:Int):Char = chars(value)
        def unapply(char:Char) = values.get(char)
        def contains(char:Char) = values contains char
    }
    object Term extends CharType {val charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"}
    object Nonterm extends CharType {val charList = "ghijklmnopqrstuvwxyz0123456789+/"}
    def apply(t:CharType, v:Int) = t(v)
    def unapply(c:Char) = c match {
        case Term(c)    => Some(Term, c)
        case Nonterm(c) => Some(Nonterm, c)
        case _          => None
    }
}

case class Base64Int(val value:Int) {
    def encode:String = {
        val sb = new StringBuilder;
        var raw = if(value < 0) - value * 2 + 1 else value
        while(raw >= 32) {
            sb append Base64Char(Base64Char.Nonterm, raw % 32)
            raw = raw - 32
        }
        sb append Base64Char(Base64Char.Term, raw)
        sb.toString
    }
}

object Base64Int {
}

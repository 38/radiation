import scala.language.postfixOps

package com.github._38.radiation.codemap {
    object VLQCodeMap {
        class InvalidVLQException(val message:String) extends Exception { override def toString = message;}
        trait Token;
        case class Position(val lineNum:Int, val lineOfs:Int);
        case class  Num(val value:Int) extends Token;
        case object MSep extends Token;
        case object LSep extends Token;
        case class  CodeMap(val srcId:Int, val symbolId:Int, val genPos:Position, val orgPos:Position) extends Token;
        case class _VLQParserState(val s:Stream[Char], val e:Int, val v:Int);
        case class _ItemParserState(val line:Int, val column:Int) {
            def nextColumn(delta:Int) = _ItemParserState(line, column + delta);
            def nextLine = _ItemParserState(line + 1, 0);
        }
        lazy val charSet = ('A' to 'Z') ++ ('a' to 'z') ++ Seq('+', '/')
        lazy val termVal = ((0 to 31) zip charSet) toMap
        lazy val initVal = ((0 to 31) zip (charSet drop 32)) toMap
        lazy val termChar = termVal map (_ swap);
        lazy val initChar = initVal map (_ swap);
        def _rawVal2S(v:Int):String = if(v >= 32) initVal(v % 32) + _rawVal2S(v / 32) else termVal(v) toString
        def valueToVLQ(v:Int) = _rawVal2S(if(v < 0) - v * 2 + 1 else v * 2)
        def _parseInits(state:_VLQParserState):_VLQParserState = state match {
            case _VLQParserState(h #:: t, e, v) if (initChar contains h) => _parseInits(_VLQParserState(t, e * 32, v + e * initChar(h)))
            case _ => state
        }
        def _mkInt(rawValue:Int) = Num(if(rawValue % 2 == 1) - rawValue / 2 else rawValue / 2)
        def _parseLex(s:Stream[Char]):Stream[Token] = s match {
           case ',' #:: t => MSep #:: _parseLex(t)
           case ';' #:: t => LSep #:: _parseLex(t)
           case c #:: t   => _parseInits(_VLQParserState(s, 1, 0)) match {
               case _VLQParserState(h #:: t, e, v) if (termChar contains h) => _mkInt(v + e * termChar(h)) #:: _parseLex(t)
           }
           case Stream.Empty => Stream.Empty
        }
        def _parseItem(s:Stream[Token], state:_ItemParserState = _ItemParserState(0,0)):Stream[Token] = s match {
            case Num(genCol) #:: (rem @ (LSep | MSep) #::_) => 
                CodeMap(-1, -1, Position(state.line, state.column + genCol), null) #:: _parseItem(rem, state nextColumn genCol)
            case Num(genCol) #:: Num(srcIdx) #:: Num(orgLine) #:: Num(orgCol) #:: (rem @ (LSep | MSep) #:: _)=>
                CodeMap(srcIdx, -1, Position(state.line, state.column + genCol), Position(orgLine, orgCol)) #:: _parseItem(rem, state nextColumn genCol)
            case Num(genCol) #:: Num(srcIdx) #:: Num(orgLine) #:: Num(orgCol) #:: Num(symId) #:: (rem @ (LSep | MSep) #:: _) =>
                CodeMap(srcIdx, symId, Position(state.line, state.column + genCol), Position(orgLine, orgCol)) #:: _parseItem(rem, state nextLine)
            case LSep #:: rem     => _parseItem(rem, state nextLine)
            case MSep #:: rem     => _parseItem(rem, state)
            case whatever #:: rem => whatever #:: _parseItem(rem)
            case Stream.Empty     => Stream.Empty
        }
    }
}

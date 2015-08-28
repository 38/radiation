import scala.language.postfixOps
import scala.language.implicitConversions
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
		case class _ItemParserState(val fileno:Int, val gline:Int, val gcol:Int, val sline:Int, val scol:Int, val name:Int) {
			//def nextColumn(delta:Int) = _ItemParserState(line, column /*+ delta*/);
			//def nextLine = _ItemParserState(line + 1, 0);
			def NL = _ItemParserState(fileno, gline + 1, 0, sline, scol, name)
		}
		implicit def _convertToParserState(map:CodeMap):_ItemParserState = {
			val CodeMap(fileno, name, Position(gline, gcol), Position(sline, scol)) = map
			_ItemParserState(fileno, gline, gcol, sline, scol, name)
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
		def _nextStream(status:_ItemParserState, rem:Stream[Token], gcol:Int = 0, fileno:Int = 0, sline:Int = 0, scol:Int = 0, name:Int = 0) = {
			val newfileno = fileno + status.fileno
			val newgline  = status.gline
			val newgcol   = gcol + status.gcol
			val newsline  = sline + status.sline
			val newscol   = scol  + status.scol
			val newname   = name + status.name
			val next = CodeMap(newfileno, newname, Position(newgline, newgcol), Position(newsline, newscol))
			next #:: _parseItem(rem, next)
		}
		def _parseItem(s:Stream[Token], state:_ItemParserState = _ItemParserState(0,0,0,0,0,0)):Stream[Token] = s match {
			case Num(genCol) #:: (rem @ (LSep | MSep) #::_) =>
			    _nextStream(state, rem, genCol)
			case Num(genCol) #:: Num(srcIdx) #:: (rem @ (LSep | MSep) #:: _)=>
			    _nextStream(state, rem, genCol, srcIdx)
			case Num(genCol) #:: Num(srcIdx) #:: Num(orgLine) #:: (rem @ (LSep | MSep) #:: _)=>
			    _nextStream(state, rem, genCol, srcIdx, orgLine)
			case Num(genCol) #:: Num(srcIdx) #:: Num(orgLine) #:: Num(orgCol) #:: (rem @ (LSep | MSep) #:: _)=>
			    _nextStream(state, rem, genCol, srcIdx, orgLine, orgCol)
			case Num(genCol) #:: Num(srcIdx) #:: Num(orgLine) #:: Num(orgCol) #:: Num(symId) #:: (rem @ (LSep | MSep) #:: _) =>
			    _nextStream(state, rem, genCol, srcIdx, orgLine, orgCol, symId)
			case LSep #:: rem     => _parseItem(rem, state NL)
			case MSep #:: rem     => _parseItem(rem, state)
			case whatever #:: rem => whatever #:: _parseItem(rem)
			case Stream.Empty     => Stream.Empty
		}
		def parse(map:String) = _parseItem(_parseLex(map toStream))
	}
}

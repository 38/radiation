import scala.language.postfixOps
import scala.language.implicitConversions
import scala.util.parsing.json.JSON

package com.github._38.radiation.codemap {
	/** Parse the VLQ Code Maps
	 *  @note see <a href="https://goo.gl/OhKc7s">Source Map Documentation</a> for format detials
	 **/
	object VLQCodeMap {
		class InvalidVLQException(val message:String) extends Exception { override def toString = message;}
		trait Token;
		case class Position(val lineNum:Int, val lineOfs:Int);
		case class  Num(val value:Int) extends Token;
		case object MSep extends Token;
		case object LSep extends Token;
		case class  CodeMap(val srcId:Int, val symbolId:Int, val genPos:Position, val orgPos:Position) extends Token;
		case class _VLQParserState(val s:Stream[Char], val e:Int, val v:Int);
		/** The internal code map item parser state
		 *  @note  all the values in this state are absolute value
		 *  @param srcId        The source code Index
		 *  @param genLine      The Generated Line number
		 *  @param genColumn    The Column number in the Gereanted file
		 *  @param orgLine      The line number in original file
		 *  @param orgColumn    The column number in original file
		 *  @param symbolId     The symbol inde
		 */
		case class _ItemParserState(val srcId:Int, val genLine:Int, val genColumn:Int, val orgLine:Int, val orgColumn:Int, val symbolId:Int) {
			def NL = _ItemParserState(srcId, genLine + 1, 0, orgLine, orgColumn, symbolId)
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
		def _nextStream(status:_ItemParserState, rem:Stream[Token], genColumn:Int = 0, srcId:Int = 0, orgLine:Int = 0, orgColumn:Int = 0, symId:Int = 0) = {
			val newfileno = srcId + status.srcId
			val newgline  = status.orgLine
			val newgcol   = orgColumn + status.orgColumn
			val newsline  = orgLine + status.orgLine
			val newscol   = orgColumn  + status.orgColumn
			val newname   = symId + status.symbolId
			val next = CodeMap(newfileno, newname, Position(newgline, newgcol), Position(newsline, newscol))
			next #:: _parseItem(rem, next)
		}
		def _parseItem(s:Stream[Token], state:_ItemParserState = _ItemParserState(0,0,0,0,0,0)):Stream[Token] = s match {
			case Num(genCol) #:: (rem @ (LSep | MSep) #::_) =>
			    _nextStream(state, rem, genCol)
			case Num(genCol) #:: Num(srcId) #:: (rem @ (LSep | MSep) #:: _)=>
			    _nextStream(state, rem, genCol, srcId)
			case Num(genCol) #:: Num(srcId) #:: Num(orgLine) #:: (rem @ (LSep | MSep) #:: _)=>
			    _nextStream(state, rem, genCol, srcId, orgLine)
			case Num(genCol) #:: Num(srcId) #:: Num(orgLine) #:: Num(orgCol) #:: (rem @ (LSep | MSep) #:: _)=>
			    _nextStream(state, rem, genCol, srcId, orgLine, orgCol)
			case Num(genCol) #:: Num(srcId) #:: Num(orgLine) #:: Num(orgCol) #:: Num(symId) #:: (rem @ (LSep | MSep) #:: _) =>
			    _nextStream(state, rem, genCol, srcId, orgLine, orgCol, symId)
			case LSep #:: rem     => _parseItem(rem, state NL)
			case MSep #:: rem     => _parseItem(rem, state)
			case whatever #:: rem => whatever #:: _parseItem(rem)
			case Stream.Empty     => Stream.Empty
		}
		def parse(map:String) = _parseItem(_parseLex(map toStream))
	}
	case class SourceMapFile(version:Int, generated:String, sources:List[String], sourceContent:String, symbols:List[String], mappings:List[VLQCodeMap.CodeMap]);
	object Parser {
		def fromFile(fileName:String) = {
			val plain_text = scala.io.Source.fromFile(fileName).getLines.reduceLeft(_+_)
			val json_object = JSON.parseFull(plain_text)
			System.out.println(json_object)
			0
		}
	}
}

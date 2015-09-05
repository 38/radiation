import scala.language.postfixOps
import scala.language.implicitConversions
import scala.util.parsing.json.JSON
import scala.math.max

import org.mozilla.javascript.ScriptRuntime

import com.github._38.radiation.source.Location

package com.github._38.radiation.codemap {
	/** Parse the VLQ Code Maps
	 *  @note see <a href="https://goo.gl/OhKc7s">Source Map Documentation</a> for format detials
	 **/
	object VLQCodeMap {
		class InvalidVLQException(val message:String) extends Exception { override def toString = message;}
		trait Token;
		case class  Num(val value:Int) extends Token;
		case object MSep extends Token;
		case object LSep extends Token;
		case class  CodeMap(val srcId:Int, val symbolId:Int, val genPos:Location, val orgPos:Location) extends Token with Ordered[CodeMap] {
			private def _get(idx:Int) = idx match {
				case 0 => Some(genPos.line)
				case 1 => Some(genPos.column)
				case 2 => Some(srcId)
				case 3 => Some(orgPos.line)
				case 4 => Some(orgPos.column)
				case 5 => Some(symbolId)
				case _ => None
			}
			private def _compareImpl(that:CodeMap, how:Int):Int = (this _get how, that _get how) match {
				case (Some(a), Some(b)) if a == b => _compareImpl(that, how + 1)
				case (Some(a), Some(b)) => a - b
				case (None   , None)    => 0
			}
			def compare(that: CodeMap) = _compareImpl(that, 0)
			def -(that:CodeMap) = CodeMap(srcId - that.srcId, symbolId - that.symbolId, genPos - that.genPos, orgPos - that.orgPos)
			private def _toVLQ(fid:Int = 1):String = _get(fid) match {
				case Some(x) => {
					val tail = _toVLQ(fid + 1)
					if(tail == "" && x == 0) "" else valueToVLQ(x) + tail
				}
				case None    => ""
			}
			def toVLQ = {
				val result = _toVLQ()
				result + "A" * max(4 - result.length, 0)
			}
			def NL = CodeMap(srcId, symbolId, Location(genPos.line + 1, 0), orgPos)
		}
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
			val CodeMap(fileno, name, Location(gline, gcol), Location(sline, scol)) = map
			_ItemParserState(fileno, gline, gcol, sline, scol, name)
		}
		lazy val charSet = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('+', '/')
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
			val next_srcId     = srcId + status.srcId
			val next_genLine   = status.genLine
			val next_genColumn = genColumn + status.genColumn
			val next_srcLine   = orgLine + status.orgLine
			val next_srcColumn = orgColumn  + status.orgColumn
			val next_name      = symId + status.symbolId
			val next = CodeMap(next_srcId, next_name, Location(next_genLine, next_genColumn), Location(next_srcLine, next_srcColumn))
			next #:: _parseItem(rem, next)
		}
		def _parseItem(s:Stream[Token], state:_ItemParserState = _ItemParserState(0,0,0,0,0,0)):Stream[Token] = s match {
			case Num(genCol) #:: (rem @ (LSep | MSep) #::_) =>
			    _nextStream(state, rem, genCol)
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
	case class SourceMapFile(version:Int,
	                         generated:Option[String],
	                         sources:Option[List[String]],
	                         symbols:Option[List[String]],
	                         mappings:List[VLQCodeMap.CodeMap])
	{
		import VLQCodeMap.CodeMap
		private def _opt(name:String, what:Option[String]) = what match {
			case Some(what) => List("\"%s\":\"%s\"".format(ScriptRuntime.escapeString(name, '"'), ScriptRuntime.escapeString(what, '"')))
			case None       => List()
		}
		private def _optl(name:String, what:Option[List[String]]) = what match {
			case Some(what) => List("\"%s\":%s".format(ScriptRuntime.escapeString(name, '"'),
                                                       "[" + (what map ("\"" + ScriptRuntime.escapeString(_, '"') + "\"")).mkString(",") + "]"))
			case None      => List()
		}
		private def _encodeMapping(m:List[CodeMap]) = {
			val grouped = m.groupBy(_.genPos.line);
			val lines   = (0 to grouped.keySet.max).map(x => grouped get x match {
				case Some(mappings) => mappings.sorted
				case None           => List()
			}).toList
			def _encodeLine(line:List[CodeMap], last:CodeMap):(List[String], CodeMap) = {
				var lst = last
				var ret = scala.collection.mutable.ArrayBuffer[String]()
				for(x <- line) {
					ret += (x - lst).toVLQ
					lst = x
				}
			    
				(ret.toList, lst)
			}
			def _encodeLines(l:List[List[CodeMap]], last:CodeMap):String = l match {
				case x :: xs => {
					val (line, lst) = _encodeLine(x, last)
					line.mkString(",")+ ";" + _encodeLines(xs, lst NL)
				}
				case _      => ""
			}
			_encodeLines(lines, CodeMap(0,0,Location(0,0), Location(0,0)))
		}
		def toVLQMap = {
			val props = List() ++
			            List("\"version\":" + version) ++
			            _opt("file", generated) ++
			            _optl("sources", sources) ++
			            _optl("names", symbols) ++
			            List("\"mappings\":\"%s\"".format(_encodeMapping(mappings)))
			"{%s}".format(props mkString ",")
		}
	}
	object Parser {
		def fromFile(fileName:String) = {
			import VLQCodeMap.CodeMap
			val plain_text = scala.io.Source.fromFile(fileName).getLines.reduceLeft(_+_)
			val obj = (JSON.parseFull(plain_text) match {
				case Some(what:Map[_,_]) => what
				case _                   => Map[String,Any]()
			}).asInstanceOf[Map[String,Any]]
			val version = obj("version").asInstanceOf[Double]
			val mapstr  = obj("mappings").asInstanceOf[String]
			val mappings       = VLQCodeMap.parse(mapstr + ";").toList.filter(_.isInstanceOf[CodeMap])
			SourceMapFile(version.toInt,
			              obj.get("file").asInstanceOf[Option[String]],
			              obj.get("sources").asInstanceOf[Option[List[String]]],
			              obj.get("names").asInstanceOf[Option[List[String]]],
			              mappings.map(_.asInstanceOf[CodeMap]))
		}
	}
	object Generator {
		import com.github._38.radiation.ast.Node
		import VLQCodeMap.CodeMap
		val VERSION = 3
		private def _getMappings(srcIdx:Int, offset:Int, tree:Node):Set[CodeMap] = {
			val current  = tree.lexerTokenMappings map (x => CodeMap(srcIdx, 0, Location(0, offset + x.target), x.source))
			val children = tree.getChildren map (x => _getMappings(srcIdx, offset + x.offset, x.node))
			val all = current :: children
			(Set[CodeMap]() /: all)(_ ++ _) 
		}
		def fromAST(targetFile:String, sourceFile:String, tree:Node):String = {
			val mappings = _getMappings(0, 0, tree).toList
			SourceMapFile(VERSION,
			              Some(targetFile),
			              Some(List(sourceFile)),
			              None,
			              mappings).toVLQMap
		}
	}
}

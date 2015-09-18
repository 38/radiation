package com.github._38.radiation.sourcemap

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import com.github._38.radiation.source.InSource
import com.github._38.radiation.ast.{Node, Complex, Lexical, Nothing}
import com.github._38.radiation.ast.Helper.whiteSpaces

import org.mozilla.javascript.ScriptRuntime.escapeString

case class SourceMapItem(val source:InSource, val target:InSource, val symbol:Option[String])
// TODO: because symbol sometimes gone, so we need ? 

class SourceMap(val mappings:List[SourceMapItem]) {
	import Base64Int._
	import Base64IntConverters._
	val version = 3
	private class _SourceMapInteral(val sourceFile:Int,
	                                val symbol:Int,
	                                val sourceLine:Int,
	                                val sourceColumn:Int,
	                                val targetLine:Int,
	                                val targetColumn:Int) extends Ordered[_SourceMapInteral]{
		def compare(that:_SourceMapInteral) =
		    if      (targetLine   != that.targetLine)   targetLine - that.targetLine
		    else if (targetColumn != that.targetColumn) targetColumn - that.targetColumn
		    else if (sourceFile   != that.sourceFile)   sourceFile - that.sourceFile
		    else if (sourceLine   != that.sourceLine)   sourceLine - that.sourceLine
		    else if (sourceColumn != that.sourceColumn) sourceColumn - that.sourceColumn
		    else if (symbol       != that.symbol)       symbol - that.symbol
		    else 0
		def -(that:_SourceMapInteral):List[Int] = {
			val dTargetColumn = targetColumn - that.targetColumn
			val dSourceFile   = sourceFile - that.sourceFile
			val dSourceLine   = sourceLine - that.sourceLine
			val dSourceColumn = sourceColumn - that.sourceColumn
			val dSymbol       = symbol - that.symbol
			//if(dSourceFile == 0 && dSourceLine == 0 && dSourceColumn == 0 && dSymbol == 0) return dTargetColumn :: Nil
			if(symbol == -1) return dTargetColumn :: dSourceFile :: dSourceLine :: dSourceColumn :: Nil
			dTargetColumn:: dSourceFile :: dSourceLine :: dSourceColumn :: dSymbol :: Nil
		}
		def newLine(n:Int) = new _SourceMapInteral(sourceFile, symbol, sourceLine, sourceColumn, targetLine + n, 0)
		override def toString = "(%d,%d,%d,%d,%d,%d)".format(sourceFile, symbol, sourceLine, sourceColumn, targetLine, targetColumn)
		//TODO keep symbol id ....
	}
	@tailrec
	private def _toInternal(mappings:List[SourceMapItem], 
							result:Array[_SourceMapInteral], 
							sources:Map[String, Int], 
							symbols:Map[String, Int],
							n:Int):(Map[String, Int], Map[String, Int]) =
	    mappings match {
		    case item :: xs => {
			    val sourceFile = item.source.file
			    val newSources = if(sources contains sourceFile) sources else sources + (sourceFile -> sources.size)
				val newSymbols = item.symbol match {
					case Some(symbol) if (!(symbols contains symbol)) => symbols + symbol
					case _                                            => symbols
				}
			    val fileIdx = newSources(sourceFile)
				val symbolIdx = item.symbol match {
					case Some(symbol) => newSymbols(symbol)
					case None         => -1
				}
			    result(n) = new _SourceMapInteral(fileIdx, symbolIdx, item.source.line, item.source.column, item.target.line, item.target.column)
			    _toInternal(xs, result, newSources, n + 1)
		    }
		    case Nil       => (sources, symbols)
	    }
	def _listToVLQ(list:List[Int]) = list.map(Base64Int(_).encode) mkString ""
	def dump = {
		val result = new StringBuilder
		
		def _dumpList(key:String, value:List[String]) {
			result append "\"%s\":".format(key)
			var first = true
			for(f <- value){
				if(first) result append '['
				else result append ','
				result append "\"%s\"".format(escapeString(f, '"'))
			}
			result append "]"
		}
		
		val buf = new Array[_SourceMapInteral](mappings.length)
		val (sources, symbols) = _toInternal(mappings, buf, Map(), 0)
		scala.util.Sorting.quickSort(buf)
		result append "{"
		result append "\"version\":3,"
		result append "\"sourceRoot\":\"\","
		_dumpList("sources", sources.toList.map(x => (x._2, x._1)).sorted.map(x => x._2))
		result append ","
		result append "\"names\":[],"
		result append "\"mappings\":\""
		var currentLine = 0
		var lastItem = new _SourceMapInteral(0,-1,0,0,0,0)
		var first = true
		for(item <- buf){
			if(item.targetLine != currentLine) {
				for(_ <- 1 to item.targetLine - currentLine) result append ';'
				lastItem.newLine(item.targetLine - currentLine)
				currentLine = item.targetLine
			} else if(!first) result append ","
			else first = false
			for(num <- (item - lastItem)) result append Base64Int(num).encode
			lastItem = item
		}
		result append "\""
		result append "}"
		result.toString
	}
}
object SourceMap {
	def fromAST(ast:Node, target:String) = {
		def _fromAST(ast:Node, target:String, result:ListBuffer[SourceMapItem], scopeName:String = "" ,offset:Int = 0) {
			ast match {
				case Lexical(_, where:InSource)  => result += SourceMapItem (where, InSource(target, 0, offset), if(scopeName == "") None else Some(scopeName))
				case c:Complex => {
					var ofs = offset
					var prevous:Node = null
					val childScope = c match {
						case FuncExpr(Some(Id(Lexical(what, _))), _)	=> if(scopeName == "") what else scopeName + "." + what
						case _                                          => scopeName
					}
					for(child <- c.child) {
						ofs += (if(prevous == null) 0 else whiteSpaces(prevous, child))
						if(child.nodeType != Block) _fromAST(child, target, result, scopeName, ofs)
						else _fromAST(child, target, result, childScope, ofs)
						ofs += child.targetCodeLength
						prevous = child
					}
				}
				case _ => ()
			}
		}
		val buffer = new ListBuffer[SourceMapItem]()
		_fromAST(ast, target, buffer, 0)
		new SourceMap(buffer.toList)
	}
}

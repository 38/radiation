package com.github._38.radiation.sourcemap

import scala.annotations.{tailrec, inline}

import com.github._38.radiation.source.InSource


case class SourceMapItem(val source:InSource, val target:InSource, val symbol:Option[String])

class SourceMap(val mappings:List[SourceMapItem]) {
	val version = 3
	private class _SourceMapInteral(val sourceFile:Int, val symbol:Int, val sourceLine:Int, val sourceCloumn:Int, val targetLine:Int, val targetCloumn:Int) {
		@inline
		def compare(that:_SourceMapInteral) =
		    if      (targetLine   != that.targetLine)   return targetLine - that.targetLine
		    else if (targetColumn != that.targetColumn) return targetColumn - that.targetColumn
		    else if (sourceFile   != that.sourceFile)   return sourceFile - that.sourceFile
		    else if (sourceLine   != that.sourceLine)   return sourceLine - that.sourceLine
		    else if (sourceColumn != that.sourceColumn) return sourceColumn - that.sourceColumn
		    else if (symbol       != that.symbol)       return symbol - that.symbol
		    return 0
		@inline
		def -(that:_SourceMapInteral):List[Int] = {
			val dTargetColumn = targetColumn - that.targetColumn
			val dSourceFile   = sourceFile - that.sourceFile
			val dSourceLine   = sourceLine - that.sourceLine
			val dSourceColumn = sourceColumn - that.sourceColumn
			val dSymbol       = symbol - that.symbol
			if(dSourceFile == 0 && dSourceLine == 0 && dSourceColumn == 0 && dSymbol == 0) return dTargetColumn :: Nil
			if(dSymbol == 0) return dTargetColumn :: dSourceFile :: dSourceLine :: dSourceColumn :: Nil
			dTargetColumn:: dSourceFile :: dSourceLine :: dSourceColumn :: dSymbol
		}
	}
	@tailrec
	private def _toInternal(mappings:List[SourceMapItem], result:Array[_SourceMapInteral], sources:Map[String, Int], n:Int):Map[String, Int] = mappings match {
		case item :: xs => {
			val sourceFile = item.sourceFile
			val newSources = if(sources contains sourceFile) sources else sources + (sourceFile -> sources.size)
			val fileIdx = newSources(sourceFile)
			result[n] = new _SourceMapInteral(fileIdx, 0, item.source.line, item.source.column, item.target.line, item.target.column)
			_toInternal(xs, result, newSources, n + 1)
		}
		case Nil        => sources
	}
	def dump = {
		val buf = new Array[_SourceMapInteral](mappings.length)
		val sources = _toInternal(mappings, buf, Map(), 0)
		/* TODO */
	}
}

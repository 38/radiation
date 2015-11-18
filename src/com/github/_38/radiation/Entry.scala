package com.github._38.radiation
import com.github._38.radiation.ast._
import com.github._38.radiation.source._

import scala.collection.mutable.{Map => MutableMap}

object Main {

	case class Result(symbol:String, location:Location) {
		override def toString = "symbol `" + symbol + "' has never been used after decleared. (" + location + ")"
	}
	case class AnalyzerResult(val refSyms:Set[String], val symsDecl:Map[String, Location], val result:Set[Result]) {
		def +(that:AnalyzerResult) = AnalyzerResult(refSyms ++ that.refSyms, symsDecl ++ that.symsDecl, result ++ that.result)
	}

	val EmptyResult = AnalyzerResult(Set(), Map(), Set())

	def analyzeAST(root:Node):AnalyzerResult = root match {
		case node:Complex => {
			val newSymbols = MutableMap[String, Location]()
			val AnalyzerResult(refSyms, symbols, result) = (node match {
				case DeclList((_, varlist)) => varlist.foldLeft(List[Node]())((lst, nxt) => nxt match {
					case Decl((Id(Lexical(what, where)), Some(initval))) => {
						newSymbols += (what -> where) 
						initval :: lst
					}
					case _ => lst
				})
				case FuncExpr((name, Arguments(args), body)) =>  {
					args.foreach((node) => node match {
						case Id(Lexical(what, where)) => newSymbols += (what -> where)
					})
					List(body)
				}
				case _ => node.child			
			}).foldLeft(EmptyResult) ((res, ch) => analyzeAST(ch) + res)

			node.nodeType match {
				case _:Scope => {
					val Some(scopeInfo:ScopeMetaData) = node.metaData
					val symlist = newSymbols.toMap ++ symbols 
					val parentSym = node match {
						case FuncExpr(Some(Id(Lexical(what, where))), _, _) => Map(what -> where)
						case _ => Map[String, Location]()
					}
					val unused = scopeInfo.locals &~ refSyms
					AnalyzerResult(refSyms &~ scopeInfo.locals, parentSym , result ++ unused.map(what => Result(what, symlist(what)))) 
				}
				case Id => {
					val Id(Lexical(what, _)) = node
					AnalyzerResult(refSyms + what, Map() , result)
				}
				case _ => AnalyzerResult(refSyms, newSymbols.toMap ++ symbols, result)
			}
		}
		case _ => EmptyResult
	}
	def main(args:Array[String]) = args.foreach(file => {
		try {
			val ast = AST.parseFromSource(file)
			analyzeAST(ast).result.foreach(System.out.println)
		} catch {
			case e:java.io.IOException => System.err.println(e)
			case whatever:Exception => throw whatever
		}
	})
}

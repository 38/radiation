package com.github._38.radiation
import com.github._38.radiation.ast._
import com.github._38.radiation.source._

class Result(location:Location);

object Main {
	def analyzeAST(root:Node):(Set[String], Set[Result]) = root match {
		case node:Complex => {
			val (refSyms, result) = (node match {
				case DeclList((_, varlist)) => varlist.foldLeft(List[Node]())((lst, nxt) => nxt match {
					case Decl((_, Some(initval))) => initval :: lst
					case _ => lst
				})
				case FuncExpr((_, _, body)) =>  List(body)
				case _ => node.child			
			}).foldLeft((Set[String](), Set[Result]())) ((res, ch) => {
				val (ref, ret) = analyzeAST(ch)
				(res._1 ++ ref, res._2 ++ ret)
			})
			val (actualRefs, newResults) = node.nodeType match {
				case _:Scope => {
					val Some(scopeInfo:ScopeMetaData) = node.metaData
					System.out.println(scopeInfo.locals &~ refSyms);
					(refSyms &~ scopeInfo.locals, /*(scopeInfo.locals &~ refSyms)*/ Set()) // TODO add more results here
				}
				case Id => {
					val Id(Lexical(what, _)) = node
					(refSyms + what, Set())
				}
				case _ => (refSyms, Set())
			}
			(actualRefs, result ++ newResults)
		}
		case _ => (Set(), Set())
	}
	def main(args:Array[String]) {
		val ast = AST.parseFromSource("../test.js");
		System.out.println(ast);
		analyzeAST(ast);
	}
}

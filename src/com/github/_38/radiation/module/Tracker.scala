package com.github._38.radiation.module

import scala.language.implicitConversions

import com.github._38.radiation.template.Template._
import com.github._38.radiation.ast._
import Helper.fromString

/** @note we should assume that the ast is already processed by closure exposure module
 *        So that we can assume all functions in Tracer Scope has __colsure__ property
 *        That means we use __closure__ object to check the boundary of tracking scope
 */
object Tracker extends ModuleBase {
	private def _modifyLHS(ast:Node, stack:List[Node], localScope:Boolean, readMap:Map[Id, Node], writeMap:Map[Id,Write], expr:Node):(Map[Id,Node], Map[Id,Node], Node)
	{
		//TODO transform the left hand-side, basically this will produce a read-set write-set and a transformed expression
	}
	private def _modifyExpr(ast:Node, stack:List[Node], localScope:Boolean) = {
		//TODO transform this expression to a function with read section, computation section and write section
		// The pattern of this should be
		// function () {
		//    $ReadSection$ //read section can be function apply, that means we modify all arguments by this function and then export them
		//    result = emit($ModifiedExpression$, redSection's tags, code localtion)
		//    //writeSet = result
		//    return result
		// } ()
	}
	private def _modifyStmt(ast:Node, stack:List[Node], localScope:Boolean) = {
		//TODO check if this needs a unpack or export
		// if, switch and for condiftion, while condition, catch condition needs to be unpacked
		
	}
	private def _modifyProgram(ast:Node, stack:List[Node]) = ast match {
		case Program(stmts) => stmts.map(stmt => _modifyStm(stmt, stmt :: stack, false))
	}
	def run(ast:Node):Node = {
		val Program(result) = ast
		new Complex(Program, TrackerJS.header ++ result)
	}
}

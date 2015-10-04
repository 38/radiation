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

	class TrackerException(message:String) extends Exception {
		override toString = "Tracker Exception: " + message
	}
	private def _modifyStatement(ast:Node, stack:List[Node]) = {
		if(!ast.nodeType.isInstanceOf[Statement]) throw new TrackerException("Statement expected bug " + ast.nodeType + " found")
		val nb = new NodeBuilder(ast)
		//TODO modify different types of statements
	}
	private def _modifyProgram(ast:Node, stack:List[Node]) = {
		if(ast.nodeType != Program) throw new TrackerException("Program required but " + ast.nodeType + " found")
		val nb = new NodeBuilder(ast)
		ast.child.foreach(child => {
			nb ++= _modifyStatement(child, child :: stack) 
		})
		nb.toNode
	}
	def run(ast:Node):Node = {
		val Program(result) = ast
		new Complex(Program, TrackerJS.header ++ result)
	}
}

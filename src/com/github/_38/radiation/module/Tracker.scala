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
	/** Export the value to a external function */
	private def _export_return(expr:Node) = new Complex(Call, TrackerJS.emit :: "(".node :: expr :: ",".node :: TrackerJS.caller :: ")".node :: Nil)

	private def _modifyExpression(ast:Node, stack:List[Node]) = {
		if(!ast.nodeType.instanceof[Expression]) throw new TrackerException("CodeBug: Expression expected but " + ast.nodeType + " found")
		val nb = new NodeBuilder(ast)
		//TODO handle 1.funbction 2.assigment 3.basic operators 4.index 5.call 
		// first test code fib
		ast match {
		}
		nb.toNode
	}
	private def _modifyStatement(ast:Node, stack:List[Node]):Node = {
		if(!ast.nodeType.isInstanceOf[Statement]) throw new TrackerException("CodeBug: Statement expected but " + ast.nodeType + " found")
		val nb = new NodeBuilder(ast)
		ast match {
			case Return(Some(expr)) => nb ++= (ast.child(0) :: _export_return(_modifyExpression(expr)) :: ast.child(1) :: Nil)
			case ExprStmt(expr)     => nb ++= (_modifyExpression(expr) :: ast.child(1) :: Nil)
			case _					=> ast
		}
		nb.toNode
	}
	private def _modifyProgram(ast:Node, stack:List[Node]):Node = {
		if(ast.nodeType != Program) throw new TrackerException("CodeBug: Program required but " + ast.nodeType + " found")
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

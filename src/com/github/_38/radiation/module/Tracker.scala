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
		override def toString = "Tracker Exception: " + message
	}
	/** Assert that this node is an complex */
	private def _MustComplex[T](ast:Node, action: Complex => T):T = ast match {
		case what:Complex => action(what)
		case _            => throw new TrackerException("Code Bug: This Node Must be a Complex")
	}
	/** Export the value to a external function */
	private def _export_return(expr:Node) = if(!expr.finalFlag) {
		Node.finalFlag = true
		new Complex(Call, TrackerJS.emit :: "(".node :: expr :: ",".node :: TrackerJS.caller :: ")".node :: Nil)
	} else expr
	/** Unpack the primitive for external use */
	private def _unpack(expr:Node) = if(!expr.finalFlag) {
		Node.finalFlag = true
		new Complex(Call, TrackerJS.unpack :: "(".node :: expr :: ")".node :: Nil)
	} else expr
	/** Modify the expression */
	private def _modifyExpression(ast:Node, stack:List[Node]) = {
		if(!ast.nodeType.isInstanceOf[Expression]) throw new TrackerException("CodeBug: Expression expected but " + ast.nodeType + " found")
		//1. Constant && Function
		//2. Assignement
		//3. Others
		
	}
	private def _modifyStatement(ast:Node, stack:List[Node]):Node = _MustComplex[Node](ast, ast => {
		    if(!ast.nodeType.isInstanceOf[Statement]) throw new TrackerException("CodeBug: Statement expected but " + ast.nodeType + " found")
		    var exportSet = Set[Int]()
		    var unpackSet = Set[Int]()
		    ast.nodeType match {
			    case what:ConditionalControlFlow => unpackSet = Set(what.testExpr) /* All the test expression should be unpacked */
			    case Return | Throw              => exportSet = Set(1)             /* All throw and return expression should be exported */
			    case _							 => ()
		    }
		    val nb = new NodeBuilder(ast)
		    var idx = 0;
		    ast.child.foreach(child => {
			    val resultNode = child.nodeType match {
				    case _:Expression => {
					    val modified = _modifyExpression(child, child :: stack)
					    if(exportSet contains idx) _export_return(modified)
					    else if(unpackSet contains idx) _unpack(modified)
					    else modified
				    }
				    case _:Statement => _modifyStatement(child, child :: stack)
				    case _			 => child
			    }
			    nb append resultNode
			    idx = idx + 1
		    })
		    nb.toNode
	})
	private def _modifyProgram(ast:Node, stack:List[Node]):Node = _MustComplex[Node](ast, ast => {
		if(ast.nodeType != Program) throw new TrackerException("CodeBug: Program required but " + ast.nodeType + " found")
		val nb = new NodeBuilder(ast)
		ast.child.foreach(child => {
			nb append _modifyStatement(child, child :: stack)
		})
		nb.toNode
	})
	def run(ast:Node):Node = {
		val Program(result) = _modifyProgram(ast, Nil)
		new Complex(Program, TrackerJS.header ++ result)
	}
}

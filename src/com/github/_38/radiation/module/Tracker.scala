package com.github._38.radiation.module

import scala.language.implicitConversions

import com.github._38.radiation.template.Template._
import com.github._38.radiation.ast._
import com.github._38.radiation.module.TrackerJS
import Helper.fromString

/** @note we should assume that the ast is already processed by closure exposure module
 *        So that we can assume all functions in Tracer Scope has __colsure__ property
 *        That means we use __closure__ object to check the boundary of tracking scope
 */
object Tracker extends ModuleBase {
	/*
	private def traverse(root:Node, parentType:NodeType):Unit = root match {
	    case node:Complex => {
	        if(node.nodeType.isInstanceOf[Expression] && (!node.finalFlag) && (!parentType.isInstanceOf[Expression])) {
	            System.out.println(node.nodeType + "\t" + node.finalFlag + "\t" + node.targetCode)
            }
	        node.child.foreach(ch => traverse(ch, node.nodeType))
	        //TODO create new childnode if needed
        }
	    case _ => ()
    }*/
	def run(ast:Node):Node = {
		val Program(result) = ast
		new Complex(Program, TrackerJS.header ++ result)
	}
}

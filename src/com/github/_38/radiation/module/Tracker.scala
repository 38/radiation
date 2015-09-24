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
	val header = """
	/* if the global is not initliazed, initialize it */
	try {
		$_$t
	} catch (e) {
		$_$t = {
			/* record the value creation event */
			log: function(id, value, deps, location) {
				console.debug(id + "\t" + value + "\t" + deps + "\t" + location)
			},
			/* get next unassigned id */
			next_id: function () {
				var next_id = 0
				return function () {
					return next_id ++;
				}
			} (),
			/* pack the value to tracked value */
			pack: function(value, deps, location) {
				/* assign a new id to the value */
				var id = $_$t.next_id()
				/* the actual dependency array
				 * TODO: in fact this is only useful when we are tracking back, so we do not keep it
				 *       in the actual tracked value strcture. So do we still need to construct the
				 *       dependency array (or recording it on the fly) ?
				 */
				var dp = []
				var ndp = deps.length
				for(var i = 0; i < ndp; i ++)
					dp.push(deps.id)
				/* record the creation event */
				$_$t.log(id, value, deps, location)
				/* create the actual object */
				return {
					__$packed$__: true,
					id:    id,
					value: value
				}
			},
			/* unpack the value if needed */
			unpack: function(value) {
				if(value.__$packed$__ == true) return value.value
				return value
			},
			/* get the return value of the expression */
		   ret: function (tracked, func) {
			   if(func.caller === undefined || func.caller.__closure__ == undefined)
				   return $_$t.unpack(tracked)
			   return tracked
		   },
		   /* make an argument */
		   getarg: function(arg, func) {
		 	   if(func.caller === undefined || func.caller.__closure__ == undefined) return unpack(arg)
			   return arg
		   }
		}
	}
""".js
	def traverse(root:Node, parentType:NodeType):Unit = root match {
		case node:Complex => {
			if(node.nodeType.isInstanceOf[Expression] && (!node.finalFlag) && (!parentType.isInstanceOf[Expression])) {
				System.out.println(node.nodeType + "\t" + node.finalFlag + "\t" + node.targetCode)
			}
			node.child.foreach(ch => traverse(ch, node.nodeType))
			//TODO create new childnode if needed
		}
		case _ => ()
	}
	def run(ast:Node):Node = {
		traverse(ast,Empty)
		ast
	}
}

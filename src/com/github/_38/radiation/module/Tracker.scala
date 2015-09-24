package com.github._38.radiation.module

import com.github._38.radiation.ast._

/** @note we should assume that the ast is already processed by closure exposure module
 *        So that we can assume all functions in Tracer Scope has __colsure__ property */
object Tracker extends ModuleBase {
	def run(ast:Node):Node = ast
}

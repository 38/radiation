package com.github._38.radiation.module

import com.github._38.radiation.ast.Node

/** The base class of a radiation module, a code transformer */
trait ModuleBase {
	def run(ast:Node):Node
}

package com.github._38.radiation.module

import scala.language.implicitConversions

import com.github._38.radiation.template.Template._
import com.github._38.radiation.ast._
import Helper.fromString

object Closure extends ModuleBase {
	val funcDef = """$$0$$;  /* The function declearation */
					 $$1$$ = _$_$($$2$$,  /* The index */
								  $$1$$,  /* Put the name here */
								  $$3$$   /* The closure object */ );""".t
	
	val funcExp = """_$_$($$0$$ /* The index */,
						 $$1$$ /* The function */ ,
						 $$2$$ /* the Closure object */)""".e
	
	val header  = """try
					{
					   __$_closure_dict__;
					}
					catch(e)
					{
					   __$_closure_dict__ = {};
					}
					function _$_$(id, func, closure)
					{
					   func.__closure__  = closure;
					   if(__$_closure_dict__[id] === undefined)
					   {
						   __$_closure_dict__[id] = [];
					   }
					   func.__closure__.id = __$_closure_dict__[id].length;
					   func.__closure__.func = id;
					   __$_closure_dict__[id].push(func);
					   return func;
					}""".js
	
	val getter  ="""function () {
						return $$0$$;  /* this is the variable we care about */
					}""".e
	var functionIndex = -1
	def getClosure(root:Node, localSymbol:Set[String]) = {
		val symbolRef = mutable.Set[String]()
		def getActiveSymbol(node:Node) {
			node match {
				case Id(Lexical(what, _)) => closure += what
				case complex:Complex      => complex.child.foreach(child => getActiveSymbol(child))
				case _					  => ()
			}
		}
		getActiveSymbol(root)
		var buffer = mutable.ListBuffer[Node]()
		var first = true
		for(symbol <- localSymbol) {
			if(symbolRef contains symbol) {
				val getterName = new Complex(Id, symbol:Lexical)
				if(first) buffer append "{"
				else buffer append ","
				buffer.append(new Complex(::: ,  getterName :: (":":Lexical) :: getter.render(getterName)))
				first = false
			}
		}
		if(first) buffer append "{"
		buffer append "}"
		new Complex(Json, buffer.toList)
	}
	def changeFunction(root:Node, localSymbol:Set[String]) = {
		/* TODO */
	}
}

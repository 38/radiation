import scala.language.implicitConversions
import com.github._38.radiation.ast
import com.github._38.radiation.template._
package com.github._38.radiation.modules.closure {
	object ClosureExposure {
		import Template._
		import ast.{Node, End, Patch, Program, Nop, Bundle, FuncDef, Num, Id}
		val funcDef = """$$0$$;  /* The function declearation */
                         $$1$$ = _$_$($$2$$,  /* The index */
                                      $$1$$,  /* Put the name here */
                                      $$3$$   /* The closure object */
                         );""".t
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
		var functionIdx = 0
		def visitor(node:Node):Node = node match {
			case End(Program(_, _)) => Patch(0, header, 0)   /* Add the header */
			case FuncDef(Some(name), args, body, locals) => {
				functionIdx += 1
                /* TODO figure out the correct clcosure */
				Bundle(funcDef.render(node, name, Num(functionIdx.toString), Num("0")))  /* Only a test */
			}
			case _ => node
		}
		
		def main(args:Array[String]) {
			val program = ast.ASTParser.fromString("function x(x,y){return x + y;}")
			
			val newprogram = program traverse visitor
			
			System.out.println(newprogram.targetCode)
			
		}
	}
}


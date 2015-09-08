import scala.language.implicitConversions
import scala.collection.mutable

import com.github._38.radiation.ast
import com.github._38.radiation.template._
import com.github._38.radiation.modules.ASTModule

package com.github._38.radiation.modules {
	object ClosureExposure extends ASTModule{
		import Template._
		import ast.{Node, End, Patch, Program, Nop, Bundle, FuncDef, FuncExp , Num, Id, Dict, :::, LocalScope, PE}
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
		val getter  ="""function () {
                            return $$0$$;  /* this is the variable we care about */
                        }""".e
		var functionIdx = -1   //This makes the index start with 0
		def getClosureOjbect(root:Node) = {
			val result = mutable.Set[String]()
			def activeSymbol(node:Node):Node = node match {
				case Id(name)       => result += name; node
				case _              => node
			}
			val closure = (Set[String]() /: Node.stack.tail.filter(_.isInstanceOf[LocalScope]).map(_.asInstanceOf[LocalScope].localSymbols))(_ ++ _)
			root traverse activeSymbol
			val filtered = closure filter (result contains _)
			Dict(filtered.toList map (name => :::(Id("get_" + name), getter.render(Id(name)))))
		}
		def visitor(node:Node):Node = node match {
			case End(Program(_, _)) => Patch(0, header, 0)   // Add the header
			case f @ FuncDef(Some(name), _, _, _) => {
				functionIdx += 1
				Bundle(funcDef.render(f.asExpr, name, Num(functionIdx.toString), getClosureOjbect(f)))
			}
			case f:FuncExp => {
				functionIdx += 1
				funcExp.render(Num(functionIdx.toString), f, getClosureOjbect(node))
			}
			case _ => node
		}
		
		def run(program:Node) = program traverse visitor 
	}
}


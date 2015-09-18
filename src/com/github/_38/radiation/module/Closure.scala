package com.github._38.radiation.module

import scala.language.implicitConversions
import scala.collection.mutable.{Set => MutableSet, ListBuffer}

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
		val symbolRef = MutableSet[String]()
		def getActiveSymbol(node:Node) {
			node match {
				case Id(Lexical(what, _)) => symbolRef += what
				case complex:Complex      => complex.child.foreach(child => getActiveSymbol(child))
				case _                    => ()
			}
		}
		getActiveSymbol(root)
		var buffer = ListBuffer[Node]()
		var first = true
		for(symbol <- localSymbol) {
			if(symbolRef contains symbol) {
				val getterName = new Complex(Id, (symbol:Lexical) :: Nil)
				if(first) buffer append "{"
				else buffer append ","
				buffer.append(new Complex(::: ,  getterName :: (":":Lexical) :: getter.render(getterName) :: Nil))
				first = false
			}
		}
		if(first) buffer append "{"
		buffer append "}"
		new Complex(Json, buffer.toList)
	}
	def changeFunction(root:Node, localSymbols:Set[String], parent:NodeType):List[Node] = root match {
		case node:Complex => {
			def _nextId = {
				functionIndex += 1
				new Complex(Num, (functionIndex.toString:Lexical) :: Nil)
			}
			val nb = NodeBuilder(node)
			val currentLocalSymbols = node.nodeType match {
				case _:LocalScope => {
					val Some(ScopeMetaData(locals)) = node.metaData
					localSymbols ++ locals
				}
				case _ => localSymbols
			}
			node.child.foreach(child => nb ++= changeFunction(child, currentLocalSymbols, node.nodeType))
			val new_node = nb.toNode
			new_node match {
				case ExprStmt(f @ FuncExpr(Some(name), _, _))       => funcDef.render(f, name, _nextId, getClosure(node, localSymbols))
				case f @ FuncExpr(_, _, _) if parent != ExprStmt    => funcExp.render(_nextId, f, getClosure(node, localSymbols)) :: Nil
				case _                                              => new_node :: Nil
			}
		}
		case _ => root :: Nil
	}
	def run(program:Node) = {
		val Program(result) :: _ = changeFunction(program, Set(), Empty)
		new Complex(Program, header ++ result)
	}
}

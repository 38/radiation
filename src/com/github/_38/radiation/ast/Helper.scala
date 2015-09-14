package com.github._38.radiation.ast

import org.mozilla.javascript.{ast => RhinoAST}
import com.github._38.radiation.source.{Location, NotInSource, InSource}
import RhinoAST.AstNode

import scala.collection.JavaConverters._

import scala.language.implicitConversions

/** The helpers */
object Helper {
	/** the possible ending of a javascript id */ 
    private val _idEndings = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq('_', '$')).toSet
    /** insert the white space between two lexical token if needed 
     *  @param last the last charecter of previous token
     *  @param next the first charecter of next token
     *  @return the string that has zero or one white space
     */
    private def _whiteSpace(last:Option[Char], next:Char) = last match {
        case Some(last) => if((_idEndings contains last) && (_idEndings contains next)) " " else ""
        case None          => ""
    }
    /** concatenate a list of node to a minimized syntactical correct string, adding white space between tokens if needed
     *  @param what what to combine
     *  @return the minimized javascript source that contains the list of ast node */
    def concat(what:List[Node]) = {
        var result = ""
        var last:Option[Char] = None
        for(str <- what map (_.targetCode)) 
            if(str != "") {
                result = result + _whiteSpace(last, str(0)) + str
                last = Some(str.last)
            }
        result
    }
    /** concatenate the list of node, but ignore the result only get the length
     *  @param what what to combine
     *  @return the length of the javascript source 
     *  @note this is semantically equivalent to concat(what).length, but its faster */
    def concatLength(what:List[Node]) = {
        var result = 0
        var last:Option[Char] = None
        for(node <- what) 
            if(node.targetCodeLength > 0) {
                result = result + _whiteSpace(last, node.firstChar).length + node.targetCodeLength
                last = Some(node.lastChar)
            }
        result
    }
	
    implicit def fromString(s:String) = new Lexical(s, NotInSource)
}

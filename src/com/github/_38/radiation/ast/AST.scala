package com.github._38.radiation.ast

import org.mozilla.javascript.ast._
import com.github._38.radiation.source.{Location}
/** The unified node type
 *  @note this means we do not have subclass of node anymore. 
 *        The design decision was made because we need to modify the 
 *        AST after it has been created. 
 *        If we have different inherated class for different node,
 *        It's difficult to change it
 */
class Node(val type:NodeType) {

}

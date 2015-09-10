package com.github._38.radiation.ast

import org.mozilla.javascript.ast._
import com.github._38.radiation.source.{Location}
/** The notation for identifying the node type */
trait NodeType;
object NodeType {
    /* How node data stored */
    /** Fixed Length Node means the node has fixed number of child */
    trait F extends NodeType;
    /** Variable Length Node */
    trait V extends NodeType;

    /* Define the node type group */
    /** The group of node types that represents Expression */
    trait Expression;
    /** The group of node types that represent a Statement */
    trait Statement;

    object Lexical extends F;
    object Block extends F with Statement;
    object Break extends F with Statement;
    object StatementList extends V;

}
/** The unified node type
 *  @note this means we do not have subclass of node anymore. 
 *        The design decision was made because we need to modify the 
 *        AST after it has been created. 
 *        If we have different inherated class for different node,
 *        It's difficult to change it
 */
abstract class Node(val type:NodeType);

/** The node that is the combination of other nodes */
class Combination(type:NodeType, val children: List[Node]) extends Node(type);

/** The node that represents a lexical token */
class Lexical(val what:String, val where:Location) extends Node(NodeType.Lexical) ;

package com.github._38.radiation.ast

import org.mozilla.javascript.ast.{AstNode => RhinoAstNode}
import org.mozilla.javascript.{Parser, TokenLocation}
import com.github._38.radiation.source.Location

/** The base trait of the Node tag
 *  @note There's 3 categories of tags, VariableLength, FxiedLength and Category
 */
trait NodeTag {
    /** The tagValue use to check the type comptibility */
    val tagValue:Int
    /** Check if the two node type are compitible */
    def :> (that: NodeTag):Boolean
    /** Concatenate two tags */
    def -(that: NodeTag) = that match {
        case c:Complex => new Complex(NodeRule.tagIdx, c.what ++ c.what)
        case _         => new Complex(NodeRule.tagIdx, this ::  that :: List())
    }
    def =< (that:Category):NodeTag;
}

/** The tag for a Category, a category is not a real rule but describe
 *  Some properties of the rule
 */
class Category(_tagValue:Int) extends NodeTag {
    val tagValue = _tagValue
    def :> (that: NodeTag) = (that.tagValue & tagValue) == tagValue
    /** mark this Category is an subcategory of that */
    def =< (that: Category) = new Category(tagValue | that.tagValue)
    override def toString() = "Category Id 0x%x" format _tagValue
}
object Category {
    /* The mask code for the Category */
    val CategoryMask = 0xffff
    private var _nextFreeBit = 1
    def category = {
        val ret = _nextFreeBit
        _nextFreeBit = _nextFreeBit << 1
        new Category(ret)
    }
}

/** The trait for actual Ast Rule */
trait NodeRule extends NodeTag {
    /** @note Because the only case that two rule are equal are they are really the same one */
    def :> (that: NodeTag) = tagValue == that.tagValue
}
object NodeRule {
    implicit def stringToRuleConversion(s:String) = new ConstantPrimitive(0, s)  // 0 is reserved for all the constant tokens
    var tagIdx = 0
    class RuleHelper(val idx:Int) {
        def >> (x:NodeTag) = x
    }
    def rule = {
        tagIdx = tagIdx + 1
        new RuleHelper(tagIdx)
    }
}
/** Indicates this is a tag that not a complex tag */
trait SimpleTag extends NodeRule ;

trait Getter {
    /** Create an Node from rhino AST with current rule */
    //def apply(rhino_node:RhinoAstNode):Node;
};

/** The tag for variable length nodes */
class VariableLength(_tagVal:Int, _childTag:NodeTag) extends NodeRule with SimpleTag{
    import java.util.List
    val tagValue = _tagVal
    val childTag = _childTag
    class VariableLengthGetter[T](getter: RhinoAstNode => List[T]) extends VariableLength(_tagVal, _childTag) with Getter{
        //def apply(rhino_node:RhinoAstNode):Node = Node(this, List()) // TODO
    }
    def <[T](how: RhinoAstNode => java.util.List[T]) = 
        new VariableLengthGetter[T](how)
    def =<(c:Category) = new VariableLength(tagValue | c.tagValue, _childTag)
}

/** The tag for fixed length nodes 
 *  @note for the fixed length node, there's 3 type of tags <br/>
 *        PrimitiveVariable, which is the lexical token know from Rhino AST
 *        PrimitiveConstant, which is the lexical token which is definitely this <br/>
 *        ComplexNode, which is the node hold fixed number of primitives <br/>
 */
trait FixedLength extends NodeRule

/** The tag trait for Primitives */
trait Primitive extends FixedLength with SimpleTag;

/** The tag for variable primitives */
class VariablePrimitive(_tagValue:Int) extends FixedLength{
    val tagValue = _tagValue
    class VariablePrimitiveGetter(val getter: RhinoAstNode => String) extends VariablePrimitive(_tagValue) with Getter {
        //def apply(rhino_node:RhinoAstNode):Node = Node(this, List(getter(rhino_node)))
    }
    def <(how: RhinoAstNode => String) = new VariablePrimitiveGetter(how)
    def =<(c:Category) = new VariablePrimitive(tagValue | c.tagValue)
}

/** The tag for constant  primitives */
class ConstantPrimitive(_tagValue:Int, what:String) extends FixedLength with Getter{
    val tagValue = _tagValue
    //def apply():Node = Node(this, what)
    def =<(c:Category) = new ConstantPrimitive(tagValue | c.tagValue, what)
}

/** The tag trait for ComplexNode */
class Complex(_tagValue:Int, val what:List[NodeTag]) extends FixedLength with Getter {
    val tagValue = _tagValue
    def =<(c:Category) = new Complex(tagValue | c.tagValue, what)
}


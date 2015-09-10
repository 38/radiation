package com.github._38.radiation.ast

import org.mozilla.javascript.ast.{AstNode => RhinoAstNode}
import org.mozilla.javascript.{Parser, TokenLocation}
import com.github._38.radiation.source.Location
import com.github._38.radiation.ast.Node

trait Node;
/** The base trait of the Node tag
 *  @note There's 3 categories of tags, VariableLength, FxiedLength and Category
 */
trait NodeTag {
    /** The tagValue use to check the type comptibility */
    val tagValue:Int
    /** Check if the two node type are compitible */
    def :> (that: NodeTag)
}

/** The tag for a Category, a category is not a real rule but describe
 *  Some properties of the rule
 */
class Category(_tagValue:Int) extends NodeTag {
    val tagValue = tagValue
    def :> (that: NodeTag) = (that.tagValue & tagValue) == tagValue
    /** mark this Category is an subcategory of that */
    def =< (that: Category) = new Category(_tagValue | that._tagValue)
}
object Category {
    /* The mask code for the Category */
    val CategoryMask = 0x3ff
    private var _nextFreeBit = 1
    def category = {
        val ret = _nextFreeBit
        _nextFreeBit = _nextFreeBit << 1
        new Category(ret)
    }
}

/** The trait for actual Ast Rule */
trait NodeRule extends NodeTag;

trait Getter {
    /** Create an Node from rhino AST with current rule */
    def apply(rhino_node:RhinoAstNode):Node;
};

/** The tag for variable length nodes */
class VariableLength(_tagVal:Int, _childTag:NodeTag) extends Varaible {
    import java.util.List
    val tagValue = _tagVal
    val childTag = _childTag
    class VariableLengthGetter[T](getter: RhinoAstNode => List[T]) extends VariableLength(_tagVal, _childTag) with Getter{
        def apply(rhino_node:RhinoAstNode):Node = Node(this, List()) // TODO
    }
    def <[T](how: RhinoAstNode => java.util.List[T]) = 
        new VariableLengthGetter[T](getter) 
}

/** The tag for fixed length nodes 
 *  @note for the fixed length node, there's 3 type of tags <br/>
 *        PrimitiveVariable, which is the lexical token know from Rhino AST
 *        PrimitiveConstant, which is the lexical token which is definitely this <br/>
 *        ComplexNode, which is the node hold fixed number of primitives <br/>
 */
trait FixedLength extends NodeRule

/** The tag trait for Primitives */
trait Primitive extends FixedLength

/** The tag trait for ComplexNode */
trait Complex extends FixedLength

/** The tag for variable primitives */
class VariablePrimitive(_tagValue:Int) extends FixedLength with Variable {
    val tagValue = _tagVal
    class VariablePrimitiveGetter(getter: RhinoAstNode => Node)
}

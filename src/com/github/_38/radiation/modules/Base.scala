import com.github._38.radiation.ast.Node
package com.github._38.radiation.modules {
    trait ASTModule {
        def run(ast:Node):Node
    }
}

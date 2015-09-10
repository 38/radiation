package com.github._38.radiation.ast;
object Rules {
    import Category._
    import NodeRule._
    /* Define the categories */
    val Expression       = category
    val Statement        = category
    val ListMember       = category
    val ForLoopInit      = category
    val Property         = category
    val Function         = category
    val Scope            = category
    val ControlFlow      = category =< Statement
    val Loop             = category =< ControlFlow
    val ForLoop          = category =< Loop
    val LocalScope       = category =< Scope
    val Constant         = category =< Expression

    /* Define the rules */
    val Break            = rule >>  "break" - ";" =< ControlFlow

}

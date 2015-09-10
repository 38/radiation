package com.github._38.radiation.source;
/** Describe the location in the source code */
trait Location;
/** The location of the source code is not defined */
case object Undefined extends Location;
/** Case describe &lt;File, Line, Column&gt; */
case class SourceLocation(file:String, line:Int, column:Int) {
    override def toString() = "%s@%d:%d".format(file, line, column)
}


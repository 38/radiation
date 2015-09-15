package com.github._38.radiation.source;
/** Describe the location in the source code */
trait Location;
/** The location of the source code is not defined */
case object NotInSource extends Location;
/** Case describe &lt;File, Line, Column&gt; */
case class InSource(file:String, line:Int, column:Int) extends Location{
	override def toString() = "%s@%d:%d".format(file, line, column)
    def -(that:InSource) = InSource(file, line - that.line, column - that.column)
}


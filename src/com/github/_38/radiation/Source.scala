
/** Source code related utils */
package com.github._38.radiation.source {
	/** Describe a Location in a source code
	 *  @param line Line number in the source code
	 *  @param column the column number, the offset can be out of range, so we need use normalize function
	 */
	class KnownLocation(val line:Int, val column:Int) extends Location{
		/** normalize means figure out the undefined line number, and make sure line offset
		 *  is in the range of the line
		 *  @param lines the list of offset of the beginning of each line
		 *  @return the newly created location
		 */
		def normalize(lines:List[Int]) = {
			val base = lines(line)
			val offset = column
			var (l,r) = (0, lines.size - line)
			while(r - l > 1) {
				val m = (l + r) / 2
				if(lines(m + line) - base <= offset) l = m
				else r = m
			}
			Location(l + line, column + base - lines(l))
		}
		/** Get the ralative diff of two locations
		 *  @param that the second operand
		 *  @return the newly created location
		 */
		def -(that:KnownLocation) = Location(this.line - that.line, this.column - that.column)

		override def toString = "<" + line + ":" + column + ">"
	}
	trait Location;
	object Location {
		def apply(line:Int, column:Int) = new KnownLocation(line, column);
		def unapply(loc:Location):Option[(Int,Int)] = loc match {
			case k:KnownLocation => Some((k.line, k.column))
			case _               => None
		}
	}
	trait Undefined extends Location;
	object Undefined extends Undefined{
		override def toString = "<UDL>"
	}
	
}

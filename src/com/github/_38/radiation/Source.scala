
/** Source code related utils */
package com.github._38.radiation.source {
	/** Describe a Location in a source code
	 *  @param line Line number in the source code
	 *  @param column the column number, the offset can be out of range, so we need use normalize function
	 */
	case class Location(val line:Int, val column:Int) {
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
        def -(that:Location) = Location(this.line - that.line, this.column - that.column)
	}
	
}

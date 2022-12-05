case class Assignment(first: Range, second: Range):
  def hasOverlap: Boolean = first.intersect(second).length == second.length || second.intersect(first).length == first.length
val toRange: String => Range = (part) => part.split("-") match
  case Array(start, end) => start.toInt to end.toInt
val mapLineToAssignment: String => Assignment = (line) => line.split(",") match
  case Array(first, second) => Assignment(toRange(first), toRange(second))
scala.io.Source.fromFile("2022/04/04.txt","utf-8")
.getLines()
.map(mapLineToAssignment)
.count(_.hasOverlap)
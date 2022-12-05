case class Crate(items: List[Char]):
  def moveFrom(count: Int): (Crate, List[Char]) = 
    val (remain, chars) = items.splitAt(items.length - count)
    (Crate(remain), chars)
  def moveTo(newItems: List[Char]): Crate = Crate(items ++ newItems)
val operationRegex = """^move (\d+) from (\d+) to (\d+)$""".r
case class Operation(from: Int, to: Int, count: Int)
val lines = scala.io.Source.fromFile("2022/05/05.txt", "utf-8")
.getLines()
.toList
val parseOperations = (lineByLine: List[String]) => lineByLine.map {
  case operationRegex(count, from, to) => Operation(from.toInt, to.toInt, count.toInt)
}
val parseCargo = (lineByLine: List[String]) => 
  lineByLine.reverse.foldLeft(Map.empty[Int, Crate]) {
  case (mapping, line) => line.grouped(4)
  .zipWithIndex.filter(_._1.filter(_.isLetter).nonEmpty).foldLeft(mapping) {
    case (mapp, (chr, index)) if mapp.contains(index + 1) => mapp.updatedWith(index + 1)(_.map(_.moveTo(List(chr.filter(_.isLetter).head))))
    case (mapp, (chr, index)) => mapp + ((index + 1) -> Crate(List(chr.filter(_.isLetter).head)))
  }
}
val emptyLineNumber = lines.indexOf("")
val (crateMap, operations) = lines.splitAt(emptyLineNumber) match 
  case (cargo: List[String], operations: List[String]) => (parseCargo(cargo), parseOperations(operations.filter(_.nonEmpty)))
val applyOp = (mapping: Map[Int, Crate], op: Operation) => {
  op match 
    case Operation(from, to, count) => 
      val fromCrate = mapping(from)
      val toCrate = mapping(to)
      val (newCrate, chrs) = fromCrate.moveFrom(count)
      mapping ++ Map(from -> newCrate, to -> toCrate.moveTo(chrs))
}
val applyOps: (Map[Int, Crate], List[Operation]) => Map[Int, Crate] = (mapping: Map[Int, Crate], remaining: List[Operation]) =>
  remaining match 
    case Nil => mapping
    case head :: tail =>
      val newMap = applyOp(mapping, head)
      applyOps(newMap, tail)
applyOps(crateMap, operations).toList.sortBy(_._1).map {
  case (index, item) => item.items.last
}.mkString("")
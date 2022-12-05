
val applyPriority = (range: scala.collection.immutable.NumericRange.Inclusive[Char], start: Int) => range.zipWithIndex.map((chr, index) => (chr -> (index + start))).toMap
val priorityMapping = applyPriority('a'.to('z'), 1) ++ applyPriority('A'.to('Z'), 27)
given Ordering[Char] = Ordering.by(chr => priorityMapping.getOrElse(chr, -1))
case class Rucksack(first: String, second: String):
  def highestPair = first.sorted.reverse.find(second.contains)
object Rucksack:
  def fromLine(line: String): Rucksack = line.length match
    case size if size % 2 == 0 => line.splitAt(size / 2) match
      case (first, second) => Rucksack(first, second)
    case size => line.splitAt((size + 1) / 2) match
      case (first, second) => Rucksack(first, second)
end Rucksack
end Rucksack
scala.io.Source.fromFile("2022/03/03.txt","utf-8")
.getLines()
.map(Rucksack.fromLine)
.map(_.highestPair.map(priorityMapping).getOrElse(0))
.sum
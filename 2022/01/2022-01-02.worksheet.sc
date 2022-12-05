given Ordering[(Int, Long)] = Ordering.by(_._2)
scala.io.Source.fromFile("2022/01/01.txt","utf-8")
.getLines()
.toList
.foldLeft(Map.apply((0 -> 0L))) {
  case (mapping, line) if line.trim().isEmpty() => mapping + (mapping.size -> 0L)
  case (mapping, line) => mapping.updatedWith(mapping.size - 1)(entry => entry.map(value => value + line.toLong))
}
.toList
.sorted
.reverse
.take(3)
.map(_._2)
.sum match
  case coleries => println(s"top 3 elves carrys $coleries coleries")

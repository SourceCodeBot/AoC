val (start, rest) = scala.io.Source.fromFile("2022/06/06.txt", "utf-8")
.getLines()
.mkString("")
.splitAt(4)
val isUnqiue = (str: String) => str.toCharArray().toSet.size == str.size
val matchFn: (String, String) => Option[Int]= (took: String, rest: String) =>
  if (isUnqiue(took.takeRight(4)))
    Some(took.length())
  else if (rest.isEmpty()) 
    None
  else
    matchFn(s"$took${rest(0)}", rest.drop(1))
val result = matchFn(start, rest)
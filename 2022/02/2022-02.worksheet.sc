
enum Feind(val chr: Char):
  case Rock     extends Feind('A')
  case Paper    extends Feind('B')
  case Scissors extends Feind('C')

enum Response(val chr: Char):
  case Rock     extends Response('X')
  case Paper    extends Response('Y')
  case Scissors extends Response('Z')

enum MatchEnd:
  case Win, Lose, Draw

val getPointFor: PartialFunction[Feind | Response, Int] = {
  case Feind.Paper        => 2
  case Feind.Rock         => 1
  case Feind.Scissors     => 3
  case Response.Paper     => 2
  case Response.Rock      => 1
  case Response.Scissors  => 3
}

val getPointOn: PartialFunction[MatchEnd, Int] = {
  case MatchEnd.Draw  => 3
  case MatchEnd.Lose  => 0
  case MatchEnd.Win   => 6
}

val getMatchEnd: PartialFunction[(Feind, Response), MatchEnd] = (turn: (Feind, Response)) => turn match {
  case (Feind.Rock, Response.Rock)=> MatchEnd.Draw
  case (Feind.Scissors, Response.Scissors) => MatchEnd.Draw
  case (Feind.Paper, Response.Paper) => MatchEnd.Draw
  case (Feind.Rock, Response.Scissors)  => MatchEnd.Lose
  case (Feind.Rock, Response.Paper)     => MatchEnd.Win
  case (Feind.Scissors, Response.Rock)  => MatchEnd.Win
  case (Feind.Scissors, Response.Paper) => MatchEnd.Lose
  case (Feind.Paper, Response.Scissors) => MatchEnd.Win
  case (Feind.Paper, Response.Rock)     => MatchEnd.Lose
}

val calculateMatchEnd = (turn: (Feind, Response)) => turn match {
  case (feind, response) => 
    getPointOn(
      getMatchEnd((feind, response))
    ) + getPointFor(response)
}

val parseLine = (str: String) => str.trim().split(" ") match
  case Array(feind, response) => (Feind.values.find(_.chr == feind.head).get, Response.values.find(_.chr == response.head).get)

val result = scala.io.Source.fromFile("2022/02/02.txt","utf-8")
.getLines()
.toList
.map(parseLine)
.map(calculateMatchEnd)
.sum

println(s"total score: $result")

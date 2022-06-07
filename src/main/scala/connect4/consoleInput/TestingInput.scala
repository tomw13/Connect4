package connect4.consoleInput

import scala.util.Random

class TestingInput(moves: List[Int]) extends ConsoleInput {
  var moveSet: List[Int] = moves
  override def getUserInput: Int = {
    val input = moveSet.head
    moveSet = moveSet.tail
    input
  }
}








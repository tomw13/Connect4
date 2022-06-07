package connect4.consoleInput

import scala.util.Random

class TestingInput extends ConsoleInput {
  override def getUserInput: Int = Random.between(0, 7)
}








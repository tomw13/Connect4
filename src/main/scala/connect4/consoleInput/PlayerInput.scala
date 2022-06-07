package connect4.consoleInput

class PlayerInput extends ConsoleInput {
  override def getUserInput: Int = try {
    val input = scala.io.StdIn.readInt() - 1
    if (input < 0 || input >= 7) then
      throw new ArrayIndexOutOfBoundsException
    else input
  } catch {
    case e: Throwable => {
      println("Please enter an integer between 1 and 7")
      getUserInput
    }
  }
}






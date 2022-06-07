package connect4.game

import connect4.board.Board
import connect4.player.Player
import connect4.consoleInput.{ConsoleInput, PlayerInput}

import scala.annotation.tailrec

case class Game(board: Board, player1: Player, player2: Player, gameOver: Boolean, consoleInput: ConsoleInput) {
  // chooses the first player randomly and returns said player
  def chooseFirstPlayer(): Player = if math.random < 0.5 then player1 else player2

  // a list of the two players, used to swap turns in later methods
  val playerList: List[Player] = List(player1, player2)

  /**
   *  @param player: Player
   *  @return
   *
   *   takes a player input and
   *   takes the input from the player
   *   calls dropToken on the board with the player input
   *   calls newMove on the player with the input
   *   calls checkWin on the new board with the new player
   *   returns a new game with the updated player, board and game state
   *
   */

  def takeTurn(player: Player): Game = {
    val col = takeInput(player)
    val newBoard = board.dropToken(col)(player)
    val newPlayer = player.newMove(col)
    val isGameOver = newBoard.checkWin(newPlayer) || newBoard.isFull
    if player == player1 then Game(newBoard, newPlayer, player2, isGameOver, consoleInput)
    else Game(newBoard, player1, newPlayer, isGameOver, consoleInput)
  }

  /**
   *
   * @param player: Player
   * @return
   *
   * takes a player as input and returns the player's inputted column choice
   * wrapped in a try-catch expression with tail recursion to avoid errors
   */

  def takeInput(player: Player): Int = {
    board.print()
    println(s"${player.name}, pick a column (1-${board.grid.length})")
    consoleInput.getUserInput

  }

  /**
   * recursively calls a function to take each players turn one after the other
   * breaks out if the game is over and prints the winner.
   */
  def play(): Game =
    @tailrec
    def auxPlay(player: Player, game: Game): Game =
      val otherPlayer = playerList.filter(_.name != player.name).head
      if game.gameOver then
        board.print()
        if board.isFull then println("Game Over") else println(s"${otherPlayer.name} wins!")
        game
      else auxPlay(otherPlayer, takeTurn(player))

    auxPlay(chooseFirstPlayer(), this)
}

def main(args: Array[String]): Unit = {
  val game = Game(Board(6,7), Player("Player 1", "X", 0), Player("Player 2", "O", 0), false, new PlayerInput)
  game.play()
}

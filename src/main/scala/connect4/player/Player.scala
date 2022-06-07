package connect4.player

object Player {
  def apply(name: String, token: String, lastMove: Int): Player = new Player(name, token, lastMove)
}

case class Player(name: String, token: String, lastMove: Int) {
  def newMove(move: Int): Player = this.copy(lastMove = move)
}

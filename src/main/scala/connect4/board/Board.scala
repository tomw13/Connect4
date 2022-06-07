package connect4.board

import connect4.player.Player

import scala.annotation.tailrec



object Board {
  // apply method allows easy construction of a new Board with custom parameters
  def apply(height: Int, width: Int): Board = new Board(Array.fill(width)(Array.fill(height)("-")))
}

case class Board(grid: Array[Array[String]]) {
  // prints the board in a clear way
  def print(): Unit =
    val formatGrid = grid.clone().transpose :+ Array("7", "6", "5", "4", "3", "2", "1").reverse
    formatGrid.foreach(col => println(s"[${col.mkString}]"))
  
  // takes the desired column and the player dropping and inserts the players token in the correct column
  // returns a new Board with the updated positions
  def dropToken(col: Int)(player: Player): Board =
    @tailrec
    def dropInCol(arr: Array[String], player: Player, pointer: Int): Array[String] =
      // if the column is full, return array
      if (pointer < 0) arr
      else if (arr(pointer) == "-") {
        // if there is an empty space, fill it with token and return
        arr(pointer) = player.token
        arr
      }
      else dropInCol(arr, player, pointer - 1) // move pointer up the stack one and check again
    val updatedGrid = grid.clone()
    updatedGrid(col) = dropInCol(updatedGrid(col), player, updatedGrid(col).length - 1)
    new Board(updatedGrid) // return a new Board with the updated grid

  // for checking wins, finds the start of the leading diagonal given the col and row of a token
  def leadingDiagonalStart(col: Int, row: Int): (Int, Int) =
    @tailrec
    def auxLeadingStart(col: Int, row: Int): (Int, Int) =
      if (col == 0 | row == grid(0).length - 1) (col, row) // if you hit the limit of the grid, return (col, row)
      else auxLeadingStart(col - 1, row + 1) // decrement col, increment row
    auxLeadingStart(col, row)

  // gets an array representation of the leading diagonal given the position of a token
  def getLeadingDiagonal(col: Int, row: Int): Array[String] =
    val (startCol, startRow) = leadingDiagonalStart(col, row)
    @tailrec
    def auxLeadingDiagonal(col: Int, row: Int, accArray: Array[String]): Array[String] =
      if (col >= grid.length | row < 0) accArray
      else auxLeadingDiagonal(col + 1, row - 1, grid(col)(row) +: accArray)
    auxLeadingDiagonal(startCol, startRow, Array[String]())

  // finds the start of the ending diagonal given the col and row of a token
  def endingDiagonalStart(col: Int, row: Int): (Int, Int) =
    @tailrec
    def auxEndingStart(col: Int, row: Int): (Int, Int) =
      if (col == 0 | row == 0) (col, row)
      else auxEndingStart(col - 1, row - 1)
    auxEndingStart(col, row)

  // returns array representation of the ending diagonal given the position of a token
  def getEndingDiagonal(col: Int, row: Int): Array[String] =
    val (startCol, startRow) = endingDiagonalStart(col, row)
    @tailrec
    def auxEndingDiagonal(col: Int, row: Int, accArray: Array[String]): Array[String] =
      if (col >= grid.length | row >= grid(0).length) accArray
      else auxEndingDiagonal(col + 1, row + 1, grid(col)(row) +: accArray)
    auxEndingDiagonal(startCol, startRow, Array[String]())
    
  // checks to see if a given player has won the game
  def checkWin(player: Player): Boolean =
    // checks the column of the players last move
    def checkColumn(col: Int): Boolean = grid(col).mkString.contains(player.token * 4)
    // checks the row of the players last move
    def checkRow(col: Int): Boolean =
      val rowPos: Int = grid(col).indexOf(player.token)
      grid.map(_(rowPos)).mkString.contains(player.token * 4)
      
    // checks the diagonals of the players last move
    def checkDiagonal(col: Int): Boolean =
      val rowPos: Int = grid(col).indexOf(player.token)
      val leadingDiagonal = getLeadingDiagonal(col, rowPos).mkString
      val endingDiagonal = getEndingDiagonal(col, rowPos).mkString
      leadingDiagonal.contains(player.token * 4) || endingDiagonal.contains(player.token * 4)
      
    // returns true if any contain 4 tokens in a row
    checkColumn(player.lastMove) || checkRow(player.lastMove) || checkDiagonal(player.lastMove)
    
  // checks to see if the grid has been filled
  def isFull: Boolean = grid.forall(_.forall(_ != "-"))
}

//def main(args: Array[String]): Unit = {
//  val tom = Player("Tom", "X", 3)
//  val ben = Player("Ben", "O", 2)
//  val test = Board(6, 7)
//  val newBoard = test.dropToken(0)(tom)
//  val secondBoard = newBoard.dropToken(2)(tom)
//  val thirdBoard = secondBoard.dropToken(1)(tom)
//  val fourthBoard = thirdBoard.dropToken(3)(tom)
//  val fifthBoard = fourthBoard.dropToken(1)(tom)
//  fifthBoard.print()
//  println(fifthBoard.checkWin(tom))
//
//}

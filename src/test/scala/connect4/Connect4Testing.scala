package connect4

import connect4.board.Board
import connect4.consoleInput.TestingInput
import connect4.player.Player
import connect4.game.Game

class Connect4Testing extends munit.FunSuite {
  test("player has correct name, token, and last move") {
    val testPlayer = Player("Tom", "X", 0)
    val name = "Tom"
    val token = "X"
    val lastMove = 0
    assertEquals(name, testPlayer.name)
    assertEquals(token, testPlayer.token)
    assertEquals(lastMove, testPlayer.lastMove)
  }

  test("Board is created correctly with right number of rows and columns, and is filled correctly") {
    val test = Board(6, 7)
    assertEquals(test.grid.length, 7)
    assertEquals(test.grid(0).length, 6)
    for {
      array <- test.grid
      elem <- array
    } yield assertEquals(elem, "-")
  }

  test("Token is dropped and stacks accordingly") {
    var test = Board(6,7)
    val player1 = Player("1", "X", 0)
    val player2 = Player("2", "O", 0)
    test = test.dropToken(0)(player1)
    test = test.dropToken(0)(player2)
    assertEquals(test.grid(0)(5), "X")
    assertEquals(test.grid(0)(4), "O")
    assertEquals(test.grid(0)(3), "-")
  }

  test("dropping into a full stack returns the stack") {
    val arr1 = Array("X","X","X","O","O","O")
    val arr2 = Array("O","O","O","X","X","X")
    val testGrid = Board(Array(arr1, arr2, arr1, arr2, arr1, arr2, arr1))
    val player1 = Player("1", "X", 0)
    val droppedToken = testGrid.dropToken(0)(player1)
    assertEquals(testGrid.grid(0) sameElements droppedToken.grid(0), true)
  }

  test("checkWin returns false on non win grid") {
    val arr1 = Array("X","X","X","O","O","O")
    val arr2 = Array("O","O","O","X","X","X")
    val testGrid = Board(Array(arr1, arr2, arr1, arr2, arr1, arr2, arr1))
    val player = Player("test", "X", 0)
    assertEquals(testGrid.checkWin(player), false)
  }

  test("isFull returns true on full grid, and false on empty grid") {
    val arr1 = Array("X","X","X","O","O","O")
    val arr2 = Array("O","O","O","X","X","X")
    val testGrid_1 = Board(Array(arr1, arr2, arr1, arr2, arr1, arr2, arr1))
    val testGrid_2 = Board(6,7)
    assertEquals(testGrid_1.isFull, true)
    assertEquals(testGrid_2.isFull, false)
  }

  test("checkWin returns true on column win grid") {
    val arr1 = Array("X","X","X","X","O","O")
    val arr2 = Array("-","O","O","X","X","X")
    val arr3 = Array.fill(6)("-")
    val testGrid = Board(Array(arr1, arr2, arr3, arr3, arr3, arr3, arr3))
    val player = Player("test", "X", 0)
    assertEquals(testGrid.checkWin(player), true)
  }

  test("checkWin returns true on row win grid") {
    val arr1 = Array("-","-","-","-","O","X")
    val arr2 = Array("-","-","-","-","-","X")
    val arr3 = Array.fill(6)("-")
    val testGrid = Board(Array(arr2, arr1, arr1, arr1, arr3, arr3, arr3))
    val player = Player("test", "X", 0)
    assertEquals(testGrid.checkWin(player), true)
  }

  test("checkWin returns true on diagonal win grid") {
    val arr1 = Array("-","-","-","-","X","O")
    val arr2 = Array("-","-","-","-","-","X")
    val arr4 = Array("-","-","-","X","O","O")
    val arr5 = Array("-","-","X","O","O","O")
    val arr3 = Array.fill(6)("-")
    val testGrid = Board(Array(arr2, arr1, arr4, arr5, arr3, arr3, arr3))
    val player = Player("test", "X", 0)
    assertEquals(testGrid.checkWin(player), true)
  }

  test("checkWin returns false on interrupted win grid") {
    val arr1 = Array("-", "-", "-", "-", "-", "O")
    val arr2 = Array("-", "-", "-", "-", "-", "X")
    val arr3 = Array.fill(6)("-")
    val testGrid = Board(Array(arr2, arr1, arr2, arr2, arr2, arr3, arr3))
    val player = Player("test", "X", 0)
    assertEquals(testGrid.checkWin(player), false)
  }

  test("game takes entered column and places correct token") {
    val game = Game(Board(6,7), Player("tom", "X", 0), Player("jack", "O", 0), false, new TestingInput(List(0)))
    val test = game.takeTurn(game.player1)
    assertEquals(test.board.grid.exists(_(5) == "X"), true)
  }

  test("game ends when board is filled") {
    val arr1 = Array("X","X","X","O","O","O")
    val arr2 = Array("O","O","O","X","X","X")
    val testGrid = Board(Array(arr1, arr2, arr1, arr2, arr1, arr2, arr1))
    val game = Game(testGrid, Player("tom", "X", 0), Player("jack", "O", 0), false, new TestingInput(List(0)))
    val test = game.play()
    assertEquals(test.gameOver, true)
  }

  test("game ends when a player wins") {
    val moveSet: List[Int] = List(0,1,0,1,0,1,0,1)
    val game = Game(Board(6,7), Player("tom", "X", 0), Player("jack", "O", 0), false, new TestingInput(moveSet))
    val test = game.play()
    assertEquals(test.gameOver, true)
  }
}

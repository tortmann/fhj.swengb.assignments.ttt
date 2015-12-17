package fhj.swengb.assignments.ttt.tortmann

import org.junit.Assert._
import org.junit.Test

class TicTacToeTest {

  lazy val allGames: Map[Seq[TMove], TicTacToe] = TicTacToe.mkGames()

  @Test def setOnEmpty(): Unit = {
    val t = TicTacToe().turn(TopCenter, Human)
    assertEquals(8, t.remainingMoves.size)
  }

  @Test def testGameOver(): Unit = {
    assertFalse(TicTacToe().gameOver)
  }

  @Test def testGameOver2(): Unit = {
    val gameOver = TicTacToe(
      Map(
        TopLeft -> Computer,
        TopCenter -> Human,
        TopRight -> Computer,
        MiddleLeft -> Human,
        MiddleCenter -> Computer,
        MiddleRight -> Human,
        BottomLeft -> Computer,
        BottomCenter -> Human,
        BottomRight -> Computer
      )
    ).gameOver
    assertTrue(gameOver)
  }
}

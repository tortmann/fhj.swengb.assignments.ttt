package fhj.swengb.assignments.ttt.tortmann

import scala.collection.{mutable, Set}
import scala.collection.mutable.MutableList


sealed trait TMove {def idx: Int}

case object TopLeft extends TMove {override def idx: Int = 0}

case object TopCenter extends TMove {override def idx: Int = 1}

case object TopRight extends TMove {override def idx: Int = 2}

case object MiddleLeft extends TMove {override def idx: Int = 3}

case object MiddleCenter extends TMove {override def idx: Int = 4}

case object MiddleRight extends TMove {override def idx: Int = 5}

case object BottomLeft extends TMove {override def idx: Int = 6}

case object BottomCenter extends TMove {override def idx: Int = 7}

case object BottomRight extends TMove {override def idx: Int = 8}


sealed trait Player

case object Human extends Player

case object Computer extends Player

object TicTacToe {

  def apply(): TicTacToe = {
    val moveHistory:Map[TMove,Player] = Map.empty[TMove,Player]
    val x = TicTacToe(moveHistory)
    x
  }

  def play(game: TicTacToe, moves: Seq[TMove]): TicTacToe = {
    var player:Player = Human
    for(move <- moves){
      game.turn(move, player)
      if(player.equals(Human))
        player = Computer
      else
        player = Human
    }

    return game
  }

  def mkGames(): Map[Seq[TMove], TicTacToe] = ???

}

case class TicTacToe(moveHistory: Map[TMove, Player], nextPlayer: Player = Human) {

  def asString(): String = {
    var GameBoard: String =
      "|---|---|---|\n" +
      "|   |   |   |\n" +
      "|---|---|---|\n" +
      "|   |   |   |\n" +
      "|---|---|---|\n" +
      "|   |   |   |\n" +
      "|---|---|---|\n"

    val pos = Map(0->16, 1->20, 2->24, 3->44, 4->48, 5->52, 6->72, 7->76, 8->80)

    for((m, p) <- moveHistory) {
      if (p == Human) {
        GameBoard = GameBoard.updated(pos(m.idx), "X").mkString
      }
      else if (p == Computer) {
      GameBoard = GameBoard.updated(pos(m.idx), "O").mkString
    }
    }
    return GameBoard
  }

  val gameOver : Boolean = {
    if(winner != None || moveHistory.size == 9)
      true
    else
      false

  }

  val remainingMoves: Set[TMove] = {
    val possibleMoves: Set[TMove] = Set(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
    for(specificMove <- possibleMoves
        if !moveHistory.contains(specificMove))
          yield specificMove
  }

  def winner: Option[(Player, Set[TMove])] = {
    val winningScenarios = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))
    val allMovesPlayerA: MutableList[Int] = MutableList()
    val allMovesPlayerB: MutableList[Int] = MutableList()

    for (move <- moveHistory) {
      if (move._2.equals(Human)) {
        allMovesPlayerA += move._1.idx
      }
      else {
        allMovesPlayerB += move._1.idx
      }
    }

    for (scenarios <- winningScenarios) {
      if (allMovesPlayerA.contains(scenarios._1) && allMovesPlayerA.contains(scenarios._2) && allMovesPlayerA.contains(scenarios._3)) {
        return Some(Human, moveHistory.keySet)
      }
      else if (allMovesPlayerB.contains(scenarios._1) && allMovesPlayerB.contains(scenarios._2) && allMovesPlayerB.contains(scenarios._3)) {
        return Some(Computer, moveHistory.keySet)
      }
    }
    None
  }

  def turn(nextMove: TMove, p: Player): TicTacToe = {
    if(!moveHistory.contains(nextMove)){
      if(p.equals(Human))
        TicTacToe(moveHistory + (nextMove -> p), Computer)
      else
        TicTacToe(moveHistory + (nextMove -> p), Human)
    }
    else
      TicTacToe(moveHistory)
  }

  lazy val nextGames: Set[TicTacToe] = ???
}

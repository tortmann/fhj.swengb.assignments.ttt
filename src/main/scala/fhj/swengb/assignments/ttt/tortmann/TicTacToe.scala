package fhj.swengb.assignments.ttt.tortmann

import scala.collection.Set

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

  def main(args: Array[String]) {

    val t = TicTacToe().turn(BottomRight, Human).turn(BottomCenter,Computer).turn(BottomLeft, Human).turn(MiddleCenter,Human).turn(MiddleRight,Computer)

    //test output
    print(t.asString())
    println("RemainingMoves: " + t.remainingMoves)
    println("Number of remaining moves: " + t.remainingMoves.size)

    //test remainingmoves
    println("Is the game over? " + t.gameOver)
    println("Winner is: " + t.winner.getOrElse(None))

  }
  def apply(): TicTacToe = TicTacToe(Map())

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


  def mkGames(): Map[Seq[TMove], TicTacToe] = {

    val allGames : Map[Seq[TMove], TicTacToe] = Map()
    return allGames
  }

}

case class TicTacToe(moveHistory: Map[TMove, Player], nextPlayer: Player = Human) {

  def asString(): String = {
    var GameBoard: String = "|---|---|---|\n" +
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
      else {
        GameBoard = GameBoard.updated(pos(m.idx), " ").mkString
      }
    }
    return GameBoard
  }

  val gameOver : Boolean = {
    if(winner == None)
      false
    else
      true
  }

  val remainingMoves: Set[TMove] = {
    val possibleMoves: Set[TMove] = Set(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
    for(specificMove <- possibleMoves
        if !moveHistory.contains(specificMove))
          yield specificMove
  }

  def winner: Option[(Player, Set[TMove])] = {
    val winningScenarios = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))
    val allMovesPlayerA: List[Int] = List()
    val allMovesPlayerB: List[Int] = List()

    for(move <- moveHistory){
      if(moveHistory.get(move._1).contains(Human))
        allMovesPlayerA.apply(move._1.idx)
      else
        allMovesPlayerB.apply(move._1.idx)
    }

    for(scenario <- winningScenarios){
      if(allMovesPlayerA.contains(scenario._1) && allMovesPlayerA.contains(scenario._2) && allMovesPlayerA.contains(scenario._3))
        Some(Human, moveHistory)
      else if(allMovesPlayerB.contains(scenario._1) && allMovesPlayerB.contains(scenario._2) && allMovesPlayerB.contains(scenario._3))
        Some(Computer, moveHistory)
    }
    None
  }

  def turn(nextMove: TMove, p: Player): TicTacToe = {
    if(moveHistory.contains(nextMove)){
      if(p.equals(Human))
        TicTacToe(moveHistory + (nextMove -> p), Computer)
      else
        TicTacToe(moveHistory + (nextMove -> p))
    }
    else
      TicTacToe(moveHistory)
  }
  /**
    * given a tic tac toe game, this function returns all
    * games which can be derived by making the next turn. that means one of the
    * possible turns is taken and added to the set.
    */
  lazy val nextGames: Set[TicTacToe] = ???
}

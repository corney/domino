package ru.corney.domino

import ru.corney.domino.Piece.Piece
import ru.corney.domino.Side.Side

import scala.util.Random

/**
  * Created by corney on 12.10.16.
  */
case class Tile(sideA: Piece, sideB: Piece) {
  require (sideA <= sideB)
  val double = sideA == sideB

  def piece(side: Side): Option[Piece] = {
    if (double) {
      Some(sideA)
    } else {
      side match {
        case Side.A =>
          Some(sideA)
        case Side.B =>
          Some(sideB)
        case Side.A_DOUBLE | Side.B_DOUBLE =>
          None
      }
    }
  }
}

object Tile {
  val all = for {
    a <- Piece.values
    b <- Piece.values if a <= b
  } yield Tile(a, b)

  def bazzar(n: Int): Seq[Tile] = {
    require(n > 2)
    require(n < 28)
    Random.shuffle(all.toSeq).take(n)
  }
}

object Piece extends Enumeration {
  type Piece = Value
  val ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX = Value
}

object Side extends Enumeration {
  type Side = Value
  val A, B, A_DOUBLE, B_DOUBLE = Value
}

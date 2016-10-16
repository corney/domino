package ru.corney.domino

import java.util.concurrent.atomic.AtomicInteger

import ru.corney.domino.Piece.Piece
import ru.corney.domino.Side.Side

import scala.util.Random

/**
  * Created by corney on 12.10.16.
  */
class Tile private (private val id: Int, sideA: Piece, sideB: Piece) {
  require(sideA <= sideB)
  val double = sideA == sideB

  def piece(side: Side): Piece = {
    side match {
      case Side.A | Side.A_DOUBLE =>
        sideA
      case Side.B | Side.B_DOUBLE =>
        sideB
    }
  }

  def side(piece: Piece): Option[Side] =
    if (piece == sideA)
      Some(Side.A)
    else if (piece == sideB)
      Some(Side.B)
    else
      None

  override def equals(obj: Any): Boolean =
    obj match {
      case that: Tile => this.id == that.id
      case _ => false
    }

  override def hashCode:Int = id.hashCode()

  override def toString: String = "Tile(%s, %s)".format(sideA, sideB)
}

object Tile {

  val id = new AtomicInteger(0)

  val all = for {
    a <- Piece.values
    b <- Piece.values if a <= b
  } yield Tile(a, b)

  def boneyard(n: Int): Seq[Tile] = {
    require(n > 2 && n < 28)
    Random.shuffle(all.toSeq).take(n)
  }

  def apply(sideA: Piece, sideB: Piece): Tile = new Tile(id.getAndIncrement(), sideA, sideB)
}

object Piece extends Enumeration {
  type Piece = Value
  val ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX = Value
}

object Side extends Enumeration {
  type Side = Value
  val A, B, A_DOUBLE, B_DOUBLE = Value
}

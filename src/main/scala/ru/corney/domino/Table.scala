package ru.corney.domino

import ru.corney.domino.Piece.Piece
import ru.corney.domino.Side.Side

case class Placed(tile: Tile, links: Map[Side, Option[Tile]]) {
  def link(link: Tile, side: Side): Placed =
    Placed(tile, links.updated(side, Some(link)))
}

object Placed {


  def apply(tile: Tile)(implicit doublesAllowed:Boolean): Placed =
    if (doublesAllowed && tile.double)
      Placed(tile, Map(Side.A -> None, Side.B -> None, Side.A_DOUBLE -> None, Side.B_DOUBLE -> None))
    else
      Placed(tile, Map(Side.A -> None, Side.B -> None))
}

class Table {

}

/**
  * Created by corney on 13.10.16.
  */
object Table {

  def canConnectAny(boneyard: Seq[Tile]): Boolean = {
    implicit val doublesAllowed = true
    val (table, status) = connect(boneyard)
    pretty(table)
    status
  }

  def canConnectInLine(boneyard: Seq[Tile]): Boolean = {
    implicit val doublesAllowed = false
    val (table, status) = connect(boneyard)
    pretty(table)
    status
  }

  def canConnectInCircle(boneyard: Seq[Tile]): Boolean = {
    // Красиво объединить в кольцо мы можем лишь набор с четным количеством костяшек
    if (boneyard.size %2 == 0) {
      implicit val doublesAllowed = false
      val (table, status) = connect(boneyard)
      if (status) {
        val rest = for {
          placed <- table.values
          (side, link) <- placed.links if link.isEmpty
        } yield placed.tile.piece(side)
        rest.size == 2 && rest.toSet.size == 1
      } else {
        false
      }
    } else {
      false
    }
  }

  protected def connect(boneyard: Seq[Tile])(implicit doublesAllowde: Boolean): (Map[Tile, Placed], Boolean) = {

    val tile :: tail = boneyard
    val placed = Placed(tile)
    val table = Map(tile -> placed)
    connect(table, tail)
  }

  protected def connect(table: Map[Tile, Placed], tiles: Seq[Tile])(implicit doublesAllowed:Boolean): (Map[Tile, Placed], Boolean) = {
    if (tiles.isEmpty) {
      (table, true)
    } else {
      val links = for {
        placed <- table.values
        (side, link) <- placed.links if link.isEmpty
        otherTile <- tiles
        otherSide <- otherTile.side(placed.tile.piece(side))
      } yield (placed.tile, side, otherTile, otherSide)

      if (links.isEmpty) {
        (table, false)
      } else {
        val result = links.map {
          case (tile1, side1, tile2, side2) =>
            val placed1 = table(tile1).link(tile2, side1)
            val placed2 = Placed(tile2).link(tile1, side2)
            val updated = table + (tile1 -> placed1, tile2 -> placed2)
            val rest = tiles.filterNot(tile => tile == tile2)
            if (rest.isEmpty) {
              (updated, true)
            } else {
              connect(updated, rest)
            }
        }.find{
          case (rTable, rStatus) => rStatus
        }
        result match {
          case Some((rTable, rStatus)) =>
            (rTable, rStatus)
          case None =>
            (table, false)
        }
      }
    }
  }

  def pretty(table: Map[Tile, Placed]) {
    for {
      (tile, placed) <- table
    } {
      println(tile)
      for {
        (side, link) <- placed.links if link.nonEmpty
      } {
        print("    ")
        print(side)
        print(": ")
        println(link.get)
      }
    }
    println()
  }
}

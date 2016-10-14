package ru.corney.domino

import ru.corney.domino.Piece.Piece
import ru.corney.domino.Side.Side

import scala.annotation.tailrec

case class Placed(tile: Tile, links: Map[Side, Option[Tile]]) {
  def link(link: Tile, side: Side): Placed =
    Placed(tile, links.updated(side, Some(link)))
}

object Placed {
  def apply(tile: Tile)(implicit doublesAllowed: Boolean): Placed =
    if (doublesAllowed && tile.double)
      Placed(tile, Map(Side.A -> None, Side.B -> None, Side.A_DOUBLE -> None, Side.B_DOUBLE -> None))
    else
      Placed(tile, Map(Side.A -> None, Side.B -> None))
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
    if (boneyard.size % 2 == 0) {
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
    connectRecursive(table, tail)
  }

  protected def connectRecursive(table: Map[Tile, Placed], tiles: Seq[Tile])(implicit doublesAllowed: Boolean): (Map[Tile, Placed], Boolean) = {

    (for {
      placed <- table.values
      (side, link) <- placed.links if link.isEmpty
      peerTile <- tiles
      peerSide <- peerTile.side(placed.tile.piece(side))
    } yield {
      val updated = table + (
        placed.tile -> table(placed.tile).link(peerTile, side),
        peerTile -> Placed(peerTile).link(placed.tile, peerSide)
        )
      val rest = tiles.filterNot(tile => tile == peerTile)
      if (rest.isEmpty) {
        (updated, true)
      } else {
        connectRecursive(updated, rest)
      }
    }).find {
      case (_, status) => status
    } match {
      case Some((rTable, rStatus)) =>
        (rTable, rStatus)
      case None =>
        (table, false)
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

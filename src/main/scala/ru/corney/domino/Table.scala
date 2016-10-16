package ru.corney.domino

import ru.corney.domino.Side.Side

import scala.collection.mutable


case class OnTable(tile: Tile, links: Map[Side, Option[Tile]]) {
  def link(link: Tile, side: Side): OnTable =
    OnTable(tile, links.updated(side, Some(link)))
}

object OnTable {
  def apply(tile: Tile)(implicit doublesAllowed: Boolean): OnTable =
    OnTable(tile, sides(tile).map(s => (s, None)).toMap)

  def sides(tile: Tile)(implicit doublesAllowed: Boolean): Set[Side] =
    if (doublesAllowed && tile.double)
      Set(Side.A, Side.B, Side.A_DOUBLE, Side.B_DOUBLE)
    else
      Set(Side.A, Side.B)
}

case class Edge(placed: OnTable, side: Side)

/**
  * Created by corney on 13.10.16.
  */
object Table {

  def canConnectAny(boneyard: Seq[Tile]): Boolean = {
    implicit val doublesAllowed = true
    val (table, status) = connect(boneyard)
    //    pretty(table)
    status
  }

  def canConnectInLine(boneyard: Seq[Tile]): Boolean = {
    implicit val doublesAllowed = false
    val (table, status) = connect(boneyard)
    //    pretty(table)
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

  object Action extends Enumeration {
    type Action = Value
    val Next, Rollback, Success, Failure = Value
  }

  sealed trait Status {
    val map: Map[Tile, OnTable]
    val rest: Seq[Tile]
    val links: Seq[(Tile, Side)]
  }

  case class TableStatus(map: Map[Tile, OnTable],
                         rest: Seq[Tile],
                         links: Seq[(Tile, Side)],
                         onTable: List[Edge]) extends Status

  case class TileStatus(map: Map[Tile, OnTable],
                        rest: Seq[Tile],
                        links: Seq[(Tile, Side)],
                        edge: Edge,
                        suitable: List[(Tile, Side)]
                       ) extends Status


  protected def connect(boneyard: Seq[Tile])(implicit doublesAllowed: Boolean): (Map[Tile, OnTable], Boolean) = {

    val head :: tail = boneyard

    val stack = mutable.Stack[Status]()
    val map = Map(head -> OnTable(head))

    stack.push(
      TableStatus(
        map,
        tail,
        for {
          tile <- boneyard
          side <- OnTable.sides(tile)
        } yield (tile, side),
        findAvailableEdges(map)
      )
    )

    var action = Action.Next

    while ((action != Action.Success) && (action != Action.Failure)) {
      action = action match {
        case Action.Next =>
          stack.head match {
            case status: TableStatus =>
              processTileOnTable(status)
            case status: TileStatus =>
              processBoneyardTile(status)
          }
        case Action.Rollback =>
          stack.pop()
          if (stack.isEmpty)
            Action.Failure
          else
            Action.Next
        case Action.Failure =>
          // Такого случиться не может
          Action.Failure
      }

      def processTileOnTable(status: TableStatus): Table.Action.Value = {
        if (status.onTable.isEmpty) {
          Action.Rollback
        } else {
          // Ищем очередную сторону, к которой можно приставить костяшку из базара
          val head :: tail = status.onTable

          stack.pop()
          stack.push(status.copy(onTable = tail))

          val tile = head.placed.tile
          val side: Side = head.side
          val piece = tile.piece(side)
          val suitable = status.links.filter {
            case (lTile, lSide) =>
              lTile != tile && lTile.piece(lSide) == piece
          }.toList
          if (suitable.nonEmpty) {
            stack.push(
              TileStatus(
                status.map,
                status.rest.filterNot(_ == tile),
                status.links.filterNot(_ ==(tile, side)),
                head,
                suitable)
            )
          }
          Action.Next
        }
      }

      def processBoneyardTile(status: TileStatus): Table.Action.Value = {
        if (status.suitable.nonEmpty) {
          val placed = status.edge.placed
          val side = status.edge.side
          val (oTile, oSide) :: tail = status.suitable
          val oPlaced = status.map.getOrElse(oTile, OnTable(oTile))


          stack.pop()
          stack.push(status.copy(suitable = tail))

          val updated = status.map + (placed.tile -> placed.link(oTile, side)) + (oTile -> oPlaced.link(placed.tile, oSide))

          val rest = status.rest.filterNot(_ == oTile)

          stack.push(
            TableStatus(
              updated,
              rest,
              status.links.filterNot(_ ==(oTile, oSide)),
              findAvailableEdges(updated)
            )
          )

          if (rest.isEmpty)
            Action.Success
          else
            Action.Next
        } else
          Action.Rollback
      }
    }

    if (action == Action.Success) {
      val status = stack.head
//      prettyPrint(status.map)
      (status.map, true)
    } else
      (Map.empty, false)
  }
  
  def findAvailableEdges(map: Map[Tile, OnTable]): List[Edge] = {
    (for {
      placed <- map.values
      link <- placed.links.keys if placed.links(link).isEmpty
    } yield Edge(placed, link)).toList
  }



  def prettyPrint(table: Map[Tile, OnTable]) {
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

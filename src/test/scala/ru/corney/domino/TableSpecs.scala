package ru.corney.domino

import org.specs2.mutable.Specification


/**
  * Created by corney on 12.10.16.
  */
class TableSpecs extends Specification {

  "Table" should {

    "Find correct way to connect tiles in any way" in {

      val tiles = Seq(
        Tile(Piece.ZERO, Piece.ZERO),
        Tile(Piece.ZERO, Piece.FIVE),
        Tile(Piece.FIVE, Piece.SIX),
        Tile(Piece.ZERO, Piece.THREE),
        Tile(Piece.ZERO, Piece.TWO),
        Tile(Piece.ZERO, Piece.FOUR),
        Tile(Piece.ZERO, Piece.SIX),
        Tile(Piece.FOUR, Piece.FIVE)
      )

      Table.canConnectAny(tiles) must_== true
      Table.canConnectInLine(tiles) must_== false
      Table.canConnectInCircle(tiles) must_== false
    }

    "Find correct way to connect tiles in line" in {

      val tiles = Seq(
        Tile(Piece.ZERO, Piece.ZERO),
        Tile(Piece.ZERO, Piece.FIVE),
        Tile(Piece.TWO, Piece.FOUR),
        Tile(Piece.ZERO, Piece.SIX),
        Tile(Piece.FOUR, Piece.FIVE)
      )

      Table.canConnectAny(tiles) must_== true
      Table.canConnectInLine(tiles) must_== true
      Table.canConnectInCircle(tiles) must_== false

    }

    "Find correct way to connect tiles in circle" in {

      val tiles = Seq(
        Tile(Piece.ZERO, Piece.ZERO),
        Tile(Piece.ZERO, Piece.FIVE),
        Tile(Piece.TWO, Piece.FOUR),
        Tile(Piece.ZERO, Piece.SIX),
        Tile(Piece.TWO, Piece.SIX),
        Tile(Piece.FOUR, Piece.FIVE)
      )
      Table.canConnectAny(tiles) must_== true
      Table.canConnectInLine(tiles) must_== true
      Table.canConnectInCircle(tiles) must_== true

    }

    "Decline to connect too much tiles to one double" in {

      val tiles = Seq(
        Tile(Piece.ZERO, Piece.ZERO),
        Tile(Piece.ZERO, Piece.ONE),
        Tile(Piece.ZERO, Piece.TWO),
        Tile(Piece.ZERO, Piece.THREE),
        Tile(Piece.ZERO, Piece.FOUR),
        Tile(Piece.ZERO, Piece.FIVE)
      )
      Table.canConnectAny(tiles) must_== false
      Table.canConnectInLine(tiles) must_== false
      Table.canConnectInCircle(tiles) must_== false

    }

    "Decline to connect unconnectable" in {

      val tiles = Seq(
        Tile(Piece.ZERO, Piece.ONE),
        Tile(Piece.ONE, Piece.TWO),
        Tile(Piece.TWO, Piece.THREE),
        Tile(Piece.FOUR, Piece.FIVE),
        Tile(Piece.FIVE, Piece.SIX),
        Tile(Piece.TWO, Piece.SIX)
      )
      Table.canConnectAny(tiles) must_== false
      Table.canConnectInLine(tiles) must_== false
      Table.canConnectInCircle(tiles) must_== false

    }


    "Duplicated tiles should be connected in right way" in {

      val tiles = Seq(
        Tile(Piece.ZERO, Piece.ZERO),
        Tile(Piece.ZERO, Piece.FIVE),
        Tile(Piece.TWO, Piece.FOUR),
        Tile(Piece.ZERO, Piece.SIX),
        Tile(Piece.TWO, Piece.SIX),
        Tile(Piece.FOUR, Piece.FIVE),
        Tile(Piece.ZERO, Piece.SIX)
      )
      Table.canConnectAny(tiles) must_== true
      Table.canConnectInLine(tiles) must_== true
      Table.canConnectInCircle(tiles) must_== false

    }

    "All tiles should be connected" in {
      Table.canConnectAny(Tile.all.toList) must_== true
      Table.canConnectInLine(Tile.all.toList) must_== true
      Table.canConnectInCircle(Tile.all.toList) must_== true
    }
  }

}

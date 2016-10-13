package ru.corney.domino

import org.specs2.mutable.Specification


/**
  * Created by corney on 12.10.16.
  */
class TileSpecs extends Specification {

  "Tiles" should {

    "Return set of all tiles and boneyard" in {
      Tile.all must have size 28
      Tile.boneyard(5) must have size 5


    }

    "Correct check if tile is double" in {
      Tile(Piece.ONE, Piece.ONE).double must_== true

      Tile(Piece.ZERO, Piece.ONE).double must_== false
    }

    "Correct find corresponding side and piece" in {
      val t45 = Tile(Piece.FOUR, Piece.FIVE)
      t45.piece(Side.A) must_== Piece.FOUR
      t45.piece(Side.B) must_== Piece.FIVE
      t45.piece(Side.A_DOUBLE) must_== Piece.FOUR
      t45.piece(Side.B_DOUBLE) must_== Piece.FIVE
      t45.side(Piece.SIX) must_== None
      t45.side(Piece.FOUR) must_== Some(Side.A)
      t45.side(Piece.FIVE) must_== Some(Side.B)
    }
  }
}

package ru.corney.domino

import org.specs2.mutable.Specification


/**
  * Created by corney on 12.10.16.
  */
class TileSpecs extends Specification {

  "Tiles" should {
    "Return set of all tiles" in {
      Tile.all must have size 28
    }

    "Correct check if tile is double" in {
      Tile(Piece.ONE, Piece.ONE).double must_== true

      Tile(Piece.ZERO, Piece.ONE).double must_== false
    }
  }
}

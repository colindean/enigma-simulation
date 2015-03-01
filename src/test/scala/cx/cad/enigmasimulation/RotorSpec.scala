package cx.cad.enigmasimulation

import org.scalatest._

class RotorSpec extends FunSpec with ShouldMatchers {

  describe("Rotor") {
    it("should rotate simply") {
      val initial = Map('A'->'Z')

      val rotor = new Rotor(initial)

      val nextRotation = rotor.rotate
      println(nextRotation.map)

      nextRotation.map shouldEqual Map('B' -> 'Z')
    }

    it("should wrap") {
      val initial = Map('Z' -> 'B')

      val rotor = new Rotor(initial)

      val nextRotation = rotor.rotate

      nextRotation.map shouldEqual Map('A' -> 'B')
    }
  }
}

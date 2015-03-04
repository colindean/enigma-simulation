package cx.cad.enigmasimulation

import org.scalatest._

class AtoZSpec extends FunSpec with ShouldMatchers  {
  describe("AtoZ") {
    it("should produce the next element") {
      Alphabet.nextLetter('A') shouldEqual 'B'
    }
    it("should loop from z to a") {
      Alphabet.nextLetter('Z') shouldEqual 'A'
    }
  }
}

package cx.cad.enigmasimulation

import org.scalatest._

class PlugboardSpec extends FunSpec with ShouldMatchers {

    describe("PlugBoard") {
      it("return the looked up value if the key doesn't exist") {
        val initial = Map('A'->'Z')

        val plugboard = new Plugboard(initial)

        plugboard.map('B') shouldEqual 'B'
      }
    }
}

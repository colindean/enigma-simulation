package cx.cad.enigmasimulation

import org.scalatest.{ShouldMatchers, FunSpec}

class EnigmaSpec extends FunSpec with ShouldMatchers {

  val text = "IHAVETAKENMOREOUTOFALCOHOLTHANALCOHOLHASTAKENOUTOFME"
  describe("Enigma") {
    it("decrypt what was encrypted") {
      val engima = new Enigma(Plugboard(), Reflector(), Seq(Rotor(), Rotor(), Rotor()))

      val crypted = engima.crypt(text)

      val decrypted = engima.crypt(crypted)

      decrypted should equal(text)
    }
  }
}


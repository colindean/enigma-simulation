package cx.cad.enigmasimulation

import org.scalatest.{ShouldMatchers, FunSpec}

class EnigmaSpec extends FunSpec with ShouldMatchers {

  val text = "IHAVETAKENMOREOUTOFALCOHOLTHANALCOHOLHASTAKENOUTOFME"
  describe("Enigma") {
    it("decrypt what was encrypted") {
      val enigma = new Enigma(Plugboard(), Reflector(), Seq(Rotor(), Rotor(), Rotor()))

      val crypted = enigma.crypt(text)

      val decrypted = enigma.crypt(crypted)

      decrypted should equal(text)
    }
    it("decrypt what was encrypted with four rotors and a long string") {
      val enigma = new Enigma(Plugboard(), Reflector(), Seq(Rotor(), Rotor(), Rotor(), Rotor()))

      val longerText = text * 25 * 25
      val crypted = enigma.crypt(longerText)

      val decrypted = enigma.crypt(crypted)

      decrypted should equal(longerText)
    }
  }
}


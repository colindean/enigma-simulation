package cx.cad.enigmasimulation

object Main {
    def main(args: Array[String]) {

      if(args.length != 1){
        println("You must supply something to encrypt.")
        System.exit(1)
      }

      val enigma = new Enigma(Plugboard.random, Reflector.random, Seq(Rotor.random, Rotor.random, Rotor.random))

      val originalInput = args(0)
      val cleanInput = originalInput.toUpperCase.replace(" ", "")

      if(!cleanInput.equals(originalInput)){
        println(
          s"""$originalInput
             |became
             |$cleanInput
           """.stripMargin)
      }

      println(enigma.crypt(cleanInput))

      print(enigma.toString)
    }
}

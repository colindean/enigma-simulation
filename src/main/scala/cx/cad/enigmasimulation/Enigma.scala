package cx.cad.enigmasimulation

import cx.cad.enigmasimulation.CharMapTypeContainer.CharMap

import scala.util.Random

class Enigma(plugboard: Plugboard, reflector: Reflector, var rotors: Seq[Rotor]) {

  override def toString = {
    s"""An Enigma with
       |Plugboard: $plugboard
       |Reflector: $reflector
       |Rotors:
       |    ${rotors.map(_.toString)}
     """.stripMargin
  }

  def crypt(text: String): String = {
    text.zipWithIndex.map( tuple => cryptChar(tuple._1, tuple._2)).mkString
  }

  private def cryptChar(char: Char, index: Int): Char = {
    rotateRotors(index)
    
    var cryptedChar = plugboard.map(char)

    cryptedChar = rotors.foldLeft(cryptedChar) { (c, rotor) => rotor.map(c) }

    cryptedChar = reflector.map(cryptedChar)

    cryptedChar = rotors.reverse.foldLeft(cryptedChar) { (c, rotor) => rotor.map(c) }

    cryptedChar = plugboard.map(char)

    cryptedChar
  }

  private def rotateRotors(index: Int) = {

    def rotateWhenAtAppropriateIndex(rotor: Rotor, index: Int): Rotor = {
      if (index % Math.pow(25, index) == 0) {
        rotor.rotate
      } else {
        rotor
      }
    }

    rotors = rotors.zipWithIndex.map( t => {
      val (rotor, index) = t
      rotateWhenAtAppropriateIndex(rotor, index)
    })
  }
}

object CharMapTypeContainer {
  type CharMap = Map[Char, Char]
}

trait MapUtilities {
  def listToMapByPairs(s: Seq[Char]): CharMap = s.grouped(2).map(s => s(0) -> s(1)).toMap
  def reverseMap[A](map: CharMap): CharMap = map.map(_.swap)
}

object AtoZ {
  val seq = ('A' to 'Z').toSeq
  def shuffled = Random.shuffle(seq)
}
case class LetterMap(mapping: CharMap = Map.empty) {
  def map(char: Char): Char = mapping(char)
}

class Plugboard(mapping: CharMap) extends LetterMap(mapping) {
  override def map(char: Char): Char = mapping.getOrElse(char, char)
}
class Reflector(mapping: CharMap) extends LetterMap(mapping)
class Rotor(mapping: CharMap) extends LetterMap(mapping) {
  def rotate: Rotor = {
    val newMap = mapping.map { case(k: Char, v: Char) =>
      val last = AtoZ.seq.last
      val newKey = k match {
        case x if x.equals(last) => AtoZ.seq.head
        case _ => AtoZ.seq(AtoZ.seq.indexOf(k)+1)
      }
      newKey -> v
    }

    new Rotor(newMap)
  }
}

object Plugboard extends MapUtilities {
  def apply() = {
    val initialMap = listToMapByPairs(AtoZ.shuffled.take(20))
    new Plugboard(initialMap ++ reverseMap(initialMap))
  }
}

object Reflector extends MapUtilities {
  def apply() = {
    val initialMap = listToMapByPairs(AtoZ.shuffled)
    new Reflector(initialMap ++ reverseMap(initialMap))
  }
}

object Rotor extends MapUtilities {
  def apply() = {
    new Rotor(AtoZ.seq.zip(AtoZ.shuffled).toMap)
  }
}
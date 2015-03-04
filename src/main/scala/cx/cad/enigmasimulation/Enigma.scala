package cx.cad.enigmasimulation

import cx.cad.enigmasimulation.Alphabet.Alphabet
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
    text.zipWithIndex.map{ case(letter, index) => cryptChar(letter, index) }.mkString
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

trait MappingUtilities {
  def seqToMapByPairs(s: Seq[Char]): CharMap = s.grouped(2).map(s => s(0) -> s(1)).toMap
  def reverseMap(map: CharMap): CharMap = map.map(_.swap)

  // TODO: rename this to something better
  def groupByTwoAndMergeReverse(alphabet: Alphabet): CharMap = {
    val initialMap = seqToMapByPairs(alphabet)
    initialMap ++ reverseMap(initialMap)
  }

  def shuffle(alphabet: Alphabet): Alphabet = Random.shuffle(alphabet)
}

object Alphabet {
  type Alphabet = Seq[Char]
  val English = ('A' to 'Z').toSeq
  val Default = English

  def nextLetter(letter: Char): Char = nextLetter(letter, Alphabet.Default)
  def nextLetter(letter: Char, alphabet: Alphabet): Char = {
    if(letter.equals(alphabet.last)){
      alphabet.head
    } else {
      alphabet(alphabet.indexOf(letter) + 1)
    }
  }

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
    // XXX: next letter here is going to always use default, and the rotor doesn't know its alphabet
    val newMap = mapping.map { case(k, v) => Alphabet.nextLetter(k) -> v}
    new Rotor(newMap)
  }
}

object Plugboard extends MappingUtilities {
  def random: Plugboard = random(Alphabet.Default)
  def random(alphabet: Alphabet): Plugboard = {
    val initialSeq = shuffle(alphabet).take(20)
    new Plugboard(groupByTwoAndMergeReverse(initialSeq))
  }
}

object Reflector extends MappingUtilities {
  def random: Reflector = random(Alphabet.Default)
  def random(alphabet: Alphabet): Reflector = {
    val initialSeq = shuffle(alphabet)
    new Reflector(groupByTwoAndMergeReverse(initialSeq))
  }
}

object Rotor extends MappingUtilities {
  def random: Rotor = random(Alphabet.Default)
  def random(alphabet: Alphabet): Rotor = new Rotor(alphabet.zip(shuffle(alphabet)).toMap)
}
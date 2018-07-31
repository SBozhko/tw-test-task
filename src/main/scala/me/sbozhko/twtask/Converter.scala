package me.sbozhko.twtask

object Converter {
  val RomanNumerals: Map[Char, Int] = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

  def romanNumeralsToDecimal(roman: String): Int = {

    if (roman.length == 0) {
      throw new IllegalArgumentException("Roman string should not be empty.")
    }

    var sum = 0
    var prevDigit = RomanNumerals(roman.last)

    roman.reverse.foreach { char =>
      val currentDigit = RomanNumerals(char)
      if (currentDigit < prevDigit) {
        sum -= currentDigit
      } else {
        sum += currentDigit
      }
      prevDigit = currentDigit
    }
    sum
  }

  def validateRomanNumeral(roman: String): Boolean = {
    roman.matches("^M{0,3}C{0,3}X{0,3}I{0,3}$")
  }


  def galaxyStringToRoman(galaxyStr: String, dictionary: Map[String, String]): String = {
    galaxyStr.split(" ").map(word => dictionary(word)).mkString("")
  }

  def galaxyStringToDecimal(galaxyStr: String, dictionary: Map[String, String]): Int = {
    val romanStr = galaxyStringToRoman(galaxyStr, dictionary)
    romanNumeralsToDecimal(romanStr)
  }

}

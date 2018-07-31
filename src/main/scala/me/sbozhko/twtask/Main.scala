package me.sbozhko.twtask

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    try {

      val fileName = args(0)

      val galaxyToRomanDictionary = collection.mutable.Map[String, String]()
      val galaxyToDecimalPrices = collection.mutable.Map[String, Double]()

      for (line <- Source.fromFile(fileName).getLines) {

        try {
          line.trim match {
            case dictStr if dictStr.matches("^\\w+ is [IVXLCDM]$") =>
              galaxyToRomanDictionary += Parser.parseGalaxyDictionaryEntry(dictStr)
            case priceStr if priceStr.matches(".* is \\d+ Credits$") =>
              galaxyToDecimalPrices += Parser.parseGalaxyPriceEntry(priceStr, galaxyToRomanDictionary.toMap)
            case questionStr if questionStr.startsWith("how") && questionStr.endsWith("?") =>
              println(Parser.parseAndProcessQuestion(questionStr, galaxyToRomanDictionary.toMap, galaxyToDecimalPrices.toMap))
            case _ => println("I have no idea what you are talking about")
          }

        } catch {
          case e: IllegalArgumentException => println(e.getMessage)
        }

      }
    } catch {
      case e: Exception =>
        println("Please provide a correct file name as an argument like /path/to/your/file/input.txt. " + e.getMessage)
        System.exit(1)
    }
  }

}

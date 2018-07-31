package me.sbozhko.twtask

object Parser {

  def parseAndProcessQuestion(questionStr: String,
                             galaxyToRomanDictionary: Map[String, String],
                             galaxyToDecimalPrices: Map[String, Double]): String = {
    try {
      questionStr.replace("how ", "").trim match {
        case howMuchStr if howMuchStr.startsWith("much is") =>
          val galaxyPrice = howMuchStr.replace("much is ", "").replace("?", "").trim
          val decimalPrice = Converter.galaxyStringToDecimal(galaxyPrice, galaxyToRomanDictionary)
          galaxyPrice + " is " + decimalPrice
        case howManyStr if howManyStr.startsWith("many Credits is") =>
          val galaxyNumberAndProduct = howManyStr.replace("many Credits is", "").replace("?", "").trim.split(" ")

          val galaxyNumberAndProductDivided = galaxyNumberAndProduct.partition(galaxyToRomanDictionary.contains)

          if (galaxyNumberAndProductDivided._1.length == 0 || galaxyNumberAndProductDivided._2.length == 0) {
            return "I have no idea what you are talking about"
          }

          val galaxyNumber = galaxyNumberAndProductDivided._1.mkString(" ")
          val galaxyProduct = galaxyNumberAndProductDivided._2.mkString(" ")

          val price = galaxyToDecimalPrices(galaxyProduct) * Converter.galaxyStringToDecimal(galaxyNumber, galaxyToRomanDictionary)
          galaxyNumberAndProduct.mkString(" ") + " is " + price.toInt + " Credits"
        case _ => "I have no idea what you are talking about"
      }
    } catch {
      case _: Exception => "I have no idea what you are talking about"
    }
  }

  def parseGalaxyPriceEntry(priceStr: String, galaxyToRomanDictionary: Map[String, String]): (String, Double) = {
    val galaxyAndDecimalParts = priceStr.split(" is ")
    val galaxyNumberAndProduct = galaxyAndDecimalParts(0).split(" ").map(_.trim)
    val galaxyNumberAndProductDivided = galaxyNumberAndProduct.partition(galaxyToRomanDictionary.contains)

    if (galaxyNumberAndProductDivided._1.length == 0 || galaxyNumberAndProductDivided._2.length == 0) {
      throw new IllegalArgumentException(priceStr + " is malformed. Should be like `glob prok Gold is 57800 Credits`.")
    }

    val romanNumberOfProduct = galaxyNumberAndProductDivided._1.map(word => galaxyToRomanDictionary(word)).mkString("")
    val decimalNumberOfProduct = Converter.romanNumeralsToDecimal(romanNumberOfProduct)

    val productName = galaxyNumberAndProductDivided._2.mkString(" ")
    val numberOfCredits = galaxyAndDecimalParts(1).replace("Credits", "").trim.toInt

    productName -> numberOfCredits.toDouble / decimalNumberOfProduct
  }

  def parseGalaxyDictionaryEntry(dictEntryString: String): (String, String) = {
    val keyAndValue = dictEntryString.split(" is ").map(_.trim)
    if (keyAndValue.length != 2) {
      throw new IllegalArgumentException(dictEntryString + " is malformed. Should be like `pish is X`.")
    } else {
      keyAndValue(0) -> keyAndValue(1)
    }
  }

}

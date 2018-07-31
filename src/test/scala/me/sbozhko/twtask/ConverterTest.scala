package me.sbozhko.twtask

import org.scalatest.FunSuite

class ConverterTest extends FunSuite {

  test("Convert Roman DXXI to Decimal") {
    assert(Converter.romanNumeralsToDecimal("DXXI") === 521)
  }

  test("Convert Roman XCIX to Decimal") {
    assert(Converter.romanNumeralsToDecimal("XCIX") === 99)
  }

  test("Validate Roman number MMMM") {
    assert(Converter.validateRomanNumeral("MMMM") === false)
  }

  test("Validate Roman number MMMCC") {
    assert(Converter.validateRomanNumeral("MMMCC") === true)
  }

  test("Validate Roman number MMMCCXI") {
    assert(Converter.validateRomanNumeral("MMMCCXI") === true)
  }

  test("Convert Roman MMMMCCXXXVI to Decimal") {
    assert(Converter.romanNumeralsToDecimal("MMMMCCXXXVI") === 4236)
  }

  test("Convert Roman I to Decimal") {
    assert(Converter.romanNumeralsToDecimal("I") === 1)
  }

  test("Convert an empty string to Decimal") {
    assertThrows[IllegalArgumentException](Converter.romanNumeralsToDecimal(""))
  }

  test("Convert Galaxy to Decimal") {
    assert(Converter.galaxyStringToDecimal("glob prok", Map("glob" -> "I", "prok" -> "V")) === 4)
  }

}


package me.sbozhko.twtask

import org.scalatest.FunSuite

class ParserTest extends FunSuite {

  test("Parse `glob iis I` with exception") {
    assertThrows[IllegalArgumentException](Parser.parseGalaxyDictionaryEntry("glob iis I"))
  }

  test("Parse `glob I` with exception") {
    assertThrows[IllegalArgumentException](Parser.parseGalaxyDictionaryEntry("glob I"))
  }

  test("Parse `glob is I`") {
    assert(Parser.parseGalaxyDictionaryEntry("glob is I") === "glob" -> "I")
  }

  test("Parse `glob prok Gold is 57800 Credits`") {
    assert(Parser.parseGalaxyPriceEntry("glob prok Gold is 57800 Credits", Map("glob" -> "I", "prok" -> "V")) === "Gold" -> 14450)
  }

  test("Parse `pish pish Iron is 3910 Credits`") {
    assert(Parser.parseGalaxyPriceEntry("pish pish Iron is 3910 Credits", Map("pish" -> "X")) === "Iron" -> 195.5)
  }

  test("Parse `pish pish Iron Blue is 3910 Credits`") {
    assert(Parser.parseGalaxyPriceEntry("pish pish Iron Blue is 3910 Credits", Map("pish" -> "X")) === "Iron Blue" -> 195.5)
  }

  test("Parse `pish pish is 3910 Credits` with exception") {
    assertThrows[IllegalArgumentException](Parser.parseGalaxyPriceEntry("pish pish is 3910 Credits", Map("pish" -> "X")))
  }

  test("Parse `how many Credits is glob prok Iron ?`") {
    assert(Parser.parseAndProcessQuestion(
      "how many Credits is glob prok Iron ?",
      Map("glob" -> "I", "prok" -> "V"),
      Map("Iron" -> 195.5)) === "glob prok Iron is 782 Credits")
  }

  test("Parse `how many Credits is glob prok Iron Foo ?`") {
    assert(Parser.parseAndProcessQuestion(
      "how many Credits is glob prok Iron Foo ?",
      Map("glob" -> "I", "prok" -> "V"),
      Map("Iron Foo" -> 195.5)) === "glob prok Iron Foo is 782 Credits")
  }

  test("Parse `how many Credits is glob prok Silver ?`") {
    assert(Parser.parseAndProcessQuestion(
      "how many Credits is glob prok Silver ?",
      Map("glob" -> "I", "prok" -> "V"),
      Map("Silver" -> 17)) === "glob prok Silver is 68 Credits")
  }

  test("Parse `how many Credits is glob prok Silver ?` without prok") {
    assert(Parser.parseAndProcessQuestion(
      "how many Credits is glob prok Silver ?",
      Map("glob" -> "I"),
      Map("Silver" -> 17)) === "I have no idea what you are talking about")
  }

  test("Parse `how much wood could a woodchuck chuck if a woodchuck could chuck wood ?`") {
    assert(Parser.parseAndProcessQuestion(
      "how much wood could a woodchuck chuck if a woodchuck could chuck wood ?",
      Map("glob" -> "I", "prok" -> "V"),
      Map("Silver" -> 17)) === "I have no idea what you are talking about")
  }

  test("Parse `qwert werty`") {
    assert(Parser.parseAndProcessQuestion(
      "qwert werty",
      Map("glob" -> "I", "prok" -> "V"),
      Map("Silver" -> 17)) === "I have no idea what you are talking about")
  }



}


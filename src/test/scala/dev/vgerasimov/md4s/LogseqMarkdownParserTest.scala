package dev.vgerasimov.md4s

import dev.vgerasimov.slowparse.{ P, POut }
import dev.vgerasimov.md4s.models.*
import dev.vgerasimov.md4s.models.elements.*
import dev.vgerasimov.md4s.models.objects.*
import dev.vgerasimov.md4s.models.objects.TextMarkup.*

class LogseqMarkdownParserTest extends munit.ScalaCheckSuite {

  test("Full parser should parse some valid markup string") {
    val toParse = """*hello world*"""
    checkParser(
      parser.document,
      toParse,
      Document(elements = List(Paragraph(objects = List(bold("", "hello world")))), sections = List())
    )
  }

  test("Full parser should parse some valid heading") {
    val toParse = """# hello world"""
    checkParser(
      parser.document,
      toParse,
      Document(elements = List(Paragraph(objects = List(bold("", "hello world")))), sections = List())
    )
  }

  lazy val ctx: LogseqMarkdownContext = LogseqMarkdownContext.defaultCtx
  lazy val parser: LogseqMarkdownParser = new LogseqMarkdownParser(ctx)

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[?] => true
      case _: POut.Failure    => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](parser: P[T], toParse: String, expected: => T): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) =>
        assertEquals(value, expected)
      case _: POut.Failure => fail(s"$toParse not parsed")
    }

  def checkParserFailed[T](parser: P[T], toParse: String): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) => fail(s"$toParse parsed to $value")
      case _                            =>
    }
}

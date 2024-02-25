package dev.vgerasimov.md4s.logseq

import dev.vgerasimov.slowparse.{ P, POut }

import models.*
import ops.{ *, given }
import parser.*

class LogseqMarkdownParserTest extends munit.ScalaCheckSuite {

  test("Full parser should parse some valid markup string") {
    val toParse = """*hello world*"""
    checkParser(
      parser.document,
      toParse,
      MarkdownAST(
        List(
          Paragraph(
            InlineContainer(
              List(
                Emphasis(
                  Emphasis.Marker.Italic("*"),
                  InlineContainer(List(Text("hello world")))
                )
              )
            )
          )
        )
      )
    )
  }

  test("Full parser should parse some one-way nested headings - simple") {
    val toParse = """# Heading 1
                   |## Heading 2
                   |### Heading 3
                   |""".stripMargin
    checkParser(
      parser.document,
      toParse,
      MarkdownAST(
        List(
          HeadedSection(
            Heading(Some(InlineContainer(List(Text("Heading 1")))), 1),
            List(
              HeadedSection(
                Heading(Some(InlineContainer(List(Text("Heading 2")))), 2),
                List(
                  HeadedSection(
                    Heading(Some(InlineContainer(List(Text("Heading 3")))), 3),
                    List()
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("Full parser should parse some one-way nested headings - two on level 2") {
    val toParse = """# Heading 1
                   |## Heading 2.1
                   |## Heading 2.2
                   |""".stripMargin
    checkParser(
      parser.document,
      toParse,
      MarkdownAST(
        List(
          HeadedSection(
            Heading(Some(InlineContainer(List(Text("Heading 1")))), 1),
            List(
              HeadedSection(
                Heading(Some(InlineContainer(List(Text("Heading 2.1")))), 2),
                List()
              ),
              HeadedSection(
                Heading(Some(InlineContainer(List(Text("Heading 2.2")))), 2),
                List()
              )
            )
          )
        )
      )
    )
  }

  test("Full parser should parse some nested headings") {
    val toParse = """# Heading 1
                   |## Heading 2
                   |## Heading 2.2
                   |### Heading 3
                   |#### Heading 4
                   |### Heading 3.1
                   |##### Heading 5
                   |# Heading 1.1
                   |###### Heading 6
                   |""".stripMargin
    checkParser(
      parser.document,
      toParse,
      MarkdownAST(
        List(
          HeadedSection(
            Heading(Some(InlineContainer(List(Text("Heading 1")))), 1),
            List(
              HeadedSection(Heading(Some(InlineContainer(List(Text("Heading 2")))), 2), List()),
              HeadedSection(
                Heading(Some(InlineContainer(List(Text("Heading 2.2")))), 2),
                List(
                  HeadedSection(
                    Heading(Some(InlineContainer(List(Text("Heading 3")))), 3),
                    List(
                      HeadedSection(
                        Heading(Some(InlineContainer(List(Text("Heading 4")))), 4),
                        List()
                      )
                    )
                  ),
                  HeadedSection(
                    Heading(Some(InlineContainer(List(Text("Heading 3.1")))), 3),
                    List(
                      HeadedSection(
                        Heading(Some(InlineContainer(List(Text("Heading 5")))), 5),
                        List()
                      )
                    )
                  )
                )
              )
            )
          ),
          HeadedSection(
            Heading(Some(InlineContainer(List(Text("Heading 1.1")))), 1),
            List(
              HeadedSection(
                Heading(Some(InlineContainer(List(Text("Heading 6")))), 6),
                List()
              )
            )
          )
        )
      )
    )
  }

  lazy val ctx = Context.defaultCtx
  lazy val parser = new parser(ctx)

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[?] => true
      case _: POut.Failure    => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](parser: P[T], toParse: String, expected: => T): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) =>
        pprint.pprintln(value)
        assertEquals(value, expected)
      case _: POut.Failure => fail(s"$toParse not parsed")
    }

  def checkParserFailed[T](parser: P[T], toParse: String): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) => fail(s"$toParse parsed to $value")
      case _                            =>
    }

  def ignore(v: String)(body: => Any): Unit = (
    ()
  )
}

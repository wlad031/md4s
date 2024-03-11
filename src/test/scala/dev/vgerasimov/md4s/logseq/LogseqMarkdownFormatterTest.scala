package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

class LogseqMarkdownFormatterTest extends munit.ScalaCheckSuite:

  test("Text is formatted as text") {
    val toFormat = Text("Hello, world!")
    val expected = "Hello, world!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Bold text is formatted as bold text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Bold("**"), InlineContainer(List(Text("Hello, world!"))))
    val expected = "**Hello, world!**"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Italic text is formatted as italic text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Italic("*"), InlineContainer(List(Text("Hello, world!"))))
    val expected = "*Hello, world!*"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Code text is formatted as code text") {
    val toFormat = Emphasis(Emphasis.Marker.Code("`"), InlineContainer(List(Text("Hello, world!"))))
    val expected = "`Hello, world!`"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("StrikeThrough text is formatted as strikethrough text") {
    val toFormat =
      Emphasis(Emphasis.Marker.StrikeThrough("~~"), InlineContainer(List(Text("Hello, world!"))))
    val expected = "~~Hello, world!~~"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Highlight text is formatted as highlighted text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Highlight("=="), InlineContainer(List(Text("Hello, world!"))))
    val expected = "==Hello, world!=="
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Mixed emphasis is formatted correctly 1") {
    val toFormat = InlineContainer(
      List(
        Text("Hello, "),
        Emphasis(Emphasis.Marker.Bold("**"), InlineContainer(List(Text("world")))),
        Text("!")
      )
    )
    val expected = "Hello, **world**!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Mixed emphasis is formatted correctly 2") {
    val toFormat = InlineContainer(
      List(
        Emphasis(Emphasis.Marker.Bold("**"), InlineContainer(List(Text("Hello")))),
        Text(", "),
        Emphasis(Emphasis.Marker.Italic("*"), InlineContainer(List(Text("world")))),
        Text("!")
      )
    )
    val expected = "**Hello**, *world*!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple text paragraph is formatted as correctly") {
    val toFormat = Paragraph(InlineContainer(List(Text("Hello, world!"))))
    val expected = "Hello, world!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple text paragraph with indentation is formatted as correctly") {
    val toFormat =
      Paragraph(InlineContainer(List(Text("Hello, world!"))), indentation = Indentation(1, "  "))
    val expected = "  Hello, world!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple text paragraph with properties is formatted as correctly") {
    val toFormat =
      Paragraph(
        InlineContainer(List(Text("Hello, world!"))),
        propertyDrawer = Some(
          PropertyDrawer(
            List(PropertyDrawer.Node("PROPERTY", Some(InlineContainer(List(Text("VALUE"))))))
          )
        )
      )
    val expected = "Hello, world!\nPROPERTY:: VALUE"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple text paragraph with multiple properties and indentation is formatted as correctly") {
    val toFormat =
      Paragraph(
        InlineContainer(List(Text("Hello, world!"))),
        indentation = Indentation(2, "\t\t"),
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              PropertyDrawer.Node("PROPERTY", Some(InlineContainer(List(Text("VALUE"))))),
              PropertyDrawer.Node(
                "ANOTHER_PROPERTY",
                Some(InlineContainer(List(Text("ANOTHER_VALUE"))))
              )
            )
          )
        )
      )
    val expected = "\t\tHello, world!\nPROPERTY:: VALUE\nANOTHER_PROPERTY:: ANOTHER_VALUE"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple headed section with single paragraph is formatted as correctly") {
    val toFormat = HeadedSection(
      Heading(Some(InlineContainer(List(Text("Hello, world!")))), 1),
      List(Paragraph(InlineContainer(List(Text("Hello, world!")))))
    )
    val expected = "# Hello, world!\nHello, world!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple headed section with multiple paragraphs is formatted as correctly") {
    val toFormat = HeadedSection(
      Heading(Some(InlineContainer(List(Text("Hello, world!")))), 1),
      List(
        Paragraph(InlineContainer(List(Text("Hello, world!")))),
        Paragraph(InlineContainer(List(Text("Hello, world!"))))
      )
    )
    val expected = "# Hello, world!\nHello, world!\nHello, world!"
    assertEquals(formatter.format(toFormat), expected)
  }

  test("Simple headed section with indentation is formatted as correctly") {
    val toFormat = HeadedSection(
      Heading(
        Some(InlineContainer(List(Text("Hello, world!")))),
        1,
        indentation = Indentation(1, "  ")
      ),
      List(
        Paragraph(InlineContainer(List(Text("Hello, world!"))), indentation = Indentation(1, "  "))
      )
    )
    val expected = "  # Hello, world!\n  Hello, world!"
    assertEquals(formatter.format(toFormat), expected)
  }

  import dev.vgerasimov.md4s.logseq.parser.*
  import dev.vgerasimov.slowparse.{ P, POut }

  lazy val ctx = Context.defaultCtx
  lazy val parser = new parser(ctx)

  test("Text -> Parsing -> Formatting [1]") {
    val toParse = """
|  type:: [[Media/Movie]]
|  alias:: Thor: Ragnarok
|  status:: [[DONE]]
|  rating:: 3
|  done-date:: [[2017-11-09]],[[2024-03-08]]
|-
|- # Cast
""".trim().stripMargin
    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

  test("Text -> Parsing -> Formatting [2]") {
    val toParse = """
|  type:: [[Media/Movie]]
|  author:: [[Кристофер Нолан]]
|  status:: [[DONE]]
|  alias:: Oppenheimer
|  rating:: 5
|  done-date:: [[2023-07-29]]
|- DONE [[Oppenheimer]] in [[Cinema City]]
|  SCHEDULED: <2023-07-29 Sat 19:00>  
|- # Cast
|	- [[Киллиан Мерфи]]
|	- [[Мэтт Дэймон]]
|	- [[Роберт Дауни мл.]]
|	- [[Эмили Блант]]
|	- [[Рами Малек]]
|	- [[Флоренс Пью]]
|	- [[Гари Олдман]]
|	- others
""".trim().stripMargin
    parser.document(toParse) match
      case POut.Success(toFormat, _, _, _) =>
        val formatted = formatter.format(toFormat)
        assertEquals(formatted, toParse)
      case POut.Failure(message, _) => fail(s"$toParse not parsed: $message")
  }

end LogseqMarkdownFormatterTest

package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

class DocumentFormatterTest extends munit.ScalaCheckSuite {

  test("Text is formatted as text") {
    val toFormat = Text("Hello, world!")
    val expected = "Hello, world!"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Bold text is formatted as bold text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Bold("**"), List(Text("Hello, world!")))
    val expected = "**Hello, world!**"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Italic text is formatted as italic text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Italic("*"), List(Text("Hello, world!")))
    val expected = "*Hello, world!*"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Code text is formatted as code text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Code("`"), List(Text("Hello, world!")))
    val expected = "`Hello, world!`"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("StrikeThrough text is formatted as strikethrough text") {
    val toFormat =
      Emphasis(Emphasis.Marker.StrikeThrough("~~"), List(Text("Hello, world!")))
    val expected = "~~Hello, world!~~"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Highlight text is formatted as highlighted text") {
    val toFormat =
      Emphasis(Emphasis.Marker.Highlight("=="), List(Text("Hello, world!")))
    val expected = "==Hello, world!=="
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Mixed emphasis is formatted correctly 1") {
    val toFormat =
      List(
        Text("Hello, "),
        Emphasis(Emphasis.Marker.Bold("**"), List(Text("world"))),
        Text("!")
      )
    val expected = "Hello, **world**!"
    assertEquals(Formatter.formatElements(toFormat), expected)
  }

  test("Mixed emphasis is formatted correctly 2") {
    val toFormat =
      List(
        Emphasis(Emphasis.Marker.Bold("**"), List(Text("Hello"))),
        Text(", "),
        Emphasis(Emphasis.Marker.Italic("*"), List(Text("world"))),
        Text("!")
      )
    val expected = "**Hello**, *world*!"
    assertEquals(Formatter.formatElements(toFormat), expected)
  }

  test("Simple text paragraph is formatted as correctly") {
    val toFormat = Paragraph(List(Text("Hello, world!")))
    val expected = "Hello, world!"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Simple text paragraph with indentation is formatted as correctly") {
    val toFormat =
      Paragraph(
        List(Text("Hello, world!")),
        indentation = Some(Indentation(1, "  "))
      )
    val expected = "  Hello, world!"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Simple text paragraph with properties is formatted as correctly") {
    import PropertyDrawer.*
    import Node.*
    val toFormat =
      Paragraph(
        List(Text("Hello, world!")),
        propertyDrawer = Some(
          PropertyDrawer(
            List(Node(Key("PROPERTY"), Value(List(Text("VALUE")))))
          )
        )
      )
    val expected = "Hello, world!\nPROPERTY:: VALUE"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Simple text paragraph with multiple properties and indentation is formatted as correctly") {
    import PropertyDrawer.*
    import Node.*
    val toFormat =
      Paragraph(
        List(Text("Hello, world!")),
        indentation = Some(Indentation(2, "\t\t")),
        propertyDrawer = Some(
          PropertyDrawer(
            List(
              Node(Key("PROPERTY"), Value(List(Text("VALUE")))),
              Node(
                Key("ANOTHER_PROPERTY"),
                Value(List(Text("ANOTHER_VALUE")))
              )
            )
          )
        )
      )
    val expected = "\t\tHello, world!\nPROPERTY:: VALUE\nANOTHER_PROPERTY:: ANOTHER_VALUE"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Simple headed section with single paragraph is formatted as correctly") {
    val toFormat = HeadedSection(
      Heading(
        level = Heading.Level(value = 1),
        elements = List(Text("Hello, world!"))
      ),
      List(Paragraph(List(Text("Hello, world!"))))
    )
    val expected = "# Hello, world!\nHello, world!"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Simple headed section with multiple paragraphs is formatted as correctly") {
    val toFormat = HeadedSection(
      Heading(
        level = Heading.Level(value = 1),
        elements = List(Text("Hello, world!"))
      ),
      List(
        Paragraph(List(Text("Hello, world!"))),
        Paragraph(List(Text("Hello, world!")))
      )
    )
    val expected = "# Hello, world!\nHello, world!\nHello, world!"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Simple headed section with indentation is formatted as correctly") {
    val toFormat = HeadedSection(
      Heading(
        level = Heading.Level(value = 1),
        elements = List(Text("Hello, world!"))
      ),
      List(
        Paragraph(
          List(Text("Hello, world!")),
          indentation = Some(Indentation(1, "  "))
        )
      ),
      indentation = Some(Indentation(1, "  "))
    )
    val expected = "  # Hello, world!\n  Hello, world!"
    assertEquals(Formatter.format(toFormat), expected)
  }

  test("Table with different elements is formatted correctly") {
    import dev.vgerasimov.md4s.logseq.models.Table.*
    import dev.vgerasimov.md4s.logseq.models.Table.Row.*
    val toFormat = Document(blocks =
      List(
        Table(rows =
          List(
            Cells(
              List(
                Cell(List(Text("header"))),
                Cell(List(Text("header 1")))
              )
            ),
            Cells(
              List(
                Cell(
                  List(
                    Emphasis(Emphasis.Marker.Bold("**"), List(Text("row 1")))
                  )
                ),
                Cell(
                  List(
                    Emphasis(
                      Emphasis.Marker.Italic("_"),
                      List(Text("row 1"))
                    )
                  )
                )
              )
            ),
            Cells(
              List(
                Cell(
                  List(Link.Internal.Tag.WithoutBrackets(Link.Location.Internal.Page("tag")))
                ),
                Cell(
                  List(
                    Link.External(Link.Location.External("http://example.com"), Some("link"))
                  )
                )
              )
            )
          )
        )
      )
    )
    val expected = """|header|header 1|
|**row 1**|_row 1_|
|#tag|[link](http://example.com)|"""
    assertEquals(Formatter.format(toFormat), expected)
  }

}

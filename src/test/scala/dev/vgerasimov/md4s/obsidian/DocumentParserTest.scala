package dev.vgerasimov.md4s
package obsidian

import dev.vgerasimov.slowparse.{ P, POut }

import models.*
import ops.*
import Parser.*

class DocumentParserTest extends munit.ScalaCheckSuite:

  test("frontmatter + paragraph") {
    val toParse = """---
title: Test note
tags: one, two
---
hello"""

    checkParser(
      toParse,
      Document(
        frontmatter = Some(
          Frontmatter(
            raw = "title: Test note\ntags: one, two",
            values = Map("title" -> "Test note", "tags" -> "one, two")
          )
        ),
        blocks = List(Paragraph(List(Text("hello"))))
      )
    )
  }

  test("heading is parseable") {
    val toParse = "## heading"
    checkParser(
      toParse,
      Document(blocks = List(Heading(level = 2, elements = List(Text("heading")))))
    )
  }

  test("wiki links and embeds are parseable") {
    val toParse = "[[Page]] [[Page#Section|Label]] ![[assets/image.png]] [[#Heading]] [[Page#^block-id]]"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(
            List(
              wiki(path = Some("Page")),
              Text(" "),
              wiki(path = Some("Page"), heading = Some("Section"), alias = Some("Label")),
              Text(" "),
              wiki(path = Some("assets/image.png"), embed = true),
              Text(" "),
              wiki(heading = Some("Heading")),
              Text(" "),
              wiki(path = Some("Page"), blockId = Some("block-id"))
            )
          )
        )
      )
    )
  }

  test("markdown links and image are parseable") {
    val toParse = "[site](https://example.com) ![logo](https://cdn/logo.png \"brand\")"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(
            List(
              Link.External("https://example.com", label = Some("site")),
              Text(" "),
              Image("logo", "https://cdn/logo.png", title = Some("brand"))
            )
          )
        )
      )
    )
  }

  test("task list is parseable") {
    val toParse = """- [ ] todo
- [x] done"""
    checkParser(
      toParse,
      Document(
        blocks = List(
          MarkdownList.Unordered(
            items = List(
              MarkdownList.Item(
                blocks = List(Paragraph(List(Text("todo")))),
                marker = MarkdownList.Item.Marker("-"),
                checkbox = Some(MarkdownList.Item.Checkbox(checked = false))
              ),
              MarkdownList.Item(
                blocks = List(Paragraph(List(Text("done")))),
                marker = MarkdownList.Item.Marker("-"),
                checkbox = Some(MarkdownList.Item.Checkbox(checked = true))
              )
            )
          )
        )
      )
    )
  }

  test("callout is parseable") {
    val toParse = """> [!note]- Optional title
> line 1
> line 2"""
    checkParser(
      toParse,
      Document(
        blocks = List(
          Callout(
            calloutType = "note",
            title = List(Text("Optional title")),
            foldable = Some(false),
            blocks = List(
              Paragraph(List(Text("line 1"))),
              Paragraph(List(Text("line 2")))
            )
          )
        )
      )
    )
  }

  test("footnote reference and definition are parseable") {
    val toParse = "Footnote here[^id]\n[^id]: explanation"
    checkParser(
      toParse,
      Document(
        blocks = List(
          Paragraph(List(Text("Footnote here"), FootnoteReference("id"))),
          FootnoteDefinition(id = "id", elements = List(Text("explanation")))
        )
      )
    )
  }

  test("comment block is parseable") {
    val toParse = """%%
hidden
comment
%%"""
    checkParser(
      toParse,
      Document(blocks = List(CommentBlock("hidden\ncomment")))
    )
  }

  test("table is parseable") {
    import Table.*
    import Table.Row.*

    val toParse = """|h1|h2|
|--|--|
|v1|v2|"""

    checkParser(
      toParse,
      Document(
        blocks = List(
          Table(
            rows = List(
              Cells(List(Cell(List(Text("h1"))), Cell(List(Text("h2"))))),
              Separator("|--|--|"),
              Cells(List(Cell(List(Text("v1"))), Cell(List(Text("v2")))))
            )
          )
        )
      )
    )
  }

  test("math stays plain text") {
    val toParse = "price is $x + y$"
    checkParser(
      toParse,
      Document(blocks = List(Paragraph(List(Text("price is $x + y$")))))
    )
  }

  lazy val parser = new Parser(Context.default())

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[?] => true
      case _: POut.Failure    => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](
    toParse: String,
    expected: => Document,
    parser: P[Document] = parser.document
  ): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) => assertEquals(value, expected)
      case POut.Failure(message, _)     => fail(s"$toParse not parsed: $message")
    }

end DocumentParserTest

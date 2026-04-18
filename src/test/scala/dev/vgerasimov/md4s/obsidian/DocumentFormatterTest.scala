package dev.vgerasimov.md4s
package obsidian

import models.*
import ops.*

class DocumentFormatterTest extends munit.ScalaCheckSuite {

  private val underTest: FormatterImpl.type = FormatterImpl

  test("wiki and markdown links are formatted") {
    val toFormat = Paragraph(
      List(
        wiki(path = Some("Page"), alias = Some("Alias")),
        Text(" "),
        Link.External("https://example.com", label = Some("site")),
        Text(" "),
        Image("logo", "https://cdn/logo.png", title = Some("brand"))
      )
    )

    val expected = "[[Page|Alias]] [site](https://example.com) ![logo](https://cdn/logo.png \"brand\")"
    assertEquals(underTest.format(toFormat), expected)
  }

  test("callout is formatted") {
    val toFormat = Callout(
      calloutType = "warning",
      foldable = Some(true),
      title = List(Text("Be careful")),
      blocks = List(Paragraph(List(Text("line"))))
    )

    val expected = "> [!warning]+ Be careful\n> line"
    assertEquals(underTest.format(toFormat), expected)
  }

  test("task list is formatted") {
    val toFormat = MarkdownList.Unordered(
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

    val expected = "- [ ] todo\n- [x] done"
    assertEquals(underTest.format(toFormat), expected)
  }

  test("frontmatter + blocks are formatted") {
    val toFormat = Document(
      frontmatter = Some(
        Frontmatter(
          raw = "title: Test note\ntags: one, two",
          values = Map("title" -> "Test note", "tags" -> "one, two")
        )
      ),
      blocks = List(h("Heading", 2), Paragraph(List(Text("text"))))
    )

    val expected = """---
title: Test note
tags: one, two
---
## Heading
text"""
    assertEquals(underTest.format(toFormat), expected)
  }

  test("math stays unchanged") {
    val toFormat = Paragraph(List(Text("price is $x + y$")))
    assertEquals(underTest.format(toFormat), "price is $x + y$")
  }

}

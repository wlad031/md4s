package dev.vgerasimov.md4s
package interop

class MemoryInteropApiTest extends munit.FunSuite {

  test("detects wikilink or tag") {
    assert(ObsidianMarkdownOps.hasLinkOrTag("Hello [[Page]]"))
    assert(ObsidianMarkdownOps.hasLinkOrTag("Hello #tag"))
    assert(!ObsidianMarkdownOps.hasLinkOrTag("Hello world"))
  }

  test("reads frontmatter value") {
    val input = """---
owner: demo
---
Body
"""
    assertEquals(ObsidianMarkdownOps.frontmatterValue(input, "owner"), Some("demo"))
  }

  test("appends links and tags as formatted paragraph") {
    val updated = ObsidianMarkdownOps.appendLinksAndTags(
      "Start",
      List("[[Page One|Alias]]"),
      List("memory tag")
    )
    assert(updated.contains("[[Page One]]"))
    assert(updated.contains("#memory-tag"))
  }
}

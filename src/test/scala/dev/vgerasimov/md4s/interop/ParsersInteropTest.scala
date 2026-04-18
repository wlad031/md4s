package dev.vgerasimov.md4s
package interop

class ParsersInteropTest extends munit.ScalaCheckSuite {

  private val underTest = ParsersInterop.defaultInstance()

  test("parse Obsidian from Java-friendly API") {
    val result = underTest.parseObsidian("## heading")
    assert(result.isSuccess)
    assert(result.getDocument.isPresent)
  }

  test("parse Logseq and format from interop API") {
    val input = "# heading\ntext"
    val doc = underTest.parseLogseqOrThrow(input)
    val formatted = underTest.formatLogseq(doc)
    assertEquals(formatted, input)
  }

  test("parse Obsidian as plain Java data map") {
    val input = """---
title: Demo
---
## heading
[[Page|Alias]] #tag"""

    val result = underTest.parseObsidianAsData(input)
    assert(result.isSuccess)

    val document = result.getDocument.orElseThrow()
    assertEquals(document.get("_type"), "Document")

    val blocks = document.get("blocks").asInstanceOf[java.util.List[?]]
    val first = blocks.get(0).asInstanceOf[java.util.Map[String, AnyRef]]
    assertEquals(first.get("_type"), "Heading")
  }

  test("java static interop class is available") {
    val result = Parsers.parseLogseq("# heading")
    assert(result.isSuccess)
    assert(result.getDocument.isPresent)
  }

}

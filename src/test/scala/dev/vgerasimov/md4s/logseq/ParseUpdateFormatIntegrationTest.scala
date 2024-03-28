package dev.vgerasimov.md4s
package logseq

import logseq.models.*
import logseq.ops.{ *, given }
import logseq.ops.blocks.{ *, given }
import logseq.ops.properties.{ *, given }

class ParseUpdateFormatIntegrationTest extends munit.ScalaCheckSuite {

  import dev.vgerasimov.md4s.logseq.Parser.*
  import dev.vgerasimov.md4s.logseq.Formatter.*
  import dev.vgerasimov.slowparse.{ P, POut }

  lazy val ctx = Context.default()
  lazy val parser = new Parser(ctx)
  lazy val formatter = Formatter
  lazy val updater = Updater()

  test("add property to heading") {
    val toParse = """# heading
foo:: bar"""
    parser.document(toParse) match
      case POut.Success(document @ Document(_, ls), _, _, _) =>
        val b = ls.head
        val newBlock = b.appendPropertyNode(node("baz", "qux"))
        val newDocument = updater.apply(document, List(Updater.Input(b, newBlock)))
        val actual = formatter.format(newDocument)
        assertEquals(actual, """# heading
foo:: bar
baz:: qux""")
      case POut.Failure(_, _) => fail("Failed to parse")
  }

}